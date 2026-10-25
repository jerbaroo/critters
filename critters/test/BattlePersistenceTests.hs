module BattlePersistenceTests (tests) where

import BattleTests (fixture)
import Control.Exception (bracket)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Expression (Expression (..))
import Critters.Input qualified as Input
import Critters.Save qualified as Save
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition qualified as BattleDefinition
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.CombatConfig qualified as CombatConfig
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..))
import Critters.State.GameState qualified as GameState
import Critters.State.Item (defaultItem)
import Critters.State.Item qualified as Item
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Species (BaseStats (..), defaultSpecies)
import Critters.State.Species qualified as Species
import Data.Map.Strict qualified as Map
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

entry :: GameState -> [Event]
entry state = either (const []) id (Engine.eventsForEffects state [StartBattle "practice"])

tests :: TestTree
tests =
    testGroup
        "Battle persistence"
        [ testCase "Resume random turns using captured rules and random state" $ do
            let configured = fixture{GameState.randomSeed = 73, GameState.combat = fixture.combat{CombatConfig.rules = fixture.combat.rules{Rules.damageFormula = Expression "floor(power * variance)", Rules.randomDamageMin = 0.85}}}
                opened = Save.apply configured (entry configured)
                history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
                midway = Save.apply configured history
                edited = configured{GameState.randomSeed = 999, GameState.combat = configured.combat{CombatConfig.rules = configured.combat.rules{Rules.damageFormula = Expression "999"}}}
            withHistory history $ \loaded -> do
                let resumed = Save.apply edited loaded
                assertBool "Expected the active battle and seed to survive game-file edits" (resumed.battle == midway.battle && resumed.party == midway.party && resumed.randomSeed == midway.randomSeed)
                assertBool "Expected identical future facts" (Turn.resolveAction resumed (UseMove (MoveId "strike")) == Turn.resolveAction midway (UseMove (MoveId "strike")))
        , testCase "Restore the entry seed before the first random turn" $ do
            let configured = fixture{GameState.randomSeed = 73}
                opened = Save.apply configured (entry configured)
                history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
                expected = Save.apply configured history
            withHistory history $ \loaded ->
                assertBool "Expected an unsampled seed to replay independently of the game file" (Save.apply configured{GameState.randomSeed = 999} loaded == expected)
        , testCase "Capture and acknowledge outcome rewards exactly once across reloads" $ do
            let configured = fixture{GameState.inventory = Map.singleton "capsule" 1, GameState.combat = fixture.combat{CombatConfig.items = Map.singleton "capsule" defaultItem{Item.captureBonus = 1}, CombatConfig.rules = fixture.combat.rules{Rules.captureFormula = Expression "1"}, CombatConfig.battles = Map.adjust (\definition -> definition{BattleDefinition.allowCapture = True, BattleDefinition.captureEffects = [GiveItem "capsule" 2, SetFlag (FlagId "caught") True]}) "practice" fixture.combat.battles}}
                opened = Save.apply configured (entry configured)
                history = entry configured <> Turn.resolveAction opened (UseItem "capsule" (-1))
            withHistory history $ \loaded -> do
                let resumed = Save.apply configured loaded
                    (closed, acknowledged) = Engine.step resumed Input.Enter
                assertBool "Expected the captured individual and consumed stock" (length resumed.party.members == 2 && Map.lookup "capsule" resumed.inventory == Just 0)
                withHistory (loaded <> acknowledged) $ \saved -> do
                    let replayed = Save.apply configured saved
                        (continued, _) = Engine.step replayed Input.Enter
                    assertBool "Expected rewards once and exploration to resume" (replayed == closed && replayed.battle == Nothing && Map.lookup "capsule" continued.inventory == Just 2 && Map.lookup (FlagId "caught") continued.flags == Just True)
        , testCase "Resume forced replacement without granting a free opponent action" $ do
            let faster = defaultSpecies{Species.learnset = Map.singleton 1 [MoveId "strike"], Species.stats = BaseStats (Map.singleton "speed" 50)}
                configured = fixture{GameState.combat = fixture.combat{CombatConfig.species = Map.insert "stonekin" faster fixture.combat.species}, GameState.party = Party [basicCritter 3 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
                opened = Save.apply configured (entry configured)
                history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
            withHistory history $ \loaded -> do
                let resumed = Save.apply configured loaded
                    replaced = Save.apply resumed (Turn.resolveAction resumed (SwitchCritter 1))
                assertBool "Expected the replacement menu to survive" (fmap (.phase) resumed.battle == Just ReplacementMenu)
                assertBool "Expected an intact replacement and no extra turn" (map (.hitPoints) replaced.party.members == [0, 10] && fmap (.turn) replaced.battle == Just 1 && fmap (.phase) replaced.battle == Just ActionMenu)
        ]

withHistory :: [Event] -> ([Event] -> IO ()) -> IO ()
withHistory history action = do
    directory <- getTemporaryDirectory
    bracket
        ( do
            (path, handle) <- openTempFile directory "critters-battle-history"
            hClose handle
            pure path
        )
        removeFile
        ( \path -> do
            result <- Save.save path history
            assertBool "Expected the history to be written" (result == Right ())
            Save.load path >>= either (const (assertFailure "Expected the history to decode")) action
        )
