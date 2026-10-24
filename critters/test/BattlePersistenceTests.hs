module BattlePersistenceTests (tests) where

import BattleTests (fixture)
import Control.Exception (bracket)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Formula (Formula (..))
import Critters.Input qualified as Input
import Critters.Save qualified as Save
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition (BattleDefinition (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..))
import Critters.State.Item (Item (..), defaultItem)
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules (Rules (..))
import Critters.State.Species (Species (..), defaultSpecies)
import Data.Map.Strict qualified as Map
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

entry :: GameState -> [Event]
entry state = either (const []) id (Engine.eventsForEffects state [StartBattle "practice"])

tests :: TestTree
tests = testGroup "Battle persistence"
  [ testCase "Resume random turns using captured rules and random state" $ do
      let configured = fixture{randomSeed = 73, combat = (combat fixture){configRules = (configRules (combat fixture)){rulesDamageFormula = Formula "floor(power * variance)", rulesRandomDamageMin = 0.85}}}
          opened = Save.apply configured (entry configured)
          history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
          midway = Save.apply configured history
          edited = configured{randomSeed = 999, combat = (combat configured){configRules = (configRules (combat configured)){rulesDamageFormula = Formula "999"}}}
      withHistory history $ \loaded -> do
        let resumed = Save.apply edited loaded
        assertBool "Expected the active battle and seed to survive game-file edits" (battle resumed == battle midway && party resumed == party midway && randomSeed resumed == randomSeed midway)
        assertBool "Expected identical future facts" (Turn.resolveAction resumed (UseMove (MoveId "strike")) == Turn.resolveAction midway (UseMove (MoveId "strike")))
    , testCase "Restore the entry seed before the first random turn" $ do
      let configured = fixture{randomSeed = 73}
          opened = Save.apply configured (entry configured)
          history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
          expected = Save.apply configured history
      withHistory history $ \loaded ->
        assertBool "Expected an unsampled seed to replay independently of the game file" (Save.apply configured{randomSeed = 999} loaded == expected)
    , testCase "Capture and acknowledge outcome rewards exactly once across reloads" $ do
      let configured = fixture{inventory = Map.singleton "capsule" 1, combat = (combat fixture){configItems = Map.singleton "capsule" defaultItem{itemCaptureBonus = 1}, configRules = (configRules (combat fixture)){rulesCaptureFormula = Formula "1"}, configBattles = Map.adjust (\definition -> definition{definitionAllowCapture = True, definitionCaptureEffects = [GiveItem "capsule" 2, SetFlag (FlagId "caught") True]}) "practice" (configBattles (combat fixture))}}
          opened = Save.apply configured (entry configured)
          history = entry configured <> Turn.resolveAction opened (UseItem "capsule" (-1))
      withHistory history $ \loaded -> do
        let resumed = Save.apply configured loaded
            (closed, acknowledged) = Engine.step resumed Input.Enter
        assertBool "Expected the captured individual and consumed stock" (length (members (party resumed)) == 2 && Map.lookup "capsule" (inventory resumed) == Just 0)
        withHistory (loaded <> acknowledged) $ \saved -> do
          let replayed = Save.apply configured saved
              (continued, _) = Engine.step replayed Input.Enter
          assertBool "Expected rewards once and exploration to resume" (replayed == closed && battle replayed == Nothing && Map.lookup "capsule" (inventory continued) == Just 2 && Map.lookup (FlagId "caught") (flags continued) == Just True)
    , testCase "Resume forced replacement without granting a free opponent action" $ do
      let faster = defaultSpecies{speciesLearnset = Map.singleton 1 [MoveId "strike"], speciesStats = Map.singleton "speed" 50}
          configured = fixture{combat = (combat fixture){configSpecies = Map.insert "stonekin" faster (configSpecies (combat fixture))}, party = Party [basicCritter 3 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
          opened = Save.apply configured (entry configured)
          history = entry configured <> Turn.resolveAction opened (UseMove (MoveId "strike"))
      withHistory history $ \loaded -> do
        let resumed = Save.apply configured loaded
            replaced = Save.apply resumed (Turn.resolveAction resumed (SwitchCritter 1))
        assertBool "Expected the replacement menu to survive" (fmap battlePhase (battle resumed) == Just ReplacementMenu)
        assertBool "Expected an intact replacement and no extra turn" (map hitPoints (members (party replaced)) == [0,10] && fmap battleTurn (battle replaced) == Just 1 && fmap battlePhase (battle replaced) == Just ActionMenu)
  ]

withHistory :: [Event] -> ([Event] -> IO ()) -> IO ()
withHistory history action = do
  directory <- getTemporaryDirectory
  bracket
    (do
      (path, handle) <- openTempFile directory "critters-battle-history"
      hClose handle
      pure path)
    removeFile
    (\path -> do
      result <- Save.save path history
      assertBool "Expected the history to be written" (result == Right ())
      Save.load path >>= either (const (assertFailure "Expected the history to decode")) action)
