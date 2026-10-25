module BattleMessageTests (tests) where

import BattleTests (fixture, start)
import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (UseMove))
import Critters.Battle.Common (Side (..))
import Critters.Battle.Menu qualified as Menu
import Critters.Battle.Message qualified as BattleMessage
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Expression (Value (..))
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (East))
import Critters.Interface.TUI qualified as TUI
import Critters.Message qualified as Message
import Critters.Stage (StageChange (..), StageOutcome (..))
import Critters.Stage qualified as Stage
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig qualified as Config
import Critters.State.GameState qualified as GameState
import Critters.State.Interaction (Interaction (..))
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Message (BattleMessage (..), Messages (..), Presentation (..))
import Critters.State.Move (defaultMove)
import Critters.State.Move qualified as Move
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.Npc qualified as Npc
import Critters.State.NpcId (NpcId (..))
import Critters.State.Position (Position (..))
import Critters.State.Species qualified as Species
import Critters.State.Stage (StageValues (..))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Witch (from)

assertRuntimeError :: Text -> IO a -> Assertion
assertRuntimeError expected action = do
    result <- try @ErrorCall action
    case result of
        Left failure -> assertBool "Expected the named runtime error" (Text.isInfixOf expected (from (show failure)))
        Right _ -> assertBool "Expected a runtime error" False

tests :: TestTree
tests =
    testGroup
        "Game messages"
        [ testCase "Describe actual increases, decreases and blocked changes"
            $ forM_
                [ (StageChange (-1) 0 (-1) "defense", Fell 1, "Your Leafling's Defence fell by 1 stage (0 → -1).")
                , (StageChange 2 0 2 "defense", Rose 2, "Your Leafling's Defence rose by 2 stages (0 → +2).")
                , (StageChange 6 5 3 "defense", Rose 1, "Your Leafling's Defence rose by 1 stage (+5 → +6).")
                , (StageChange (-6) (-5) (-3) "defense", Fell 1, "Your Leafling's Defence fell by 1 stage (-5 → -6).")
                , (StageChange 6 6 1 "defense", UpperLimit, "Your Leafling's Defence cannot go any higher.")
                , (StageChange (-6) (-6) (-1) "defense", LowerLimit, "Your Leafling's Defence cannot go any lower.")
                , (StageChange 0 0 0 "defense", Unchanged, "Your Leafling's Defence stayed unchanged (0).")
                ]
            $ \(change, outcome, expected) -> do
                Stage.stageOutcome change @?= outcome
                stage fixture Player change @?= expected
        , testCase "Use authored species and stat labels with custom templates" $ do
            let configured = fixture{GameState.messages = Messages (Map.insert "stageRose" "{subject}: {stat} {before} -> {after}; requested {requested}; multiplier {beforeMultiplier} -> {afterMultiplier}" fixture.messages.templates), GameState.combat = fixture.combat{Config.statNames = Map.insert "defense" "Guard" fixture.combat.statNames, Config.species = Map.adjust (\value -> value{Species.name = "Moss Pup"}) "leafling" fixture.combat.species}}
            stage configured Enemy (StageChange 2 1 4 "defense") @?= "Opposing Moss Pup: Guard 1 -> 2; requested 4; multiplier 1.5 -> 2"
        , testCase "Compose root templates and numeric expressions" $ do
            let messages = Messages (Map.fromList [("notice", "{messages.prefix}: {amount * 2}"), ("prefix", "Guard")])
            Message.renderMessage messages Map.empty (Message.named "notice" [("amount", Message.numeric 2)]) @?= Right "Guard: 4"
        , testCase "Keep literal arguments literal and expand explicit references" $ do
            let messages = Messages (Map.fromList [("notice", "{name}"), ("other", "Guard")])
            Message.renderMessage messages Map.empty (Message.named "notice" [("name", Message.text "{messages.other}")]) @?= Right "{messages.other}"
            Message.renderMessage messages Map.empty (Message.named "notice" [("name", Message.reference "{messages.other}")]) @?= Right "Guard"
        , testCase "Reject missing references, malformed templates and reference cycles" $
            forM_ [Map.singleton "a" "{messages.missing}", Map.singleton "a" "{unknown}", Map.singleton "a" "{", Map.fromList [("a", "{messages.b}"), ("b", "{messages.a}")]] $ \templates ->
                assertBool "Expected a useful rendering error" (isLeft (Message.renderText (Messages templates) Map.empty "{messages.a}"))
        , testCase "Reject local argument cycles" $
            assertBool "Expected cyclic local references to fail" (isLeft (Message.renderMessage (Messages (Map.singleton "a" "{x}")) Map.empty (Message.named "a" [("x", Message.reference "{y}"), ("y", Message.reference "{x}")])))
        , testCase "Support dotted references and hyphenated identifiers in expressions" $ do
            Message.renderText (Messages Map.empty) (Map.fromList [("npcs.route-1-clerk.name", TextValue "Clerk"), ("numbers.before", NumberValue 3)]) "{npcs.route-1-clerk.name}: {numbers.before-1}" @?= Right "Clerk: 2"
        , testCase "Serialize message identities, literal/reference arguments and presentation" $ do
            let value = BattleMessage (Message.named "battleDamage" [("species", Message.reference "{combat.species.leafling.name}"), ("amount", Message.numeric 2)]) HealthChange
            Aeson.eitherDecode (Aeson.encode value) @?= Right value
        , testCase "Raise named runtime errors without fallback wording" $ do
            assertRuntimeError "Missing message: stageRose" (evaluate (Text.length (stage fixture{GameState.messages = Messages (Map.delete "stageRose" fixture.messages.templates)} Player rise)))
            assertRuntimeError "combat.statNames.defense" (evaluate (Text.length (stage fixture{GameState.combat = fixture.combat{Config.statNames = Map.empty}} Player rise)))
        , testCase "Return actual stage facts from the engine" $ do
            let (values, change) = Stage.changeStage fixture.combat.stages (StageValues (Map.singleton "defense" 5)) "defense" 3
            values @?= StageValues (Map.singleton "defense" 6)
            change @?= StageChange 6 5 3 "defense"
        , testCase "Emit informative messages for both move targets and preserve replay" $
            forM_ [(Self, "Your Leafling's Defence fell by 1 stage (0 → -1)."), (Opponent, "Opposing Stonekin's Defence fell by 1 stage (0 → -1).")] $ \(target, expected) -> do
                let configured = fixture{GameState.combat = fixture.combat{Config.moves = Map.singleton (MoveId "strike") defaultMove{Move.power = 0, Move.effects = [ChangeStage target "defense" (-1)]}}}
                    opened = start configured
                    events = Turn.resolveAction opened (UseMove (MoveId "strike"))
                    result = foldl' Engine.applyEvent opened events
                assertBool "Expected the informative message in the log" (maybe False (elem expected . map (GameState.renderMessage result . (.message)) . (.messages)) result.battle)
                assertBool "Expected serialized events to reproduce state" (fmap (foldl' Engine.applyEvent opened) (Aeson.eitherDecode @[Event] (Aeson.encode events)) == Right result)
        , testCase "Preserve the battle catalogue when root messages are edited" $ do
            let opened = start fixture
                edited = opened{GameState.messages = Messages (Map.singleton "battleMoveMissed" "Changed")}
            GameState.renderMessage edited (Message.named "battleMoveMissed" []) @?= "The move missed."
        , testCase "Let translated damage wording keep its health-change presentation" $ do
            let configured = fixture{GameState.messages = Messages (Map.insert "battleDamage" "Ouch {amount}" (Map.insert "battleMoveUsed" "{species}: {move}" fixture.messages.templates))}
                opened = start configured
                events = Turn.resolveAction opened (UseMove (MoveId "strike"))
                after = maybe opened (Engine.applyEvent opened) (case events of event : _ -> Just event; [] -> Nothing)
            case (events, after.battle) of
                (event : _, Just active) -> do
                    assertBool "Expected custom damage text to remain in the recorded log" (any (Text.isPrefixOf "Ouch" . GameState.renderMessage after . (.message)) active.messages)
                    assertBool "Expected health-change messages to be suppressed by kind" (all (not . Text.isPrefixOf "Ouch") (TUI.eventMessages 0 event opened after))
                _ -> assertBool "Expected a move event" False
        , testCase "Suppress an empty narrative template without losing damage facts" $ do
            let configured = fixture{GameState.messages = Messages (Map.insert "battleMoveUsed" "" fixture.messages.templates)}
                opened = start configured
                events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            case events of
                event : _ -> do
                    let after = Engine.applyEvent opened event
                    TUI.eventMessages 0 event opened after @?= []
                [] -> assertBool "Expected a move event" False
        , testCase "Let root templates control menu and party text" $ do
            let configured = fixture{GameState.messages = Messages (Map.insert "menuMoves" "Techniques" (Map.insert "partyHeading" "Team {count}/{maximum}" fixture.messages.templates))}
                opened = start configured
            map fst (Menu.entries opened) @?= ["Techniques", "Party", "Items"]
            assertBool "Expected authored menu wording in the TUI" (Text.isInfixOf "Techniques" (TUI.render opened))
            assertBool "Expected authored party wording" (Text.isInfixOf "Team 1/6" (TUI.render configured{GameState.partyOpen = True}))
        , testCase "Freeze selected dialogue templates while keeping NPC names live" $ do
            let configured = fixture{GameState.messages = Messages (Map.insert "greeting" "Hello {npcs.guide.name}." fixture.messages.templates), GameState.playerDirection = East, GameState.playerPosition = Position 1 4, GameState.npcs = Map.singleton (NpcId "guide") (Npc [] [Interaction [] [] ["{messages.greeting}"] []] (Location (MapId "overworld") (Position 2 4)) "Guide")}
                (opened, events) = Engine.step configured Input.Enter
                edited = opened{GameState.messages = Messages Map.empty, GameState.npcs = Map.adjust (\npc -> npc{Npc.name = "Moss"}) (NpcId "guide") opened.npcs}
            assertBool "Expected the engine to select an interaction" (not (null events) && maybe False (const True) edited.activeInteraction)
            GameState.resolveText edited "{messages.greeting}" @?= Right "Hello Moss."
        ]
  where
    rise = StageChange 1 0 1 "defense"
    stage state side change = GameState.renderMessage state (BattleMessage.stageMessage state.combat side "leafling" change)
