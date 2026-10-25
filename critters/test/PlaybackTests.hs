module PlaybackTests (tests) where

import BattleTests (fixture, start)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event (..))
import Critters.Input qualified as Input
import Critters.Interface (Interface (..))
import Critters.Interface qualified as Interface
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Run (playEvents)
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.BattleDefinition qualified as BattleDefinition
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleSide (BattleSide (..))
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.CombatConfig qualified as CombatConfig
import Critters.State.GameState (GameState (..))
import Critters.State.GameState qualified as GameState
import Critters.State.MoveId (MoveId (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Witch (from)

tests :: TestTree
tests =
    testGroup
        "Battle playback"
        [ testCase "Render introduction dialogue one line at a time without combatants or options" $ do
            let configured = fixture{GameState.combat = fixture.combat{CombatConfig.battles = Map.adjust (\definition -> definition{BattleDefinition.introduction = ["Meet my new partner!", "Let's battle!"]}) "practice" fixture.combat.battles}}
                opened = start configured
                (next, _) = Engine.step opened Input.Enter
                (ready, events) = Engine.step next Input.Enter
            forM_ [(opened, "Meet my new partner!"), (next, "Let's battle!")] $ \(state, message) -> do
                let frame = TUI.renderFrame state
                    drawing = frameText frame
                assertBool "Expected a bottom dialogue panel without combat details or commands" $
                    viewport frame
                        && showsDialogue message frame
                        && all (\text -> not (Text.isInfixOf text drawing)) ["HP ", "Moves", "Items", "Party", "Leafling", "Stonekin"]
            assertBool "Expected send-out messages before returning to the normal battle view" $
                events == [BattleCombatantsSentOut]
                    && TUI.eventMessages 0 BattleCombatantsSentOut next ready == ["Battle sent out Stonekin.", "Go, Leafling!"]
                    && fmap (.phase) ready.battle == Just ActionMenu
                    && showsHealth 20 10 (TUI.renderFrame ready)
                    && Text.isInfixOf "> Moves" (frameText (TUI.renderFrame ready))
        , testCase "Apply and finish displaying each event before the next, without requesting input" $ do
            let opened = start fixture
                events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            displayed <- newIORef []
            inputs <- newIORef (0 :: Int)
            let interface =
                    Interface
                        { Interface.input = \_ -> modifyIORef' inputs (+ 1) >> pure Input.Enter
                        , Interface.log = \_ _ _ -> pure ()
                        , Interface.output = \context _ -> pure context
                        , Interface.outputEvent = \context event state -> do
                            modifyIORef' displayed (<> [(context, event, state)])
                            pure (context + 1)
                        }
            (context, final) <- playEvents interface (0 :: Int) opened events
            observations <- readIORef displayed
            inputCount <- readIORef inputs
            assertBool "Expected exactly one ordered output callback per event and no playback commands" $
                context == 3
                    && inputCount == 0
                    && map (\(index, _, _) -> index) observations == [0, 1, 2]
                    && map (\(_, event, _) -> event) observations == events
                    && map (\(_, _, state) -> state) observations == drop 1 (scanl Engine.applyEvent opened events)
                    && final == foldl' Engine.applyEvent opened events
        , testCase "Show each Strike once and animate only the health changed by that move" $
            let opened = start fixture
                events = Turn.resolveAction opened (UseMove (MoveId "strike"))
                states = drop 1 (scanl Engine.applyEvent opened events)
             in case (events, states) of
                    ([first, second, completed], [hit, retaliation, final]) -> do
                        let firstMessages = TUI.eventMessages 0 first opened hit
                            firstCount = maybe 0 (length . (.messages)) hit.battle
                            secondMessages = TUI.eventMessages firstCount second hit retaliation
                            secondCount = maybe 0 (length . (.messages)) retaliation.battle
                            firstFrames = TUI.dialogueFrames 4 "leafling used Basic strike." opened hit
                            secondFrames = TUI.dialogueFrames 4 "stonekin used Basic strike." hit retaliation
                        assertBool "Expected independent move sentences and no repeated text at turn completion" $
                            firstMessages == ["Leafling used Basic strike."]
                                && secondMessages == ["Stonekin used Basic strike."]
                                && TUI.eventMessages secondCount completed retaliation final == []
                        assertBool "Expected the first move's message during opponent damage while player HP stays fixed" $
                            length firstFrames == 4
                                && and (zipWith (showsHealth 20) [9, 8, 7, 5] firstFrames)
                                && all (showsDialogue "Leafling used Basic strike.") firstFrames
                        assertBool "Expected the second message during player damage while opponent HP stays fixed" $
                            length secondFrames == 4
                                && and (zipWith (\hp -> showsHealth hp 5) [19, 18, 17, 15] secondFrames)
                                && all (showsDialogue "Stonekin used Basic strike.") secondFrames
                    _ -> assertFailure "Expected two move events followed by turn completion"
        , testCase "Replace options with one bottom dialogue box and restore them in the final frame" $
            let opened = start fixture
                frames = TUI.dialogueFrames 3 "leafling used Basic strike." opened opened
                ordinary = frameText (TUI.renderFrame opened)
             in assertBool "Expected a bordered dialogue panel without selectable battle options" $
                    all (\frame -> viewport frame && showsDialogue "Leafling used Basic strike." frame && not (Text.isInfixOf "> Moves" (frameText frame))) frames
                        && Text.isInfixOf "> Moves" ordinary
                        && not (Text.isInfixOf "Leafling used Basic strike." ordinary)
        , testCase "Wrap dialogue by words and terminal columns within the border" $
            let opened = start fixture
             in forM_
                    [ (Text.replicate 57 "a" <> " word", ["A" <> Text.replicate 56 "a", "word"])
                    , (Text.replicate 29 "界" <> " word", [Text.replicate 29 "界", "word"])
                    , (Text.replicate 60 "e\x0301" <> " word", ["E\x0301" <> Text.replicate 59 "e\x0301", "word"])
                    ]
                    $ \(message, expected) ->
                        let frames = TUI.dialogueFrames 1 message opened opened
                            content frame = map (Text.stripEnd . Text.dropEnd 2 . Text.drop 2) (take 2 (drop (TUI.height - 3) (rows frame)))
                         in assertBool "Expected whole-word wrapping without breaking Unicode or the fixed viewport" $
                                all viewport frames && map content frames == [expected]
        , testCase "Menu navigation does not replay the previous turn's dialogue" $
            let opened = start fixture
                finished = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
                (menu, events) = Engine.step finished Input.Enter
             in assertBool "Expected the move menu to open without repeating battle messages" $
                    all (\event -> null (TUI.eventMessages 0 event finished menu)) events
        , testCase "Repeated identical moves remain visible on the next turn" $
            let opened = start fixture
                old = opened{GameState.battle = fmap (\active -> active{Battle.messages = ["leafling used Basic strike."]}) opened.battle}
             in case old.battle of
                    Just active ->
                        let event = MoveUsed Player (MoveId "strike") (BattleUpdate active Nothing Nothing Nothing)
                         in assertBool "Expected the new move even when its sentence matches the previous turn" $
                                TUI.eventMessages 0 event old old == ["Leafling used Basic strike."]
                    Nothing -> assertFailure "Expected an active battle"
        ]
  where
    frameText = Text.concat . rows
    rows = map (Text.concat . map (.glyph))
    showsDialogue expected frame =
        let drawing = rows frame
         in Text.strip (Text.dropEnd 1 (Text.drop 1 (drawing !! (TUI.height - 3)))) == expected
                && Text.isPrefixOf "┌" (drawing !! (TUI.height - 4))
                && Text.isPrefixOf "└" (last drawing)
    showsHealth playerHealth enemyHealth frame =
        let drawing = rows frame
         in Text.isInfixOf ("HP " <> number playerHealth <> "/20") (drawing !! 4)
                && Text.isInfixOf ("HP " <> number enemyHealth <> "/10") (drawing !! 2)
    number :: Int -> Text.Text
    number = from . show
    viewport frame = length frame == TUI.height && all ((== TUI.width * 2) . sum . map Cell.columns) frame
