module BattleTests (fixture, start, tests) where

import Autodocodec.Yaml qualified as Schema
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Engine qualified as BattleEngine
import Critters.Battle.Turn qualified as Turn
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event qualified as Event
import Critters.Formula (Formula (..))
import Critters.Formula qualified as Formula
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (East))
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Save qualified as Save
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition)
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.Choice (Choice (..))
import Critters.State.CombatConfig (CombatConfig (..), defaultCombat)
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.Encounter (Encounter (..))
import Critters.State.EncounterEntry (EncounterEntry (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..), initialState)
import Critters.State.Interaction (Interaction (..))
import Critters.State.Item (Item (..), defaultItem)
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Move (Move (..), defaultMove)
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Object (Object (..))
import Critters.State.ObjectId (ObjectId (..))
import Critters.State.Party (Party (..))
import Critters.State.Position (Position (..))
import Critters.State.Rules (Rules (..), defaultRules)
import Critters.State.Species (BaseStats (..), Species (..), defaultSpecies)
import Critters.State.StatusDefinition (StatusDefinition (..), defaultStatus)
import Critters.State.Tile (Tile (Ground))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Witch (from)

animationTests :: TestTree
animationTests =
  testGroup
    "Health animation"
    [ testCase "Render empty, partial and full health bars" $
        forM_ [(0, 20, 0), (10, 20, 8), (20, 20, 16), (1, 3, 5)] $ \(current, maximumHealth, filled) ->
          assertBool "Expected a sixteen-segment gauge proportional to current health" $
            TUI.healthBar current maximumHealth == "[" <> Text.replicate filled "█" <> Text.replicate (16 - filled) "░" <> "]",
      testCase "Animate both health counters and bars to their exact final values" $
        let before = start fixture
            after = withHealth 12 6 before
            frames = TUI.animationFrames 4 before after
         in assertBool "Expected four ordered frames with matching HP, bars and viewport dimensions" $
              length frames == 4
                && and (zipWith showsHealth [(18, 9), (16, 8), (14, 7), (12, 6)] frames)
                && all viewport frames
                && drop 3 frames == [TUI.renderFrame after],
      testCase "Animate healing and damage down to zero health" $
        let before = withHealth 5 5 (start fixture)
            after = withHealth 20 0 before
            frames = TUI.animationFrames 3 before after
         in assertBool "Expected gradual healing and damage without overshooting either endpoint" $
              length frames == 3
                && and (zipWith showsHealth [(10, 4), (15, 2), (20, 0)] frames)
                && drop 2 frames == [TUI.renderFrame after],
      testCase "Draw immediately when health is unchanged or a battle starts or ends" $
        let opened = start fixture
            (menu, _) = Engine.step opened Input.Enter
         in forM_ [(initialState, opened), (opened, initialState), (opened, opened), (opened, menu)] $ \(before, after) ->
              assertBool "Expected one frame without an animation delay" $
                TUI.animationFrames 12 before after == [TUI.renderFrame after],
      testCase "Draw the final state when no animation frames are requested" $
        let before = start fixture
            after = withHealth 12 6 before
         in forM_ [0, -1] $ \count ->
              assertBool "Expected the exact final display" (TUI.animationFrames count before after == [TUI.renderFrame after]),
      testCase "Do not interpolate between different active party members" $
        let before = start fixture{party = Party [basicCritter 20 2 20 "leafling", basicCritter 7 2 20 "leafling"]}
            after = before{battle = fmap (\active -> active{battleActivePlayer = 1}) (battle before)}
         in assertBool "Expected the incoming critter's actual health immediately" $
              TUI.animationFrames 12 before after == [TUI.renderFrame after],
      testCase "Replace numeric damage messages in the TUI while retaining other battle messages" $
        let opened = start fixture
            state = opened{battle = fmap (\active -> active{battleMessages = ["leafling used Basic strike.", "stonekin lost 2 HP.", "leafling is poisoned.", "Victory."]}) (battle opened)}
            update = BattleUpdate (maybe defaultBattle id (battle state)) Nothing Nothing Nothing
            messages = TUI.eventMessages 0 (Event.BattleChanged update) opened state
            frames = concatMap (\message -> TUI.dialogueFrames 1 message opened state) messages
            drawings = map (Text.concat . map (Text.concat . map Cell.glyph)) frames
         in assertBool "Expected move, status and outcome messages with both combatants' health bars" $
              messages == ["Leafling used Basic strike.", "Leafling is poisoned.", "You defeated Battle"]
                && all (\drawing -> not (Text.isInfixOf "lost 2 HP." drawing) && Text.count ("[" <> Text.replicate 16 "█" <> "]") drawing == 2) drawings
    ]
  where
    rows = map (Text.stripEnd . Text.concat . map Cell.glyph)
    showsHealth (playerHealth, enemyHealth) frame = case rows frame of
      _ : _ : enemy : enemyBar : player : playerBar : _ ->
        Text.isSuffixOf ("HP " <> number enemyHealth <> "/10") enemy
          && enemyBar == TUI.healthBar enemyHealth 10
          && Text.isSuffixOf ("HP " <> number playerHealth <> "/20") player
          && playerBar == TUI.healthBar playerHealth 20
      _ -> False
    number :: Int -> Text.Text
    number = from . show
    viewport frame = length frame == TUI.height && all ((== TUI.width * 2) . sum . map Cell.columns) frame
    withHealth playerHealth enemyHealth state =
      state
        { battle = fmap (\active -> active{battleOpponentParty = map (\critter -> critter{hitPoints = enemyHealth}) (battleOpponentParty active)}) (battle state),
          party = Party (map (\critter -> critter{hitPoints = playerHealth}) (members (party state)))
        }

encounterTests :: TestTree
encounterTests =
  testGroup
    "Encounters"
    [ testCase "Generate a full-health wild critter from a weighted encounter entry" $
        let configured = fixture{combat = (combat fixture){configEncounters = [Encounter 1 [EncounterEntry True True 3 "stonekin" 1] (MapId "overworld") 2 [Ground]]}}
            (first, events1) = Engine.step configured Input.Right
            (second, events2) = Engine.step first Input.Right
            encounterDetails active = (map level (battleOpponentParty active), map hitPoints (battleOpponentParty active), map maxHitPoints (battleOpponentParty active), definitionAllowCapture (battleDefinition active), definitionAllowEscape (battleDefinition active))
         in assertBool "Expected a minimum step count, generated full health, encounter permissions, and replayable entry" (battle first == Nothing && encounterSteps first == 1 && fmap encounterDetails (battle second) == Just ([3], [26], [26], True, True) && encounterSteps second == 0 && foldl' Engine.applyEvent configured (events1 <> events2) == second),
      testCase "Blocked movement and unusable parties do not roll encounters" $
        let configured = fixture{combat = (combat fixture){configEncounters = [Encounter 1 [EncounterEntry True True 2 "stonekin" 1] (MapId "overworld") 0 [Ground]]}}
            (blocked, _) = Engine.step configured Input.Left
            (empty, _) = Engine.step configured{party = Party []} Input.Right
         in assertBool "Expected no encounter state or random changes" (battle blocked == Nothing && encounterSteps blocked == 0 && randomSeed blocked == randomSeed configured && battle empty == Nothing && encounterSteps empty == 0),
      testCase "Encounter slots default to capturable and escapable" $
        assertBool "Expected wild encounter permissions to default to true" (Aeson.eitherDecode @EncounterEntry "{\"level\":3,\"species\":\"stonekin\"}" == Right (EncounterEntry True True 3 "stonekin" 1)),
      testCase "Encounter tables and selected outcomes survive serialization" $
        let configured = fixture{combat = (combat fixture){configEncounters = [Encounter 0.8 [EncounterEntry False False 2 "stonekin" 3] (MapId "overworld") 0 [Ground]]}}
         in assertBool "Expected complete encounter data to round trip" (Aeson.eitherDecode (Aeson.encode configured) == Right configured)
    ]

fixture :: GameState
fixture =
  initialState
    { combat =
        defaultCombat
          { configBattles = Map.singleton "practice" defaultDefinition{definitionOpponents = [basicCritter 10 2 10 "stonekin"]},
            configItems = Map.singleton "tonic" defaultItem{itemHealing = 5},
            configMoves = Map.singleton (MoveId "strike") defaultMove{moveMaxUses = 5, movePower = 5},
            configRules = defaultRules{rulesDamageFormula = Formula "power"},
            configSpecies = Map.fromList [("leafling", definition), ("stonekin", definition)]
          },
      party = Party [basicCritter 20 2 20 "leafling"]
    }
  where
    definition = defaultSpecies{speciesExperienceYield = 0, speciesLearnset = Map.singleton 1 [MoveId "strike"]}

itemTests :: TestTree
itemTests =
  testGroup
    "Battle items and escape"
    [ testCase "Heal a chosen party member and consume a turn" $
        let opened = start fixture{inventory = Map.singleton "tonic" 1, party = Party [basicCritter 10 2 20 "leafling"]}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseItem "tonic" 0))
         in assertBool "Expected healing followed by retaliation and item consumption" (map hitPoints (members (party result)) == [10] && inventory result == Map.singleton "tonic" 0 && fmap battleTurn (battle result) == Just 1),
      testCase "Ineffective items consume neither stock nor a turn" $
        let opened = start fixture{inventory = Map.singleton "tonic" 1}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseItem "tonic" 0))
         in assertBool "Expected a visible rejection without any resource change" (party result == party opened && inventory result == inventory opened && fmap battleTurn (battle result) == Just 0 && maybe False (any (Text.isInfixOf "no effect") . battleMessages) (battle result)),
      testCase "Capture a configured opponent and preserve its state" $
        let opened = start capturable
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseItem "capsule" (-1)))
         in assertBool "Expected a bounded party addition and a recorded capture" (map species (members (party result)) == ["leafling", "stonekin"] && map hitPoints (members (party result)) == [20, 10] && inventory result == Map.singleton "capsule" 0 && fmap battleOutcome (battle result) == Just (Just Captured)),
      testCase "Reject full-party and forbidden capture attempts" $ do
        let full = start capturable{maxPartySize = 1}
            unchanged = foldl' Engine.applyEvent full (Turn.resolveAction full (UseItem "capsule" (-1)))
            forbidden = start capturable{combat = (combat capturable){configBattles = Map.adjust (\definition -> definition{definitionAllowCapture = False}) "practice" (configBattles (combat capturable))}}
            denied = foldl' Engine.applyEvent forbidden (Turn.resolveAction forbidden (UseItem "capsule" (-1)))
        assertBool "Expected full-party rejection without consuming the item" (party unchanged == party full && inventory unchanged == inventory full && fmap battleTurn (battle unchanged) == Just 0)
        assertBool "Expected capture permissions to be enforced" (party denied == party forbidden && inventory denied == inventory forbidden),
      testCase "Failed captures consume the item and permit retaliation" $
        let opened = start capturable{combat = (combat capturable){configRules = (configRules (combat capturable)){rulesCaptureFormula = Formula "0"}}}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseItem "capsule" (-1)))
         in assertBool "Expected the failed attempt to spend a turn" (map hitPoints (members (party result)) == [15] && inventory result == Map.singleton "capsule" 0 && fmap battleOutcome (battle result) == Just Nothing),
      testCase "Escape uses the configured formula and previous failures" $
        let configured = fixture{combat = (combat fixture){configBattles = Map.adjust (\definition -> definition{definitionAllowEscape = True}) "practice" (configBattles (combat fixture)), configRules = (configRules (combat fixture)){rulesEscapeFormula = Formula "if(attempts >= 1, 1, 0)"}}}
            opened = start configured
            failed = foldl' Engine.applyEvent opened (Turn.resolveAction opened EscapeBattle)
            escaped = foldl' Engine.applyEvent failed (Turn.resolveAction failed EscapeBattle)
         in assertBool "Expected a failed attempt followed by safe escape" (fmap battleEscapeAttempts (battle failed) == Just 1 && map hitPoints (members (party escaped)) == [15] && fmap battleOutcome (battle escaped) == Just (Just Escaped))
    ]
  where
    capturable = fixture{combat = (combat fixture){configBattles = Map.adjust (\definition -> definition{definitionAllowCapture = True}) "practice" (configBattles (combat fixture)), configItems = Map.singleton "capsule" defaultItem{itemCaptureBonus = 1}, configRules = (configRules (combat fixture)){rulesCaptureFormula = Formula "1"}}, inventory = Map.singleton "capsule" 1}

lifecycleTests :: TestTree
lifecycleTests =
  testGroup
    "Battle entry"
    [ testCase "Start a configured battle and block exploration" $
        case BattleEngine.beginBattle fixture "practice" of
          Left _ -> assertBool "Expected a valid battle" False
          Right active ->
            let opened = Engine.applyEvent fixture (Event.BattleStarted active)
                (selected, _) = Engine.step opened Input.Right
             in assertBool "Expected a configuration snapshot and an isolated battle context" (battleConfig active == combat fixture && selected == opened{battle = Just active{battleSelected = 1}} && Engine.eventsFor opened (Command.Walk East) == []),
      testCase "Reject empty, fainted or unknown parties before any effects commit" $ do
        assertBool "Expected an empty party to be rejected" (isLeft (Engine.eventsForEffects fixture{party = Party []} [SetFlag (FlagId "test") True, StartBattle "practice"]))
        assertBool "Expected a fainted party to be rejected" (isLeft (BattleEngine.beginBattle fixture{party = Party [basicCritter 0 2 20 "leafling"]} "practice"))
        assertBool "Expected an unknown battle to be rejected" (isLeft (BattleEngine.beginBattle fixture "missing")),
      testCase "Grant configured items and restore an individual profile" $
        let injured = (basicCritter 0 2 20 "leafling"){battleProfile = defaultProfile{profileExperience = 12, profileStatus = Just (Affliction Nothing "slow"), profileRemainingUses = Map.singleton (MoveId "strike") 0}}
            state = fixture{party = Party [injured]}
         in case Engine.eventsForEffects state [GiveItem "tonic" 2, HealParty] of
              Left _ -> assertBool "Expected valid recovery effects" False
              Right events ->
                let result = foldl' Engine.applyEvent state events
                 in assertBool "Expected healing without losing learned progression" (inventory result == Map.singleton "tonic" 2 && party result == Party [injured{hitPoints = 20, battleProfile = (battleProfile injured){profileStatus = Nothing, profileRemainingUses = Map.empty}}])
    ]

opponentTests :: TestTree
opponentTests =
  testGroup
    "Opponent policies"
    [ testCase "Choose the highest-scoring available move and respect its uses" $
        let configured = armed "strongest"
            opened = start configured
            first = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
            second = foldl' Engine.applyEvent first (Turn.resolveAction first (UseMove (MoveId "strike")))
         in assertBool "Expected a stronger attack followed by an available weaker one" (map hitPoints (members (party first)) == [11] && map hitPoints (members (party second)) == [6]),
      testCase "Random opponents make reproducible, recorded choices" $
        let opened = start (armed "random")
            facts = Turn.resolveAction opened (UseMove (MoveId "strike"))
            result = foldl' Engine.applyEvent opened facts
         in assertBool "Expected reproducibility and advanced random state" (facts == Turn.resolveAction opened (UseMove (MoveId "strike")) && randomSeed result /= randomSeed opened)
    ]
  where
    armed policy = fixture{combat = (combat fixture){configMoves = Map.insert (MoveId "heavy") defaultMove{moveName = "Heavy strike", movePower = 9, moveMaxUses = 1} (configMoves (combat fixture)), configSpecies = Map.adjust (\definition -> definition{speciesLearnset = Map.singleton 1 [MoveId "strike", MoveId "heavy"]}) "stonekin" (configSpecies (combat fixture)), configBattles = Map.adjust (\definition -> definition{definitionOpponentPolicy = policy, definitionOpponents = [basicCritter 30 2 30 "stonekin"]}) "practice" (configBattles (combat fixture))}}

profileTests :: TestTree
profileTests =
  testGroup
    "Combat profiles"
    [ testCase "Preserve individual experience, moves, resources and status" $
        let critter = (basicCritter 8 2 10 "leafling"){battleProfile = defaultProfile{profileExperience = 15, profileKnownMoves = [MoveId "strike"], profileRemainingUses = Map.singleton (MoveId "strike") 2, profileStatus = Just (Affliction (Just 3) "slow")}}
         in assertBool "Expected a complete individual combat profile" (Aeson.eitherDecode (Aeson.encode critter) == Right critter),
      testCase "Load authored species stats and level-based moves" $
        let definition = defaultSpecies{speciesLearnset = Map.singleton 3 [MoveId "strike"], speciesStats = BaseStats (Map.singleton "focus" 12)}
         in assertBool "Expected author-defined stats and moves" (Aeson.eitherDecode (Aeson.encode definition) == Right definition),
      testCase "Reject invalid progression, duplicate moves and statuses" $ do
        assertBool "Expected nonnegative experience" (isLeft (Aeson.eitherDecode @BattleProfile "{\"experience\":-1}"))
        assertBool "Expected unique learned moves" (isLeft (Aeson.eitherDecode @BattleProfile "{\"knownMoves\":[\"strike\",\"strike\"]}"))
        assertBool "Expected positive status duration" (isLeft (Aeson.eitherDecode @Affliction "{\"status\":\"slow\",\"remaining\":0}"))
        assertBool "Expected positive species stats" (isLeft (Aeson.eitherDecode @Species "{\"baseStats\":{\"attack\":0}}"))
    ]

progressionTests :: TestTree
progressionTests =
  testGroup
    "Progression"
    [ testCase "Gain experience, increase health and learn an authored move" $
        let opened = start progressing
            result = win opened
         in assertBool "Expected curve-based growth and learned moves" (map level (members (party result)) == [3] && map maxHitPoints (members (party result)) == [26] && map hitPoints (members (party result)) == [21] && map (profileExperience . battleProfile) (members (party result)) == [28] && map (profileKnownMoves . battleProfile) (members (party result)) == [[MoveId "strike", MoveId "surge"]]),
      testCase "Apply the configured move overflow and level cap policies" $
        let configured = progressing{combat = (combat progressing){configRules = (configRules (combat progressing)){rulesMaxKnownMoves = 1, rulesMaxLevel = 3}}}
            newest = win (start configured)
            oldest = win (start configured{combat = (combat configured){configRules = (configRules (combat configured)){rulesLearningPolicy = "keep-oldest"}}})
         in assertBool "Expected game-defined learning behavior" (map (profileKnownMoves . battleProfile) (members (party newest)) == [[MoveId "surge"]] && map (profileKnownMoves . battleProfile) (members (party oldest)) == [[MoveId "strike"]] && map (profileExperience . battleProfile) (members (party newest)) == [27]),
      testCase "Share rewards according to the configured recipient policy" $
        let configured = progressing{party = Party [basicCritter 20 2 20 "leafling", basicCritter 20 2 20 "leafling"], combat = (combat progressing){configRules = (configRules (combat progressing)){rulesExperienceSharing = "party"}}}
            result = win (start configured)
         in assertBool "Expected a divided reward for both conscious party members" (map (profileExperience . battleProfile) (members (party result)) == [18, 18]),
      testCase "Do not award an opponent twice when acknowledging the outcome" $
        let won = win (start progressing)
            (closed, _) = Engine.step won Input.Enter
         in assertBool "Expected acknowledgement to retain the recorded progression" (party closed == party won && battle closed == Nothing),
      testCase "Reject a nonincreasing experience curve" $
        assertBool "Expected an invalid growth policy to fail decoding" (isLeft (Aeson.eitherDecode @Rules "{\"experienceFormula\":\"1\"}"))
    ]
  where
    progressing = fixture{combat = (combat fixture){configMoves = Map.insert (MoveId "surge") defaultMove{moveName = "Surge", movePower = 12} (configMoves (combat fixture)), configSpecies = Map.adjust (\definition -> definition{speciesExperienceYield = 10}) "stonekin" (Map.adjust (\definition -> definition{speciesLearnset = Map.fromList [(1, [MoveId "strike"]), (3, [MoveId "surge"])]}) "leafling" (configSpecies (combat fixture)))}}
    win opened = iterate (\state -> foldl' Engine.applyEvent state (Turn.resolveAction state (UseMove (MoveId "strike")))) opened !! 2

rematchTests :: TestTree
rematchTests =
  testGroup
    "Rematches"
    [ testCase "Rematch after losing without removing consumed objects again" $
        forM_ companions $ \(name, identifier, selection) -> do
          let offered = Interaction [Choice [GiveCritter (basicCritter 10 2 10 name), RemoveObject companionBox, RemoveObject opponentBox, StartBattle identifier] "Accept"] [] ["Choose this companion?"] []
              ready =
                configured
                  { objects = Map.fromList [(companionBox, Object "🔴" [offered] (Location (MapId "overworld") (Position 2 1))), (opponentBox, Object "🔴" [] (Location (MapId "overworld") (Position 3 1)))]
                  }
              choose state input = fst (Engine.step state input)
              started = foldl' choose ready [Input.Enter, Input.Enter]
              loseTurn state = foldl' Engine.applyEvent state (Turn.resolveAction state (UseMove (MoveId "wait")))
              lost = iterate loseTurn started !! 3
              acknowledged = choose lost Input.Enter
              atOpponent = acknowledged{playerPosition = Position 4 1}
              rematched = foldl' choose atOpponent ([Input.Enter] <> selection <> [Input.Enter])
          assertBool "Expected the companion choice to consume both objects and start the configured battle" $
            map species (members (party started)) == [name]
              && Map.null (objects started)
              && fmap battleDefinition (battle started) == Map.lookup identifier (configBattles (combat configured))
          assertBool "Expected a lost battle to be acknowledged" $
            fmap battleOutcome (battle lost) == Just (Just Defeat) && battle acknowledged == Nothing
          assertBool "Expected a fresh matching battle without removing objects or changing the player's party" $
            fmap battleDefinition (battle rematched) == Map.lookup identifier (configBattles (combat configured))
              && fmap battleOutcome (battle rematched) == Just Nothing
              && fmap battleTurn (battle rematched) == Just 0
              && activeInteraction rematched == Nothing
              && objects rematched == objects acknowledged
              && party rematched == party acknowledged
    ]
  where
    companionBox = ObjectId "companion-box"
    companions = [("leafling", "practice-leaf", []), ("emberling", "practice-ember", [Input.Right]), ("brookling", "practice-brook", [Input.Down])]
    configured =
      fixture
        { combat =
            (combat fixture)
              { configBattles = Map.fromList [(identifier, defaultDefinition{definitionName = identifier, definitionOpponents = [basicCritter 10 2 10 "stonekin"]}) | (_, identifier, _) <- companions],
                configMoves = Map.insert (MoveId "wait") defaultMove{moveName = "Wait", movePower = 0} (configMoves (combat fixture)),
                configSpecies = Map.union (Map.fromList [(name, defaultSpecies{speciesLearnset = Map.singleton 1 [MoveId "wait"]}) | (name, _, _) <- companions]) (configSpecies (combat fixture))
              },
          npcs = Map.singleton (NpcId "opponent") (Npc [] [Interaction [Choice [StartBattle identifier] name | (name, identifier, _) <- companions] [] ["Which companion will battle?"] []] (Location (MapId "overworld") (Position 5 1))),
          party = Party [],
          playerDirection = East,
          playerPosition = Position 1 1
        }
    opponentBox = ObjectId "opponent-box"

snapshotTests :: TestTree
snapshotTests =
  testGroup
    "Battle snapshots"
    [ testCase "Round trip a configured battle and complete game state" $
        let critter = basicCritter 10 2 10 "leafling"
            definition = defaultDefinition{definitionOpponents = [critter]}
            config = defaultCombat{configSpecies = Map.singleton "leafling" defaultSpecies, configBattles = Map.singleton "practice" definition}
            active = defaultBattle{battleConfig = config, battleDefinition = definition, battleOpponentParty = [critter], battleParticipants = [0]}
            state = initialState{battle = Just active, combat = config, party = Party [critter]}
         in assertBool "Expected the battle configuration snapshot and phase to persist" (Aeson.eitherDecode (Aeson.encode state) == Right state),
      testCase "Replay facts without validating commands or resetting other state" $
        let critter = basicCritter 10 2 10 "leafling"
            active = defaultBattle{battleOpponentParty = [critter]}
            starting = initialState{party = Party [critter], partyOpen = True}
            update = BattleUpdate active (Just (Map.singleton "tonic" 2)) Nothing (Just 42)
            result = Engine.applyEvent starting (Event.BattleChanged update)
         in assertBool "Expected exactly the recorded fields to change" (result == starting{battle = Just active, inventory = Map.singleton "tonic" 2, randomSeed = 42} && Aeson.eitherDecode (Aeson.encode (Event.BattleChanged update)) == Right (Event.BattleChanged update)),
      testCase "Reject dangling species, move and encounter references" $ do
        assertBool "Expected a learnset reference to exist" (isLeft (Aeson.eitherDecode @CombatConfig "{\"species\":{\"leafling\":{\"learnset\":{\"1\":[\"missing\"]}}}}"))
        assertBool "Expected an encounter species reference to exist" (isLeft (Aeson.eitherDecode @CombatConfig "{\"encounters\":[{\"mapId\":\"overworld\",\"entries\":[{\"species\":\"missing\",\"level\":2}]}]}"))
        assertBool "Expected nonnegative inventory counts" (isLeft (Aeson.eitherDecode @GameState "{\"version\":1,\"maps\":{},\"playerPosition\":{\"x\":0,\"y\":0},\"inventory\":{\"tonic\":-1}}"))
    ]

statusTests :: TestTree
statusTests =
  testGroup
    "Status and move effects"
    [ testCase "Block an action without spending uses and expire a timed status" $
        let configured = configure defaultMove{movePower = 0, moveEffects = [InflictStatus Opponent "wither"]} defaultStatus{statusBlockChance = 1, statusDurationMin = 1, statusDurationMax = 1, statusTickOnApply = True}
            opened = start configured
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "technique")))
         in assertBool "Expected a blocked opponent, no retaliation, and status expiry" (map hitPoints (members (party result)) == [20] && fmap (map (profileStatus . battleProfile) . battleOpponentParty) (battle result) == Just [Nothing] && fmap (map (profileRemainingUses . battleProfile) . battleOpponentParty) (battle result) == Just [Map.empty]),
      testCase "Apply residual damage for the configured duration" $
        let configured = configure defaultMove{movePower = 0, moveEffects = [InflictStatus Opponent "wither"]} defaultStatus{statusDamageFraction = 0.25, statusDurationMin = 2, statusDurationMax = 2, statusTickOnApply = True}
            opened = start configured
            first = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "technique")))
            second = foldl' Engine.applyEvent first (Turn.resolveAction first (UseMove (MoveId "technique")))
         in assertBool "Expected bounded residual HP loss and eventual recovery" (fmap (map hitPoints . battleOpponentParty) (battle first) == Just [8] && fmap (map hitPoints . battleOpponentParty) (battle second) == Just [6] && fmap (map (profileStatus . battleProfile) . battleOpponentParty) (battle second) == Just [Nothing]),
      testCase "Clamp stat stages and clear them on a configured switch" $
        let configured = configure defaultMove{movePower = 0, moveEffects = [ChangeStage Self "attack" maxBound]} defaultStatus
            opened = start configured{party = Party (members (party configured) <> [basicCritter 10 2 10 "leafling"])}
            boosted = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "technique")))
            switched = foldl' Engine.applyEvent boosted (Turn.resolveAction boosted (SwitchCritter 1))
         in assertBool "Expected the game limit and switch reset" (fmap (Map.lookup "attack" . battlePlayerStages) (battle boosted) == Just (Just 6) && fmap battlePlayerStages (battle switched) == Just Map.empty),
      testCase "Use actual total damage for drain effects" $
        let configured = configure defaultMove{movePower = 5, moveMinHits = 2, moveMaxHits = 2, moveEffects = [ChangeHealth Self (Formula "damage / 2")]} defaultStatus
            opened = start configured{party = Party [((basicCritter 10 2 20 "leafling"){battleProfile = defaultProfile{profileKnownMoves = [MoveId "technique"]}})]}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "technique")))
         in assertBool "Expected drain from both hits before the battle ends" (map hitPoints (members (party result)) == [15] && fmap battleOutcome (battle result) == Just (Just Victory)),
      testCase "Configure simultaneous final faint outcomes" $
        let configured = configure defaultMove{movePower = 10, moveEffects = [ChangeHealth Self (Formula "-hp")]} defaultStatus
            opened = start configured
            lost = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "technique")))
            alternative = start configured{combat = (combat configured){configRules = (configRules (combat configured)){rulesSimultaneousDefeatWins = True}}}
            won = foldl' Engine.applyEvent alternative (Turn.resolveAction alternative (UseMove (MoveId "technique")))
         in assertBool "Expected the authored outcome policy" (fmap battleOutcome (battle lost) == Just (Just Defeat) && fmap battleOutcome (battle won) == Just (Just Victory))
    ]
  where
    configure move status = fixture{combat = (combat fixture){configMoves = Map.insert (MoveId "technique") move (configMoves (combat fixture)), configStatuses = Map.singleton "wither" status}, party = Party [(basicCritter 20 2 20 "leafling"){battleProfile = defaultProfile{profileKnownMoves = [MoveId "technique"]}}]}

start :: GameState -> GameState
start state = either (const state) (\active -> state{battle = Just active}) (BattleEngine.beginBattle state "practice")

tests :: TestTree
tests =
  testGroup
    "Battle"
    [ animationTests,
      encounterTests,
      itemTests,
      lifecycleTests,
      opponentTests,
      profileTests,
      progressionTests,
      rematchTests,
      snapshotTests,
      statusTests,
      turnTests,
      variationTests,
      viewTests,
      testGroup
        "Rules"
        [ testCase "Render finite documentation for recursive move effects" $ do
            size <- timeout 5000000 (evaluate (Text.length (Schema.renderPlainSchemaViaCodec @GameState)))
            assertBool "Expected the complete game schema to render" (maybe False (> 0) size),
          testCase "Evaluate arithmetic, rounding and conditional rules" $ do
            assertBool "Expected game-defined damage" (Formula.evaluate (Map.fromList [("power", 30), ("attack", 15), ("defense", 8)]) (Formula "max(1, floor(power * attack / defense / 5))") == Right 11)
            assertBool "Expected lazy conditionals and comparisons" (Formula.evaluate Map.empty (Formula "if(2 >= 1, 2^3 + ceil(1.2), 1/0)") == Right 10)
            assertBool "Expected exponentiation before negation" (Formula.evaluate Map.empty (Formula "-2^2 + 2^-2 + 2^3^2") == Right 508.25)
            assertBool "Expected a finite rule result" (isLeft (Formula.evaluate Map.empty (Formula "1/0")))
            assertBool "Expected unknown variables to fail" (isLeft (Formula.evaluate Map.empty (Formula "missing + 1"))),
          testCase "Round trip a game-defined ruleset" $
            let rules = defaultRules{rulesDamageFormula = Formula "floor(power + level)", rulesTieBreak = "random", rulesMaxKnownMoves = 6, rulesTypeChart = Map.singleton "fire" (Map.singleton "grass" 2)}
             in assertBool "Expected rules to be game data" (Aeson.eitherDecode (Aeson.encode rules) == Right rules),
          testCase "Require explicit health and stat formulas" $ do
            assertBool "Expected healthFormula to be required" (isLeft (Aeson.eitherDecode @Rules "{\"statFormula\":\"base\"}"))
            assertBool "Expected statFormula to be required" (isLeft (Aeson.eitherDecode @Rules "{\"healthFormula\":\"base\"}")),
          testCase "Reject unknown policy values and formula variables" $ do
            assertBool "Expected policy validation" (isLeft (Aeson.eitherDecode @Rules "{\"tieBreak\":\"missing\"}"))
            assertBool "Expected rule variable validation" (isLeft (Aeson.eitherDecode @Rules "{\"damageFormula\":\"missing + 1\"}")),
          testCase "Reject malformed expressions and invalid function arity" $ do
            assertBool "Expected malformed expressions to fail decoding" (isLeft (Aeson.eitherDecode @Formula "\"floor(\""))
            assertBool "Expected arity validation" (isLeft (Aeson.eitherDecode @Formula "\"min(1)\""))
            assertBool "Expected variable discovery" (Formula.variables (Formula "max(attack, attack / defense)") == Right ["attack", "defense"])
        ],
      testGroup
        "Moves"
        [ testCase "Decode an independently written move type and defaults" $
            assertBool "Expected configured move type and default targeting rules" (Aeson.eitherDecode @Move "{\"name\":\"Basic strike\",\"power\":20,\"type\":\"fire\"}" == Right defaultMove{moveType = "fire"}),
          testCase "Round trip conditional move effects" $
            let move = defaultMove{moveEffects = [Chance 0.25 (InflictStatus Opponent "slow"), ChangeHealth Self (Formula "damage / 2"), ChangeStage Opponent "defense" (-1)]}
             in assertBool "Expected all authored effects to survive serialization" (Aeson.eitherDecode (Aeson.encode move) == Right move),
          testCase "Reject invalid probabilities, resources and hit counts" $
            mapM_
              (\encoded -> assertBool "Expected invalid move data to fail" (isLeft (Aeson.eitherDecode @Move encoded)))
              ["{\"name\":\"x\",\"accuracy\":2}", "{\"name\":\"x\",\"maxUses\":-1}", "{\"name\":\"x\",\"minHits\":2,\"maxHits\":1}", "{\"name\":\"x\",\"effects\":[{\"type\":\"chance\",\"probability\":-1,\"effect\":{\"type\":\"clear-status\"}}]}"]
        ]
    ]

turnTests :: TestTree
turnTests =
  testGroup
    "Turns"
    [ testCase "Apply both moves, consume uses, then skip a fainted opponent" $
        let opened = start fixture
            first = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
            second = foldl' Engine.applyEvent first (Turn.resolveAction first (UseMove (MoveId "strike")))
         in assertBool "Expected ordered damage and victory without a second retaliation" (map hitPoints (members (party first)) == [15] && map hitPoints (members (party second)) == [15] && fmap battleOutcome (battle second) == Just (Just Victory) && fmap battleTurn (battle second) == Just 2 && map (profileRemainingUses . battleProfile) (members (party first)) == [Map.singleton (MoveId "strike") 4]),
      testCase "Use configured speed ordering and require a free replacement" $
        let faster = defaultSpecies{speciesLearnset = Map.singleton 1 [MoveId "strike"], speciesStats = BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("speed", 50)])}
            configured = fixture{combat = (combat fixture){configSpecies = Map.insert "stonekin" faster (configSpecies (combat fixture))}, party = Party [basicCritter 3 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
            opened = start configured
            fainted = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
            replacement = foldl' Engine.applyEvent fainted (Turn.resolveAction fainted (SwitchCritter 1))
         in assertBool "Expected the fainted actor to skip its move and replacement to consume no turn" (fmap battlePhase (battle fainted) == Just ReplacementMenu && null (Turn.resolveAction fainted (UseMove (MoveId "strike")))) >> assertBool "Expected the new critter intact" (fmap battleActivePlayer (battle replacement) == Just 1 && fmap battleTurn (battle replacement) == Just 1 && map hitPoints (members (party replacement)) == [0, 10]),
      testCase "A voluntary switch consumes a turn" $
        let opened = start fixture{party = Party [basicCritter 20 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (SwitchCritter 1))
         in assertBool "Expected retaliation against the switched-in critter" (map hitPoints (members (party result)) == [20, 5] && fmap battleTurn (battle result) == Just 1),
      testCase "Complete every battle outcome automatically after its final dialogue" $
        forM_ [(Victory, UseMove (MoveId "strike")), (Defeat, UseMove (MoveId "strike")), (Escaped, EscapeBattle), (Captured, UseItem "capsule" (-1))] $ \(outcome, action) -> do
          let completedFlag = FlagId "practice.completed"
              effects = [SetFlag completedFlag True]
              definition = defaultDefinition{definitionAllowCapture = True, definitionAllowEscape = True, definitionCaptureEffects = effects, definitionDefeatEffects = effects, definitionEscapeEffects = effects, definitionOpponents = [basicCritter 5 2 10 "stonekin"], definitionVictoryEffects = effects}
              configured = fixture{combat = (combat fixture){configBattles = Map.singleton "practice" definition, configItems = Map.singleton "capsule" defaultItem{itemCaptureBonus = 1}, configRules = (configRules (combat fixture)){rulesCaptureFormula = Formula "1", rulesEscapeFormula = Formula "1"}}, inventory = Map.singleton "capsule" 1, party = Party [basicCritter (if outcome == Defeat then 3 else 20) 2 20 "leafling"]}
              opened = start configured
              -- Survive the player's hit so the opponent can cause defeat.
              ready = if outcome == Defeat then opened{battle = fmap (\active -> active{battleOpponentParty = [basicCritter 10 2 10 "stonekin"]}) (battle opened)} else opened
              turn = Turn.resolveAction ready action
              settled = foldl' Engine.applyEvent ready turn
              events = Engine.eventsFor ready (Command.ActInBattle action)
              closed = Save.apply ready events
              states = scanl Engine.applyEvent ready turn
              sentences = snd $ foldl' (\(seen, messages) (event, before, after) -> (maybe 0 (length . battleMessages) (battle after), messages <> TUI.eventMessages seen event before after)) (0, []) (zip3 turn states (drop 1 states))
          assertBool "Expected the complete turn before closure and exactly one application of outcome effects" $
            fmap battleOutcome (battle settled) == Just (Just outcome)
              && events == turn <> [Event.BattleEnded, Event.FlagChanged completedFlag True]
              && closed == settled{battle = Nothing, flags = Map.insert completedFlag True (flags settled)}
              && not (null sentences)
              && not (Text.isInfixOf "Continue" (TUI.render settled))
              && playerPosition closed == playerPosition ready,
      testCase "Battle menus support arrows, j/k and Escape without Back entries" $ do
        let opened = start fixture{inventory = Map.singleton "tonic" 1, party = Party [basicCritter 20 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
            (next, _) = Engine.step opened (Input.Character 'j')
            (previous, _) = Engine.step next (Input.Character 'k')
            (moves, _) = Engine.step previous Input.Enter
        assertBool "Expected context-sensitive menu commands" (fmap battleSelected (battle next) == Just 2 && fmap battleSelected (battle previous) == Just 0 && fmap battlePhase (battle moves) == Just MoveMenu && Engine.commandFor opened (Input.Character 'p') == Nothing)
        forM_ [ItemMenu, MoveMenu, SwitchMenu] $ \phase -> do
          let state = opened{battle = fmap (\active -> active{battlePhase = phase}) (battle opened)}
              (selected, _) = Engine.step state Input.Down
              (actions, _) = Engine.step selected Input.Escape
          assertBool "Expected selection to follow the menu layout and Escape to return to actions" (fmap battleSelected (battle selected) == Just (if phase == SwitchMenu then 1 else 0) && fmap battlePhase (battle actions) == Just ActionMenu),
      testCase "Selecting the active critter returns to actions without consuming a turn" $
        forM_ [0, 2] $ \index ->
          let opened = start fixture{party = Party [basicCritter 20 2 20 "leafling", basicCritter 0 2 10 "leafling", basicCritter 10 2 10 "leafling"]}
              choosing = opened{battle = fmap (\active -> active{battleActivePlayer = index, battlePhase = SwitchMenu, battleSelected = index}) (battle opened)}
              (confirmed, events) = Engine.step choosing Input.Enter
              expected = choosing{battle = fmap (\active -> active{battlePhase = ActionMenu, battleSelected = 0}) (battle choosing)}
           in assertBool "Expected only the menu phase and selection to change, with replayable events" $
                confirmed == expected && foldl' Engine.applyEvent choosing events == expected,
      testCase "Escape preserves Items selection in the action menu" $
        let opened = start fixture
            (selected, _) = Engine.step opened Input.Down
         in assertBool "Expected Items to remain selected with no state changes or events" $
              fmap battleSelected (battle selected) == Just 2
                && Engine.step selected Input.Escape == (selected, []),
      testCase "Navigate action menus by their displayed rows and columns"
        $ forM_
          [ (False, 0, Input.Down, 2),
            (False, 2, Input.Up, 0),
            (False, 0, Input.Right, 1),
            (False, 1, Input.Left, 0),
            (False, 1, Input.Down, 1),
            (False, 2, Input.Right, 2),
            (False, 2, Input.Left, 2),
            (False, 1, Input.Right, 1),
            (True, 1, Input.Down, 3),
            (True, 3, Input.Up, 1),
            (True, 2, Input.Right, 3),
            (True, 3, Input.Left, 2),
            (True, 0, Input.Character 'j', 2),
            (True, 2, Input.Character 'k', 0),
            (True, 2, Input.Character 'l', 3),
            (True, 3, Input.Character 'h', 2)
          ]
        $ \(allowEscape, index, input, expected) ->
          let opened = start fixture
              state = opened{battle = fmap (\active -> active{battleDefinition = (battleDefinition active){definitionAllowEscape = allowEscape}, battleSelected = index}) (battle opened)}
              (selected, _) = Engine.step state input
           in assertBool "Expected grid navigation to change only the selected battle entry" $
                selected == state{battle = fmap (\active -> active{battleSelected = expected}) (battle state)},
      testCase "Keep party selection vertical in every battle party menu" $
        forM_ [ItemTargetMenu, ReplacementMenu, SwitchMenu] $ \phase ->
          forM_ [(Input.Down, 3), (Input.Up, 1), (Input.Left, 2), (Input.Right, 2), (Input.Character 'j', 3), (Input.Character 'k', 1), (Input.Character 'h', 2), (Input.Character 'l', 2)] $ \(input, expected) ->
            let opened = start fixture{party = Party (replicate 6 (basicCritter 10 2 10 "leafling"))}
                state = opened{battle = fmap (\active -> active{battlePhase = phase, battleSelected = 2}) (battle opened)}
                (selected, _) = Engine.step state input
             in assertBool "Expected Up/Down to move one entry and Left/Right to stay put" $
                  selected == state{battle = fmap (\active -> active{battleSelected = expected}) (battle state)}
    ]

variationTests :: TestTree
variationTests =
  testGroup
    "Battle variation"
    [ testCase "A miss consumes resources without dealing damage" $
        let configured = fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveAccuracy = 0}) (MoveId "strike") (configMoves (combat fixture))}}
            opened = start configured
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
         in assertBool "Expected both misses and spent uses" (map hitPoints (members (party result)) == [20] && fmap (map hitPoints . battleOpponentParty) (battle result) == Just [10] && map (profileRemainingUses . battleProfile) (members (party result)) == [Map.singleton (MoveId "strike") 4]),
      testCase "Configure critical damage" $
        let configured = fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveCriticalRate = 1}) (MoveId "strike") (configMoves (combat fixture)), configRules = defaultRules{rulesDamageFormula = Formula "power * critical"}}}
            opened = start configured
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
         in assertBool "Expected a configured critical victory" (fmap battleOutcome (battle result) == Just (Just Victory) && map hitPoints (members (party result)) == [20]),
      testCase "Respect authored type immunity and weighted multi-hit counts" $ do
        let immuneSpecies = defaultSpecies{speciesTypes = ["rock"], speciesLearnset = Map.singleton 1 [MoveId "strike"]}
            configured = fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveType = "grass"}) (MoveId "strike") (configMoves (combat fixture)), configSpecies = Map.insert "stonekin" immuneSpecies (configSpecies (combat fixture)), configRules = (configRules (combat fixture)){rulesTypeChart = Map.singleton "grass" (Map.singleton "rock" 0)}}}
            opened = start configured
            immune = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
            multiple = start fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveMinHits = 2, moveMaxHits = 2, moveHitWeights = Map.singleton 2 1}) (MoveId "strike") (configMoves (combat fixture))}}
            result = foldl' Engine.applyEvent multiple (Turn.resolveAction multiple (UseMove (MoveId "strike")))
        assertBool "Expected an immunity to prevent all damage" (fmap (map hitPoints . battleOpponentParty) (battle immune) == Just [10])
        assertBool "Expected two hits with only one use consumed" (fmap battleOutcome (battle result) == Just (Just Victory) && map (profileRemainingUses . battleProfile) (members (party result)) == [Map.singleton (MoveId "strike") 4]),
      testCase "Exhausted moves fall back to the configured unlimited move" $
        let exhausted = (basicCritter 20 2 20 "leafling"){battleProfile = defaultProfile{profileRemainingUses = Map.singleton (MoveId "strike") 0}}
            opened = start fixture{party = Party [exhausted]}
            result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "")))
         in assertBool "Expected a usable fallback and no exhausted move action" (null (Turn.resolveAction opened (UseMove (MoveId "strike"))) && fmap battleOutcome (battle result) == Just (Just Victory)),
      testCase "Record random results and reproduce them by replay without rerolling" $
        let configured = fixture{combat = (combat fixture){configRules = defaultRules{rulesTieBreak = "random", rulesRandomDamageMin = 0.8, rulesRandomDamageMax = 1.2}}}
            opened = start configured
            facts = Turn.resolveAction opened (UseMove (MoveId "strike"))
            result = foldl' Engine.applyEvent opened facts
            replayed = foldl' Engine.applyEvent opened{randomSeed = 999} facts
         in assertBool "Expected reproducible sampling and recorded random state" (facts == Turn.resolveAction opened (UseMove (MoveId "strike")) && randomSeed result /= randomSeed opened && party replayed == party result && battle replayed == battle result && randomSeed replayed == randomSeed result)
    ]

viewTests :: TestTree
viewTests =
  testGroup
    "Battle view"
    [ testCase "Render both combatants and selectable moves in a fixed viewport" $
        let opened = start fixture
            (moves, _) = Engine.step opened Input.Enter
            frame = TUI.renderFrame moves
         in assertBool "Expected health and moves within the terminal viewport" (length frame == TUI.height && all ((== TUI.width * 2) . sum . map Cell.columns) frame && all (\text -> Text.isInfixOf text (TUI.render moves)) ["Stonekin", "Leafling", "HP 20/20", "> Basic strike"]),
      testCase "Display combatants without prefixes and name the defeated opponent" $
        forM_ ["Trainer Fern", "Guide Moss"] $ \opponent ->
          let configured = fixture{combat = (combat fixture){configSpecies = Map.adjust (\definition -> definition{speciesName = "LeafLING"}) "leafling" (configSpecies (combat fixture))}}
              opened = start configured
              won = opened{battle = fmap (\active -> active{battleDefinition = (battleDefinition active){definitionName = opponent}, battleMessages = ["Victory."], battleOutcome = Just Victory, battlePhase = Finished}) (battle opened)}
              drawing = map (Text.stripEnd . Text.concat . map Cell.glyph) (TUI.renderFrame won)
              victory = Text.concat [Cell.glyph cell | frame <- TUI.dialogueFrames 1 "Victory." opened won, row <- frame, cell <- row]
              choosing = opened{battle = fmap (\active -> active{battlePhase = SwitchMenu}) (battle opened)}
           in assertBool "Expected configured capitalization, capitalized fallback names and an opponent-specific victory message" $
                take 1 drawing == [opponent]
                  && drawing !! 2 == "Stonekin  Lv 2  HP 10/10"
                  && drawing !! 4 == "LeafLING  Lv 2  HP 20/20"
                  && Text.isInfixOf ("You defeated " <> opponent) victory
                  && not (any (\prefix -> Text.isInfixOf prefix (Text.concat drawing)) ["Battle —", "Opponent:", "You:", "Victory."])
                  && Text.isInfixOf "> LeafLING  HP 20/20" (TUI.render choosing)
                  && Text.isInfixOf "1. LeafLING  Lv 2  HP 20/20" (TUI.render opened{battle = Nothing, partyOpen = True}),
      testCase "Capitalize battle messages without changing their remaining casing" $
        forM_ [("defense stage changed.", "Defense stage changed."), ("specialDefense stage changed.", "SpecialDefense stage changed."), ("recovered 2 HP with X-Ray.", "Recovered 2 HP with X-Ray."), ("", ""), ("HP remains 10/20.", "HP remains 10/20.")] $ \(message, expected) ->
          let opened = start fixture
              frames = TUI.dialogueFrames 1 message opened opened
              dialogue frame = map (Text.strip . Text.dropEnd 1 . Text.drop 1 . Text.concat . map Cell.glyph) (take 2 (drop (TUI.height - 3) frame))
           in assertBool "Expected sentence capitals while preserving identifiers, acronyms, names and empty messages" $
                map dialogue frames == [[expected, ""]],
      testCase "Use the bottom option box for actions, moves and items"
        $ forM_
          [ (ActionMenu, [optionRow " > Moves" "   Party", optionRow "   Items" ""]),
            (MoveMenu, [optionRow " > Basic strike  5/5" "", optionRow "" ""]),
            (ItemMenu, [optionRow " > tonic x1" "", optionRow "" ""])
          ]
        $ \(phase, options) ->
          let opened = start fixture{inventory = Map.singleton "tonic" 1}
              state = opened{battle = fmap (\current -> current{battlePhase = phase}) (battle opened)}
              frame = TUI.renderFrame state
              drawing = map (Text.concat . map Cell.glyph) frame
              box = border "┌" "┐" : options <> [border "└" "┘"]
           in assertBool "Expected the correct options within a bottom-aligned border without help text" $
                length frame == TUI.height
                  && all ((== TUI.width * 2) . sum . map Cell.columns) frame
                  && drop (TUI.height - length box) drawing == box
                  && all (\hint -> not (Text.isInfixOf hint (Text.concat drawing))) ["Choose an action:", "Choose a move:", "Choose an item:", "Battle complete:", "Select  Enter:", "Enter: Confirm", "Enter: Continue", "q: Quit"],
      testCase "Keep an empty item menu blank and allow Escape" $
        let opened = start fixture
            items = opened{battle = fmap (\active -> active{battlePhase = ItemMenu}) (battle opened)}
            (confirmed, events) = Engine.step items Input.Enter
            (actions, _) = Engine.step confirmed Input.Escape
            drawing = map (Text.concat . map Cell.glyph) (TUI.renderFrame confirmed)
         in assertBool "Expected an empty two-row box, no confirmation effects and Escape navigation" $
              drop (TUI.height - 4) drawing == [border "┌" "┐", optionRow "" "", optionRow "" "", border "└" "┘"]
                && confirmed == items
                && null events
                && fmap battlePhase (battle actions) == Just ActionMenu,
      testCase "Show the active critter when it is the only party member" $
        let opened = start fixture
            (partySelected, _) = Engine.step opened Input.Right
            (choosing, _) = Engine.step partySelected Input.Enter
            drawing = map (Text.concat . map Cell.glyph) (TUI.renderFrame choosing)
         in assertBool "Expected a visible active critter and confirmation to return to actions" $
              fmap battlePhase (battle choosing) == Just SwitchMenu
                && drawing !! 11 == Text.justifyLeft (TUI.width * 2) ' ' "> Leafling  HP 20/20"
                && fst (Engine.step choosing Input.Enter) == opened,
      testCase "List the whole switch party in order while preserving switch validation" $ do
        let opened = start fixture{party = Party [basicCritter 20 2 20 "leafling", basicCritter 0 2 10 "leafling", basicCritter 10 2 10 "leafling"]}
            choosing = opened{battle = fmap (\active -> active{battlePhase = SwitchMenu}) (battle opened)}
            drawing = map (Text.concat . map Cell.glyph) (TUI.renderFrame choosing)
        assertBool "Expected active, fainted and available critters in party order" $
          take 3 (drop 11 drawing) == map (Text.justifyLeft (TUI.width * 2) ' ') ["> Leafling  HP 20/20", "  Leafling  HP 0/10", "  Leafling  HP 10/10"]
        let fainted = choosing{battle = fmap (\active -> active{battleSelected = 1}) (battle choosing)}
        assertBool "Expected no state change or turn when selecting a fainted critter" $
          Engine.step fainted Input.Enter == (fainted, [])
        let selected = choosing{battle = fmap (\active -> active{battleSelected = 2}) (battle choosing)}
            (switched, _) = Engine.step selected Input.Enter
            replacement = choosing{battle = fmap (\active -> active{battleActivePlayer = 1, battlePhase = ReplacementMenu}) (battle choosing)}
            replacementRows = map (Text.concat . map Cell.glyph) (TUI.renderFrame replacement)
        assertBool "Expected a valid choice to switch using its original party index and consume one turn" $
          fmap battleActivePlayer (battle switched) == Just 2 && fmap battleTurn (battle switched) == Just 1
        assertBool "Expected forced replacement to retain only eligible critters" $
          take 3 (drop 11 replacementRows) == map (Text.justifyLeft (TUI.width * 2) ' ') ["> Leafling  HP 20/20", "  Leafling  HP 10/10", ""],
      testCase "Keep switching, replacement and item targets as party lists" $
        forM_ [ItemTargetMenu, ReplacementMenu, SwitchMenu] $ \phase ->
          let opened = start fixture{party = Party (replicate 6 (basicCritter 10 2 10 "leafling"))}
              state = opened{battle = fmap (\current -> current{battlePhase = phase}) (battle opened)}
              frame = TUI.renderFrame state
              drawing = map (Text.concat . map Cell.glyph) frame
           in assertBool "Expected full-width party entries without an option box" $
                length frame == TUI.height
                  && drawing !! 11 == Text.justifyLeft (TUI.width * 2) ' ' "> Leafling  HP 10/10"
                  && Text.count "Leafling  HP" (Text.concat drawing) == (if phase == ReplacementMenu then 5 else 6)
                  && not (any (Text.isPrefixOf "  Back") drawing)
                  && all (\hint -> not (Text.isInfixOf hint (Text.concat drawing))) ["Choose a critter:", "Choose a target:", "Choose a replacement:", "Select  Enter:", "Enter: Switch", "Esc: Back", "q: Quit"]
                  && not (Text.isInfixOf "┌" (Text.concat drawing)),
      testCase "Keep selected entries visible for large parties" $
        let configured = fixture{maxPartySize = 40, party = Party (replicate 40 (basicCritter 10 2 10 "leafling"))}
            opened = start configured
            active = fmap (\current -> current{battlePhase = SwitchMenu, battleSelected = 35}) (battle opened)
            drawing = TUI.render opened{battle = active}
         in assertBool "Expected the selected entry to remain visible without a help footer" (Text.isInfixOf "> Leafling" drawing && not (Text.isInfixOf "Esc: Back" drawing))
    ]
  where
    border left right = left <> Text.replicate (TUI.width * 2 - 2) "─" <> right
    optionRow left right = "│" <> Text.justifyLeft (TUI.width - 1) ' ' left <> Text.justifyLeft (TUI.width - 1) ' ' right <> "│"
