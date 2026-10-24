module BattleTests (tests) where

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
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition)
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.CombatConfig (CombatConfig (..), defaultCombat)
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.Effect (Effect (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..), initialState)
import Critters.State.Item (Item (..), defaultItem)
import Critters.State.Move (Move (..), defaultMove)
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules (Rules (..), defaultRules)
import Critters.State.Species (Species (..), defaultSpecies)
import Critters.State.StatusDefinition (StatusDefinition (..), defaultStatus)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

fixture :: GameState
fixture = initialState
  { combat = defaultCombat
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

lifecycleTests :: TestTree
lifecycleTests = testGroup "Battle entry"
  [ testCase "Start a configured battle and block exploration" $
      case BattleEngine.beginBattle fixture "practice" of
        Left _ -> assertBool "Expected a valid battle" False
        Right active ->
          let opened = Engine.applyEvent fixture (Event.BattleStarted active)
           in assertBool "Expected a configuration snapshot and an isolated battle context" (battleConfig active == combat fixture && Engine.step opened Input.Right == (opened, []) && Engine.eventsFor opened (Command.Walk East) == []),
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

profileTests :: TestTree
profileTests = testGroup "Combat profiles"
  [ testCase "Preserve individual experience, moves, resources and status" $
      let critter = (basicCritter 8 2 10 "leafling"){battleProfile = defaultProfile{profileExperience = 15, profileKnownMoves = [MoveId "strike"], profileRemainingUses = Map.singleton (MoveId "strike") 2, profileStatus = Just (Affliction (Just 3) "slow")}}
       in assertBool "Expected a complete individual combat profile" (Aeson.eitherDecode (Aeson.encode critter) == Right critter),
    testCase "Load authored species stats and level-based moves" $
      let definition = defaultSpecies{speciesLearnset = Map.singleton 3 [MoveId "strike"], speciesStats = Map.singleton "focus" 12}
       in assertBool "Expected author-defined stats and moves" (Aeson.eitherDecode (Aeson.encode definition) == Right definition),
    testCase "Reject invalid progression, duplicate moves and statuses" $ do
      assertBool "Expected nonnegative experience" (isLeft (Aeson.eitherDecode @BattleProfile "{\"experience\":-1}"))
      assertBool "Expected unique learned moves" (isLeft (Aeson.eitherDecode @BattleProfile "{\"knownMoves\":[\"strike\",\"strike\"]}"))
      assertBool "Expected positive status duration" (isLeft (Aeson.eitherDecode @Affliction "{\"status\":\"slow\",\"remaining\":0}"))
      assertBool "Expected positive species stats" (isLeft (Aeson.eitherDecode @Species "{\"stats\":{\"attack\":0}}"))
  ]

snapshotTests :: TestTree
snapshotTests = testGroup "Battle snapshots"
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
      assertBool "Expected nonnegative inventory counts" (isLeft (Aeson.eitherDecode @GameState "{\"version\":1,\"maps\":{},\"playerPosition\":{\"x\":0,\"y\":0},\"inventory\":{\"tonic\":-1}}"))
  ]

statusTests :: TestTree
statusTests = testGroup "Status and move effects"
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
tests = testGroup "Battle"
  [ lifecycleTests, profileTests, snapshotTests, statusTests, turnTests, variationTests, viewTests,
    testGroup "Rules"
      [ testCase "Evaluate arithmetic, rounding and conditional rules" $ do
          assertBool "Expected game-defined damage" (Formula.evaluate (Map.fromList [("power", 30), ("attack", 15), ("defense", 8)]) (Formula "max(1, floor(power * attack / defense / 5))") == Right 11)
          assertBool "Expected lazy conditionals and comparisons" (Formula.evaluate Map.empty (Formula "if(2 >= 1, 2^3 + ceil(1.2), 1/0)") == Right 10)
          assertBool "Expected a finite rule result" (isLeft (Formula.evaluate Map.empty (Formula "1/0")))
          assertBool "Expected unknown variables to fail" (isLeft (Formula.evaluate Map.empty (Formula "missing + 1"))),
        testCase "Round trip a game-defined ruleset" $
          let rules = defaultRules{rulesDamageFormula = Formula "floor(power + level)", rulesTieBreak = "random", rulesMaxKnownMoves = 6}
           in assertBool "Expected rules to be game data" (Aeson.eitherDecode (Aeson.encode rules) == Right rules),
        testCase "Reject unknown policy values and formula variables" $ do
          assertBool "Expected policy validation" (isLeft (Aeson.eitherDecode @Rules "{\"tieBreak\":\"missing\"}"))
          assertBool "Expected rule variable validation" (isLeft (Aeson.eitherDecode @Rules "{\"damageFormula\":\"missing + 1\"}")),
        testCase "Reject malformed expressions and invalid function arity" $ do
          assertBool "Expected malformed expressions to fail decoding" (isLeft (Aeson.eitherDecode @Formula "\"floor(\""))
          assertBool "Expected arity validation" (isLeft (Aeson.eitherDecode @Formula "\"min(1)\""))
          assertBool "Expected variable discovery" (Formula.variables (Formula "max(attack, attack / defense)") == Right ["attack", "defense"])
      ],
    testGroup "Moves"
      [ testCase "Decode an independently written move and defaults" $
          assertBool "Expected configured power and default targeting rules" (Aeson.eitherDecode @Move "{\"name\":\"Basic strike\",\"power\":20}" == Right defaultMove),
        testCase "Round trip conditional move effects" $
          let move = defaultMove{moveEffects = [Chance 0.25 (InflictStatus Opponent "slow"), ChangeHealth Self (Formula "damage / 2"), ChangeStage Opponent "defense" (-1)]}
           in assertBool "Expected all authored effects to survive serialization" (Aeson.eitherDecode (Aeson.encode move) == Right move),
        testCase "Reject invalid probabilities, resources and hit counts" $
          mapM_ (\encoded -> assertBool "Expected invalid move data to fail" (isLeft (Aeson.eitherDecode @Move encoded)))
            ["{\"name\":\"x\",\"accuracy\":2}", "{\"name\":\"x\",\"maxUses\":-1}", "{\"name\":\"x\",\"minHits\":2,\"maxHits\":1}", "{\"name\":\"x\",\"effects\":[{\"type\":\"chance\",\"probability\":-1,\"effect\":{\"type\":\"clear-status\"}}]}"]
      ]
  ]

turnTests :: TestTree
turnTests = testGroup "Turns"
  [ testCase "Apply both moves, consume uses, then skip a fainted opponent" $
      let opened = start fixture
          first = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
          second = foldl' Engine.applyEvent first (Turn.resolveAction first (UseMove (MoveId "strike")))
       in assertBool "Expected ordered damage and victory without a second retaliation" (map hitPoints (members (party first)) == [15] && map hitPoints (members (party second)) == [15] && fmap battleOutcome (battle second) == Just (Just Victory) && fmap battleTurn (battle second) == Just 2 && map (profileRemainingUses . battleProfile) (members (party first)) == [Map.singleton (MoveId "strike") 4]),
    testCase "Use configured speed ordering and require a free replacement" $
      let faster = defaultSpecies{speciesLearnset = Map.singleton 1 [MoveId "strike"], speciesStats = Map.fromList [("attack", 10), ("defense", 10), ("speed", 50)]}
          configured = fixture{combat = (combat fixture){configSpecies = Map.insert "stonekin" faster (configSpecies (combat fixture))}, party = Party [basicCritter 3 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
          opened = start configured
          fainted = foldl' Engine.applyEvent opened (Turn.resolveAction opened (UseMove (MoveId "strike")))
          replacement = foldl' Engine.applyEvent fainted (Turn.resolveAction fainted (SwitchCritter 1))
       in assertBool "Expected the fainted actor to skip its move and replacement to consume no turn" (fmap battlePhase (battle fainted) == Just ReplacementMenu && null (Turn.resolveAction fainted (UseMove (MoveId "strike")))) >> assertBool "Expected the new critter intact" (fmap battleActivePlayer (battle replacement) == Just 1 && fmap battleTurn (battle replacement) == Just 1 && map hitPoints (members (party replacement)) == [0,10]),
    testCase "A voluntary switch consumes a turn" $
      let opened = start fixture{party = Party [basicCritter 20 2 20 "leafling", basicCritter 10 2 10 "leafling"]}
          result = foldl' Engine.applyEvent opened (Turn.resolveAction opened (SwitchCritter 1))
       in assertBool "Expected retaliation against the switched-in critter" (map hitPoints (members (party result)) == [20,5] && fmap battleTurn (battle result) == Just 1),
    testCase "Apply outcome effects only after acknowledgement" $
      let configured = fixture{combat = (combat fixture){configBattles = Map.adjust (\definition -> definition{definitionVictoryEffects = [SetFlag (FlagId "practice.won") True]}) "practice" (configBattles (combat fixture))}}
          opened = start configured
          won = iterate (\state -> foldl' Engine.applyEvent state (Turn.resolveAction state (UseMove (MoveId "strike")))) opened !! 2
          (closed, _) = Engine.step won Input.Enter
       in assertBool "Expected a visible outcome, then story effects and exploration" (Map.lookup (FlagId "practice.won") (flags won) == Nothing && battle closed == Nothing && Map.lookup (FlagId "practice.won") (flags closed) == Just True && playerPosition closed == playerPosition configured),
    testCase "Battle menus support arrows, j/k and cancellation" $
      let opened = start fixture
          (moves, _) = Engine.step opened Input.Enter
          (back, _) = Engine.step moves (Input.Character 'j')
          (actions, _) = Engine.step back Input.Enter
       in assertBool "Expected context-sensitive menu commands" (fmap battlePhase (battle moves) == Just MoveMenu && fmap battlePhase (battle actions) == Just ActionMenu && Engine.commandFor opened (Input.Character 'p') == Nothing)
  ]

variationTests :: TestTree
variationTests = testGroup "Battle variation"
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
    testCase "Respect authored affinity immunity and weighted multi-hit counts" $ do
      let immuneSpecies = defaultSpecies{speciesAffinities = ["stone"], speciesLearnset = Map.singleton 1 [MoveId "strike"]}
          configured = fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveAffinity = "leaf"}) (MoveId "strike") (configMoves (combat fixture)), configSpecies = Map.insert "stonekin" immuneSpecies (configSpecies (combat fixture)), configRules = (configRules (combat fixture)){rulesAffinityChart = Map.singleton "leaf" (Map.singleton "stone" 0)}}}
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
viewTests = testGroup "Battle view"
  [ testCase "Render both combatants and selectable moves in a fixed viewport" $
      let opened = start fixture
          (moves, _) = Engine.step opened Input.Enter
          frame = TUI.renderFrame moves
       in assertBool "Expected health, moves and controls within the terminal viewport" (length frame == TUI.height && all ((== TUI.width * 2) . sum . map Cell.columns) frame && all (\text -> Text.isInfixOf text (TUI.render moves)) ["Opponent: stonekin", "You: leafling", "HP 20/20", "> Basic strike", "Enter: Confirm"]),
    testCase "Keep selected entries visible for large parties" $
      let configured = fixture{maxPartySize = 40, party = Party (replicate 40 (basicCritter 10 2 10 "leafling"))}
          opened = start configured
          active = fmap (\current -> current{battlePhase = SwitchMenu, battleSelected = 35}) (battle opened)
          drawing = TUI.render opened{battle = active}
       in assertBool "Expected the selected entry and footer to remain visible" (Text.isInfixOf "> leafling" drawing && Text.isInfixOf "Esc: Back" drawing)
  ]
