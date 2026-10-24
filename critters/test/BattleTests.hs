module BattleTests (tests) where

import Critters.Engine qualified as Engine
import Critters.Event qualified as Event
import Critters.Formula (Formula (..))
import Critters.Formula qualified as Formula
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition (BattleDefinition (..), defaultDefinition)
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.CombatConfig (CombatConfig (..), defaultCombat)
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.GameState (GameState (..), initialState)
import Critters.State.Move (Move (..), defaultMove)
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Rules (Rules (..), defaultRules)
import Critters.State.Species (Species (..), defaultSpecies)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

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

tests :: TestTree
tests = testGroup "Battle"
  [ profileTests, snapshotTests,
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
