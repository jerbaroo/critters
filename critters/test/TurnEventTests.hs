module TurnEventTests (tests) where

import BattleTests (fixture, start)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event (..))
import Critters.State.Affliction (Affliction (..))
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition (BattleDefinition (..))
import Critters.State.BattleOutcome (BattleOutcome (..))
import Critters.State.BattleProfile (BattleProfile (..), defaultProfile)
import Critters.State.BattleSide (BattleSide (..))
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.Critter (Critter (..), basicCritter)
import Critters.State.GameState (GameState (..))
import Critters.State.Move (Move (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Critters.State.Species (BaseStats (..), Species (..), defaultSpecies)
import Critters.State.StatusDefinition (StatusDefinition (..), defaultStatus)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Turn events"
    [ testCase "Record both strikes separately before completing the turn" $
        let opened = start fixture
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            replayed = drop 1 (scanl Engine.applyEvent opened events)
         in assertBool "Expected independent health changes, resource use and a final completed turn" $
              case (events, replayed) of
                ([MoveUsed Player first _, MoveUsed Enemy second _, TurnCompleted _], [attacked, retaliated, completed]) ->
                  first == MoveId "strike"
                    && second == MoveId "strike"
                    && playerHealth attacked == [20]
                    && enemyHealth attacked == Just [5]
                    && playerHealth retaliated == [15]
                    && enemyHealth retaliated == Just [5]
                    && fmap battleTurn (battle attacked) == Just 0
                    && fmap battleTurn (battle retaliated) == Just 0
                    && fmap battleTurn (battle completed) == Just 1
                    && map (Map.lookup (MoveId "strike") . profileRemainingUses . battleProfile) (members (party completed)) == [Just 4]
                _ -> False,
      testCase "Record moves in configured speed order" $
        let opened = start faster
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
         in assertBool "Expected the opponent's move to be first" (actors events == [Enemy, Player]),
      testCase "Keep a fainting hit visible and skip the defeated opponent's action" $
        let opened = start fixture{combat = (combat fixture){configBattles = Map.adjust (\definition -> definition{definitionOpponents = [basicCritter 5 2 10 "stonekin"]}) "practice" (configBattles (combat fixture))}}
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            replayed = drop 1 (scanl Engine.applyEvent opened events)
         in assertBool "Expected zero HP before victory settlement and no retaliation" $
              actors events == [Player]
                && case replayed of
                  hit : _ -> enemyHealth hit == Just [0] && fmap battleOutcome (battle hit) == Just Nothing && fmap battleOutcome (battle (last replayed)) == Just (Just Victory)
                  [] -> False,
      testCase "Keep zero player HP visible before defeat recovery" $
        let opened = start faster{party = Party [basicCritter 3 2 20 "leafling"]}
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            replayed = drop 1 (scanl Engine.applyEvent opened events)
         in assertBool "Expected the attack to reach zero before later recovery restores health" $
              actors events == [Enemy]
                && case replayed of
                  hit : _ -> playerHealth hit == [0] && playerHealth (last replayed) == [20] && fmap battleOutcome (battle (last replayed)) == Just (Just Defeat)
                  [] -> False,
      testCase "Record missed moves as used and consume their uses" $
        let opened = start fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveAccuracy = 0}) (MoveId "strike") (configMoves (combat fixture))}}
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            completed = foldl' Engine.applyEvent opened events
         in assertBool "Expected both move records, no damage and recorded misses" $
              actors events == [Player, Enemy]
                && playerHealth completed == [20]
                && enemyHealth completed == Just [10]
                && fmap (length . filter (== "The move missed.") . battleMessages) (battle completed) == Just 2
                && map (Map.lookup (MoveId "strike") . profileRemainingUses . battleProfile) (members (party completed)) == [Just 4],
      testCase "Record a used move even when its snapshot matches the previous messages" $
        let opened = start fixture{combat = (combat fixture){configMoves = Map.adjust (\move -> move{moveAccuracy = 0, moveMaxUses = 0}) (MoveId "strike") (configMoves (combat fixture))}}
            repeated = opened{battle = fmap (\active -> active{battleMessages = ["leafling used Basic strike.", "The move missed."]}) (battle opened)}
         in assertBool "Expected both moves to be recorded even without health, resource or message changes" $
              actors (Turn.resolveAction repeated (UseMove (MoveId "strike"))) == [Player, Enemy],
      testCase "Do not record a blocked action as a used move" $
        let resting = (basicCritter 20 2 20 "leafling"){battleProfile = defaultProfile{profileStatus = Just (Affliction Nothing "sleep")}}
            opened = start fixture{combat = (combat fixture){configStatuses = Map.singleton "sleep" defaultStatus{statusBlockChance = 1}}, party = Party [resting]}
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            completed = foldl' Engine.applyEvent opened events
         in assertBool "Expected only the opponent's move and no player move use" $
              actors events == [Enemy]
                && map (Map.lookup (MoveId "strike") . profileRemainingUses . battleProfile) (members (party completed)) == [Nothing]
                && maybe False (any (Text.isInfixOf "Unable to act") . battleMessages) (battle completed),
      testCase "Record item healing before the opponent's retaliation" $
        let opened = start fixture{inventory = Map.singleton "tonic" 1, party = Party [basicCritter 10 2 20 "leafling"]}
            events = Turn.resolveAction opened (UseItem "tonic" 0)
            replayed = drop 1 (scanl Engine.applyEvent opened events)
         in assertBool "Expected the item update followed by an independently recorded enemy move" $
              actors events == [Enemy]
                && case replayed of
                  healed : hit : _ -> playerHealth healed == [15] && Map.lookup "tonic" (inventory healed) == Just 0 && playerHealth hit == [10]
                  _ -> False,
      testCase "Keep residual status damage in turn completion" $
        let afflicted = (basicCritter 20 2 20 "leafling"){battleProfile = defaultProfile{profileStatus = Just (Affliction Nothing "poison")}}
            opened = start fixture{combat = (combat fixture){configStatuses = Map.singleton "poison" defaultStatus{statusDamageFraction = 0.25}}, party = Party [afflicted]}
            events = Turn.resolveAction opened (UseMove (MoveId "strike"))
            replayed = drop 1 (scanl Engine.applyEvent opened events)
         in assertBool "Expected ordinary retaliation before the status tick at turn end" $
              case (events, replayed) of
                ([MoveUsed Player _ _, MoveUsed Enemy _ _, TurnCompleted _], [_, hit, completed]) -> playerHealth hit == [15] && playerHealth completed == [10]
                _ -> False,
      testCase "All generated event records survive codecs and reproduce their final state" $
        forM_ [start fixture, start faster, start faster{party = Party [basicCritter 3 2 20 "leafling"]}] $ \opened ->
          let events = Turn.resolveAction opened (UseMove (MoveId "strike"))
           in assertBool "Expected replayable completed turns" $
                Aeson.eitherDecode (Aeson.encode events) == Right events
                  && fmap battleTurn (battle (foldl' Engine.applyEvent opened events)) == Just 1
    ]
  where
    actors events = [side | MoveUsed side _ _ <- events]
    enemyHealth state = map hitPoints . battleOpponentParty <$> battle state
    faster = fixture{combat = (combat fixture){configSpecies = Map.insert "stonekin" defaultSpecies{speciesExperienceYield = 0, speciesLearnset = Map.singleton 1 [MoveId "strike"], speciesStats = BaseStats (Map.fromList [("attack", 10), ("defense", 10), ("speed", 50)])} (configSpecies (combat fixture))}}
    playerHealth = map hitPoints . members . party
