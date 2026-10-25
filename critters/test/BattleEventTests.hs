module BattleEventTests (tests) where

import BattleTests (fixture, start)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Critters.Engine qualified as Engine
import Critters.Event (Event (..))
import Critters.Pretty (pretty)
import Critters.Save qualified as Save
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleSide (BattleSide (..))
import Critters.State.BattleUpdate (BattleUpdate (..))
import Critters.State.CombatConfig (defaultCombat)
import Critters.State.Critter (basicCritter)
import Critters.State.GameState (GameState (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Party (Party (..))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Battle events"
    [ testCase "Decode independently specified move, turn and legacy update events"
        $ forM_
          [ ("{\"type\":\"move-used\",\"side\":\"player\",\"move\":\"strike\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}", MoveUsed Player (MoveId "strike") update),
            ("{\"type\":\"move-used\",\"side\":\"enemy\",\"move\":\"strike\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}", MoveUsed Enemy (MoveId "strike") update),
            ("{\"type\":\"turn-completed\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}", TurnCompleted update),
            ("{\"type\":\"battle-changed\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}", BattleChanged update)
          ]
        $ \(encoded, expected) ->
          assertBool "Expected the documented event fields, sides and resolved update" $
            Aeson.eitherDecode encoded == Right expected
              && Aeson.eitherDecode @Aeson.Value encoded == Right (Aeson.toJSON expected),
      testCase "Reject incomplete move and turn records"
        $ forM_
          [ "{\"type\":\"move-used\",\"move\":\"strike\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}",
            "{\"type\":\"move-used\",\"side\":\"player\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}",
            "{\"type\":\"move-used\",\"side\":\"player\",\"move\":\"strike\"}",
            "{\"type\":\"move-used\",\"side\":\"spectator\",\"move\":\"strike\",\"update\":{\"battle\":{\"opponentParty\":[{\"hitPoints\":7,\"level\":2,\"maxHitPoints\":10,\"species\":\"stonekin\"}]}}}",
            "{\"type\":\"turn-completed\"}"
          ]
        $ \encoded -> assertBool "Expected invalid event records to fail decoding" (isLeft (Aeson.eitherDecode @Event encoded)),
      testCase "Apply move and turn facts in order without revalidating moves or resetting unrelated state" $ do
        let before = fixture{combat = defaultCombat, inventory = Map.singleton "tonic" 2, partyOpen = True, randomSeed = 99}
            attacked = recorded{battleConfig = defaultCombat}
            completed = attacked{battleTurn = 1}
            damagedParty = Party [basicCritter 16 2 20 "leafling"]
            first = Engine.applyEvent before (MoveUsed Player (MoveId "unavailable") (BattleUpdate attacked Nothing Nothing Nothing))
            second = Engine.applyEvent first (MoveUsed Enemy (MoveId "unavailable") (BattleUpdate attacked Nothing (Just damagedParty) (Just 123)))
            final = Engine.applyEvent second (TurnCompleted (BattleUpdate completed (Just (Map.singleton "tonic" 1)) Nothing Nothing))
        assertBool "Expected the first move to apply only its battle snapshot" (first == before{battle = Just attacked})
        assertBool "Expected the second move to apply the recorded party and seed" (second == first{party = damagedParty, randomSeed = 123})
        assertBool "Expected completion to use recorded facts while preserving the other fields" (final == second{battle = Just completed, inventory = Map.singleton "tonic" 1}),
      testCase "Persist and replay a mixed stream of old and new battle events" $ do
        let opened = start fixture
        case battle opened of
          Nothing -> assertFailure "Expected the battle fixture to start"
          Just active -> do
            let attacked = active{battleOpponentParty = [basicCritter 7 2 10 "stonekin"]}
                damagedParty = Party [basicCritter 16 2 20 "leafling"]
                completed = attacked{battleTurn = 1}
                history = [BattleStarted active, BattleChanged (BattleUpdate active Nothing Nothing Nothing), MoveUsed Player (MoveId "strike") (BattleUpdate attacked Nothing Nothing Nothing), MoveUsed Enemy (MoveId "strike") (BattleUpdate attacked Nothing (Just damagedParty) (Just 123)), TurnCompleted (BattleUpdate completed Nothing Nothing Nothing)]
                expected = opened{battle = Just completed, party = damagedParty, randomSeed = 123}
            directory <- getTemporaryDirectory
            bracket
              ( do
                  (path, handle) <- openTempFile directory "critters-move-events"
                  hClose handle
                  pure path
              )
              removeFile
              ( \path -> do
                  saved <- Save.save path history
                  assertBool "Expected the event stream to be saved" (saved == Right ())
                  loaded <- Save.load path
                  assertBool "Expected lossless codecs and deterministic replay" $
                    loaded == Right history && fmap (Save.apply fixture) loaded == Right expected
              ),
      testCase "Include the actor and move in event log labels"
        $ assertBool "Expected readable move and turn events"
        $ pretty (MoveUsed Player (MoveId "strike") update) == "MoveUsed Player strike"
          && pretty (MoveUsed Enemy (MoveId "strike") update) == "MoveUsed Enemy strike"
          && pretty (TurnCompleted update) == "TurnCompleted"
    ]
  where
    recorded = defaultBattle{battleOpponentParty = [basicCritter 7 2 10 "stonekin"]}
    update = BattleUpdate recorded Nothing Nothing Nothing
