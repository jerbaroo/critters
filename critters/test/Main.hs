module Main (main) where

import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import State
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertBool, Assertion, testCase)

assertRoundTrip :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Assertion
assertRoundTrip value =
  assertBool "JSON should preserve the value" $
    Aeson.eitherDecode (Aeson.encode value) == Right value

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "State"
    [ testGroup
        "Decode errors"
        [ testCase "Fractional coordinate" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "{\"x\":1.5,\"y\":0}" :: Either String Position)
        , testCase "Invalid JSON" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "{" :: Either String GameState)
        , testCase "Missing coordinate" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "{\"x\":1}" :: Either String Position)
        , testCase "Missing save version" $
            assertBool "Expected a parse error" $
              isLeft
                ( Aeson.eitherDecode
                    "{\"playerPosition\":{\"x\":0,\"y\":0},\"world\":{\"tiles\":[]}}"
                    :: Either String GameState
                )
        , testCase "Non-string tile" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "1" :: Either String Tile)
        , testCase "Out-of-range coordinate" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "{\"x\":1e100,\"y\":0}" :: Either String Position)
        , testCase "Unknown tile" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "\"lava\"" :: Either String Tile)
        , testCase "Unknown world tile" $
            assertBool "Expected a parse error" $
              isLeft (Aeson.eitherDecode "{\"tiles\":[[\"lava\"]]}" :: Either String World)
        , testCase "Unsupported save version" $
            assertBool "Expected a parse error" $
              isLeft
                ( Aeson.eitherDecode
                    "{\"playerPosition\":{\"x\":0,\"y\":0},\"version\":2,\"world\":{\"tiles\":[]}}"
                    :: Either String GameState
                )
        ]
    , testGroup
        "JSON structure"
        [ testCase "Load an independently written fixture" $
            assertBool "Expected the fixture's state" $
              Aeson.eitherDecode fixture == Right state
        , testCase "Save the expected JSON structure" $
            assertBool "Expected the fixture's JSON fields" $
              Aeson.eitherDecode fixture == Right (Aeson.toJSON state)
        ]
    , testGroup
        "Round trips"
        [ testCase "GameState" $ assertRoundTrip initialState
        , testCase "Position" $
            forM_ [Position 0 0, Position (-1) 2, Position minBound maxBound] assertRoundTrip
        , testCase "Tile" $
            forM_ [Ground, TallGrass, Wall, Water] assertRoundTrip
        , testCase "World" $
            forM_ [World [], World [[]], World [[Ground], [], [Water, Wall]]] assertRoundTrip
        ]
    , testGroup
        "World coordinates"
        [ testCase "Bottom-left origin with y increasing upwards" $
            assertBool "Expected columns to run rightwards and rows upwards" $
              map (tileAt sampleWorld) [Position 0 0, Position 0 1, Position 1 0, Position 1 1]
                == map Just [Ground, TallGrass, Water, Wall]
        , testCase "Missing cells" $
            assertBool "Expected empty worlds and missing cells to return Nothing" $
              tileAt (World []) (Position 0 0) == Nothing
                && tileAt (World [[]]) (Position 0 0) == Nothing
                && tileAt (World [[Ground], []]) (Position 0 0) == Nothing
                && tileAt (World [[Ground], []]) (Position 0 1) == Just Ground
        , testCase "Outside the world" $
            forM_
              [ Position (-1) 0
              , Position 0 (-1)
              , Position 0 2
              , Position 2 0
              , Position maxBound maxBound
              , Position minBound minBound
              ]
              $ \position -> assertBool "Expected Nothing outside the world" (tileAt sampleWorld position == Nothing)
        ]
    ]
  where
    fixture =
        "{\"playerPosition\":{\"x\":2,\"y\":1},\"version\":1,\"world\":{\"tiles\":[[\"ground\",\"wall\"],[\"water\",\"tall-grass\"]]}}"
    sampleWorld = World [[TallGrass, Wall], [Ground, Water]]
    state = GameState (Position 2 1) (World [[Ground, Wall], [Water, TallGrass]])
