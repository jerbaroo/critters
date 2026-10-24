module SchemaTests (tests) where

import Autodocodec.Schema qualified as Schema
import BattleTests (fixture, start)
import Critters.Schema (gameSchema)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

example :: Text -> Aeson.Value
example effectType =
  Aeson.object
    [ "combat"
        Aeson..= Aeson.object
          [ "moves"
              Aeson..= Aeson.object
                [ "cleanse"
                    Aeson..= Aeson.object
                      [ "effects" Aeson..= [chance (chance (Aeson.object ["type" Aeson..= effectType]))],
                        "name" Aeson..= ("Cleanse" :: Text)
                      ]
                ]
          ],
      "maps" Aeson..= Aeson.object ["overworld" Aeson..= Aeson.object ["tiles" Aeson..= [["ground" :: Text]]]],
      "playerPosition" Aeson..= Aeson.object ["x" Aeson..= (0 :: Int), "y" Aeson..= (0 :: Int)],
      "version" Aeson..= (1 :: Int)
    ]
  where
    chance effect = Aeson.object ["effect" Aeson..= effect, "probability" Aeson..= (0.5 :: Double), "type" Aeson..= ("chance" :: Text)]

tests :: TestTree
tests =
  testGroup
    "Schema"
    [ testCase "Accept a game with recursive move effects" $
        assertBool "Expected recursive references to resolve without an active battle" (Schema.validateAccordingTo (example "clear-status") gameSchema),
      testCase "Accept shared definitions in game content and an active battle" $
        assertBool "Expected references to validate both the game and its battle snapshot" (Schema.validateAccordingTo (Aeson.toJSON (start fixture)) gameSchema),
      testCase "Reject an unknown effect nested inside chance effects" $
        assertBool "Expected recursive references to validate the nested effect" (not (Schema.validateAccordingTo (example "unknown") gameSchema))
    ]
