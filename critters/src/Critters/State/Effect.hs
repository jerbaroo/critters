module Critters.State.Effect (Effect (..)) where

import Autodocodec (Autodocodec (..), HasCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.State.Critter (Critter)
import Critters.State.FlagId (FlagId)
import Critters.State.NpcId (NpcId)
import Critters.State.ObjectId (ObjectId)
import Critters.State.Position (Position)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

-- | An interaction's requested outcome, validated before events are produced.
data Effect
  = GiveCritter Critter
  | GiveItem Text Int
  | HealParty
  | RemoveObject ObjectId
  | SetFlag FlagId Bool
  | StartBattle Text
  | WalkNpc NpcId [Position]
  deriving stock (Eq)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec Effect)

instance HasCodec Effect where
  codec =
    Codec.named "Effect"
      $ Codec.object "Effect"
      $ Codec.discriminatedUnionCodec "type" encode
      $ HashMap.fromList
        [ ("give-critter", ("GiveCritter", Codec.mapToDecoder GiveCritter given)),
          ("give-item", ("GiveItem", Codec.mapToDecoder (uncurry GiveItem) item)),
          ("heal-party", ("HealParty", pure HealParty)),
          ("remove-object", ("RemoveObject", Codec.mapToDecoder RemoveObject removed)),
          ("set-flag", ("SetFlag", Codec.mapToDecoder (uncurry SetFlag) set)),
          ("start-battle", ("StartBattle", Codec.mapToDecoder StartBattle started)),
          ("walk-npc", ("WalkNpc", Codec.mapToDecoder (uncurry WalkNpc) walked))
        ]
    where
      encode = \case
        GiveCritter critter -> ("give-critter", Codec.mapToEncoder critter given)
        GiveItem identifier count -> ("give-item", Codec.mapToEncoder (identifier, count) item)
        HealParty -> ("heal-party", pure ())
        RemoveObject identifier -> ("remove-object", Codec.mapToEncoder identifier removed)
        SetFlag identifier value -> ("set-flag", Codec.mapToEncoder (identifier, value) set)
        StartBattle identifier -> ("start-battle", Codec.mapToEncoder identifier started)
        WalkNpc identifier path -> ("walk-npc", Codec.mapToEncoder (identifier, path) walked)
      given :: JSONObjectCodec Critter
      given = Codec.requiredField "critter" "The critter to add to the party."
      item :: JSONObjectCodec (Text, Int)
      item = (,) <$> Codec.requiredField "item" "Game-defined item identifier." Codec..= fst <*> Codec.requiredFieldWith "count" (Codec.bimapCodec (\count -> if count > 0 then Right count else Left "Item grants must be positive.") id Codec.codec) "Positive item count." Codec..= snd
      removed :: JSONObjectCodec ObjectId
      removed = Codec.requiredField "objectId" "The world object to remove."
      set :: JSONObjectCodec (FlagId, Bool)
      set =
        (,)
          <$> Codec.requiredField "flag" "The game-defined flag identifier." Codec..= fst
          <*> Codec.requiredField "value" "The new flag value." Codec..= snd
      started :: JSONObjectCodec Text
      started = Codec.requiredField "battle" "Game-defined battle identifier."
      walked :: JSONObjectCodec (NpcId, [Position])
      walked = (,) <$> Codec.requiredField "npc" "The NPC to walk." Codec..= fst <*> Codec.requiredField "path" "Adjacent positions in walking order." Codec..= snd
