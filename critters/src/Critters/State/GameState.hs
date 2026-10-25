module Critters.State.GameState (catalogue, conversationText, GameState (..), initialState, interactionText, messageText, npcAt, objectAt, renderMessage, resolveText, textEnvironment, world) where

import Autodocodec (Autodocodec (..), Codec (EqCodec), HasCodec, JSONCodec, JSONObjectCodec)
import Autodocodec qualified as Codec
import Critters.Expression (Value (..))
import Critters.Input.Direction (Direction (North))
import Critters.Message qualified as Message
import Critters.Pretty (pretty)
import Critters.State.ActiveInteraction (ActiveInteraction)
import Critters.State.ActiveInteraction qualified as Active
import Critters.State.Battle (Battle)
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig (CombatConfig, defaultCombat)
import Critters.State.CombatConfig qualified as Combat
import Critters.State.Conversation (Conversation (..))
import Critters.State.Encounter qualified as Encounter
import Critters.State.FlagId (FlagId)
import Critters.State.Interaction qualified as Interaction
import Critters.State.Item qualified as Item
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.Message (Argument, Message, MessageKey, Messages (..))
import Critters.State.Move qualified as Move
import Critters.State.Npc (Npc)
import Critters.State.Npc qualified as Npc
import Critters.State.NpcId (NpcId (..))
import Critters.State.Object (Object)
import Critters.State.Object qualified as Object
import Critters.State.ObjectId (ObjectId)
import Critters.State.Party (Party)
import Critters.State.Party qualified as Party
import Critters.State.Position (Position (..))
import Critters.State.Species qualified as Species
import Critters.State.StatusDefinition qualified as Status
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..))
import Critters.State.World qualified as World
import Data.Aeson qualified as Aeson
import Data.Char (toUpper)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Witch (from)

catalogue :: GameState -> Messages
catalogue state = case state.battle of
    Just active -> active.messageTemplates
    Nothing -> maybe state.messages (.messageTemplates) state.activeInteraction

conversationText :: GameState -> Maybe Text
conversationText state = do
    active <- state.conversation
    npc <- Map.lookup active.npcId state.npcs
    case drop active.lineIndex npc.dialogue of
        line : _ | active.lineIndex >= 0 && npc.location.mapId == state.playerMap -> Just line
        _ -> Nothing

data GameState = GameState
    { activeInteraction :: Maybe ActiveInteraction
    , battle :: Maybe Battle
    , combat :: CombatConfig
    , conversation :: Maybe Conversation
    , encounterSteps :: Int
    , flags :: Map FlagId Bool
    , inventory :: Map Text Int
    , maps :: Map MapId World
    , maxPartySize :: Int
    , messages :: Messages
    , npcs :: Map NpcId Npc
    , objects :: Map ObjectId Object
    , party :: Party
    , partyOpen :: Bool
    , playerDirection :: Direction
    , playerMap :: MapId
    , playerPosition :: Position
    , randomSeed :: Int
    }
    deriving stock (Eq)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (Autodocodec GameState)

initialState :: GameState
initialState =
    GameState
        { activeInteraction = Nothing
        , battle = Nothing
        , combat = defaultCombat
        , conversation = Nothing
        , encounterSteps = 0
        , flags = Map.empty
        , inventory = Map.empty
        , maps =
            Map.singleton (MapId "overworld") $
                World
                    { World.tiles =
                        map
                            (map Just)
                            [ replicate 8 Wall
                            , [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall]
                            , [Wall, Ground, Ground, Ground, Ground, TallGrass, TallGrass, Wall]
                            , [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall]
                            , [Wall, Ground, Water, Water, Ground, Ground, Ground, Wall]
                            , replicate 8 Wall
                            ]
                    , World.warps = []
                    }
        , maxPartySize = 6
        , messages = Messages Map.empty
        , npcs = Map.empty
        , objects = Map.empty
        , party = Party.empty
        , partyOpen = False
        , playerDirection = North
        , playerMap = MapId "overworld"
        , playerPosition = Position 1 4
        , randomSeed = 1
        }

-- | Read from the interaction snapshot, without choosing a branch again.
interactionText :: GameState -> Maybe Text
interactionText state = do
    active <- state.activeInteraction
    let index = active.dialogueIndex
    case drop index active.script.dialogue of
        line : _ | index >= 0 -> Just line
        _ -> Nothing

messageText :: GameState -> MessageKey -> [(Text, Argument)] -> Text
messageText state key arguments = renderMessage state (Message.named key arguments)

npcAt :: GameState -> Position -> Maybe (NpcId, Npc)
npcAt state coordinates =
    find ((== Location state.playerMap coordinates) . (.location) . snd) (Map.toAscList state.npcs)

objectAt :: GameState -> Position -> Maybe (ObjectId, Object)
objectAt state coordinates =
    find ((== Location state.playerMap coordinates) . (.location) . snd) (Map.toAscList state.objects)

renderMessage :: GameState -> Message -> Text
renderMessage state message = either (error . from) id (Message.renderMessage (catalogue state) (textEnvironment state) message)

-- | Catalogue references use the same expression module as numeric game rules.
resolveText :: GameState -> Text -> Either Text Text
resolveText state = Message.renderText (catalogue state) (textEnvironment state)

textEnvironment :: GameState -> Map Text Value
textEnvironment state =
    Map.fromList $
        [("npcs." <> pretty identifier <> ".name", TextValue npc.name) | (identifier, npc) <- Map.toList state.npcs]
            <> [("combat.species." <> identifier <> ".name", TextValue (speciesName identifier definition.name)) | (identifier, definition) <- Map.toList combat.species]
            <> [("combat.items." <> identifier <> ".name", TextValue (name identifier definition.name)) | (identifier, definition) <- Map.toList combat.items]
            <> [("combat.moves." <> pretty identifier <> ".name", TextValue (name (pretty identifier) definition.name)) | (identifier, definition) <- Map.toList combat.moves]
            <> [("combat.statuses." <> identifier <> ".name", TextValue (name identifier definition.name)) | (identifier, definition) <- Map.toList combat.statuses]
            <> [("combat.statNames." <> pretty identifier, TextValue label) | (identifier, label) <- Map.toList combat.statNames]
  where
    combat = maybe state.combat (.config) state.battle
    name identifier configured = if Text.null configured then identifier else configured
    speciesName identifier configured
        | not (Text.null configured) = configured
        | otherwise = case Text.uncons identifier of
            Just (first, rest) -> Text.cons (toUpper first) rest
            Nothing -> identifier

versionCodec :: JSONCodec Int
versionCodec = EqCodec (1 :: Int) Codec.codec

-- | Select the active tile map. An unknown map has no tiles to render or enter.
world :: GameState -> World
world state = Map.findWithDefault (World [] []) state.playerMap state.maps

instance HasCodec GameState where
    codec = Codec.bimapCodec validate id $ Codec.object "GameState" $ Codec.parseAlternative current legacy
      where
        current = fields (Codec.requiredField "maps" "Tile maps indexed by stable identifiers.") (Codec.optionalFieldWithOmittedDefault "playerMap" (MapId "overworld") "The active map.")
        fields :: JSONObjectCodec (Map MapId World) -> JSONObjectCodec MapId -> JSONObjectCodec GameState
        fields layouts activeMap =
            GameState
                <$> Codec.optionalFieldOrNull "activeInteraction" "Selected scripted interaction." Codec..= (.activeInteraction)
                <*> Codec.optionalFieldOrNull "battle" "Active battle, including its configuration snapshot." Codec..= (.battle)
                <*> Codec.optionalFieldWithOmittedDefault "combat" defaultCombat "Game-defined battle rules and content." Codec..= (.combat)
                <*> Codec.optionalFieldOrNull "conversation" "Active legacy conversation." Codec..= (.conversation)
                <*> Codec.optionalFieldWithOmittedDefault "encounterSteps" 0 "Eligible steps since the previous encounter." Codec..= (.encounterSteps)
                <*> Codec.optionalFieldWithOmittedDefault "flags" Map.empty "Named story flags; missing flags are false." Codec..= (.flags)
                <*> Codec.optionalFieldWithOmittedDefault "inventory" Map.empty "Item counts indexed by game-defined identifiers." Codec..= (.inventory)
                <*> layouts Codec..= (.maps)
                <*> Codec.optionalFieldWithOmittedDefault "maxPartySize" 6 "Positive game-wide party limit." Codec..= (.maxPartySize)
                <*> Codec.optionalFieldWithOmittedDefault "messages" (Messages Map.empty) "All game-facing message templates." Codec..= (.messages)
                <*> Codec.optionalFieldWithOmittedDefault "npcs" Map.empty "Stationary NPCs." Codec..= (.npcs)
                <*> Codec.optionalFieldWithOmittedDefault "objects" Map.empty "Interactable world objects." Codec..= (.objects)
                <*> Codec.optionalFieldWithOmittedDefault "party" Party.empty "Player party." Codec..= (.party)
                <*> Codec.optionalFieldWithOmittedDefault "partyOpen" False "Whether the party view is open." Codec..= (.partyOpen)
                <*> Codec.optionalFieldWithOmittedDefault "playerDirection" North "Player facing direction." Codec..= (.playerDirection)
                <*> activeMap Codec..= (.playerMap)
                <*> Codec.requiredField "playerPosition" "Coordinates in the active map." Codec..= (.playerPosition)
                <*> Codec.optionalFieldWithOmittedDefault "randomSeed" 1 "Reproducible random state, from zero through 2147483647." Codec..= (.randomSeed)
                <* Codec.requiredFieldWith "version" versionCodec "Save format version." Codec..= const 1
        legacy = fields (Codec.dimapCodec (Map.singleton (MapId "overworld")) (Map.findWithDefault (World [] []) (MapId "overworld")) (Codec.requiredField "world" "Legacy single map.")) (Codec.pureCodec (MapId "overworld"))
        validate state
            | state.maxPartySize <= 0 = Left "maxPartySize must be positive."
            | length state.party.members > state.maxPartySize = Left "Party members exceed the game party limit."
            | state.randomSeed < 0 || state.randomSeed > 2147483647 || state.encounterSteps < 0 = Left "Invalid random seed or encounter step count."
            | any (< 0) (Map.elems state.inventory) = Left "Inventory counts must not be negative."
            | any (\table -> not (Map.member table.mapId state.maps)) state.combat.encounters = Left "An encounter references a missing map."
            | maybe False (\active -> active.activePlayer >= length state.party.members || any (>= length state.party.members) active.participants) state.battle = Left "Active battle references a missing party member."
            | otherwise = Right state
