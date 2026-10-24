module Critters.Battle.Engine (beginBattle) where

import Critters.Battle.Common qualified as Common
import Critters.State.Battle (Battle (..), defaultBattle)
import Critters.State.BattleDefinition qualified as Definition
import Critters.State.CombatConfig qualified as Config
import Critters.State.Critter qualified as Critter
import Critters.State.GameState (GameState (..))
import Critters.State.Party qualified as Party
import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)

beginBattle :: GameState -> Text -> Either Text Battle
beginBattle state identifier
  | isJust (battle state) = Left "A battle is already in progress."
  | partyOpen state = Left "Close your party before starting a battle."
  | otherwise = do
      definition <- maybe (Left "That battle is not defined.") Right (Map.lookup identifier (Config.configBattles (combat state)))
      player <- maybe (Left "You need a conscious critter to battle.") Right (findIndex Common.usable (Party.members (party state)))
      opponent <- maybe (Left "There are no conscious opponents.") Right (findIndex Common.usable (Definition.definitionOpponents definition))
      if all (\critter -> Map.member (Critter.species critter) (Config.configSpecies (combat state))) (Party.members (party state) <> Definition.definitionOpponents definition)
        then Right defaultBattle
          { battleActiveOpponent = opponent,
            battleActivePlayer = player,
            battleConfig = combat state,
            battleDefinition = definition,
            battleMessages = [Definition.definitionName definition],
            battleOpponentParty = Definition.definitionOpponents definition,
            battleParticipants = [player]
          }
        else Left "A participant has no species definition."
