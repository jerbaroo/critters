# Game messages

All game-facing text belongs in the root `messages` catalogue. Content fields
reference it using `{messages.KEY}`. Entity names stay in their definitions:
`{npcs.oak.name}`, `{combat.species.bulbasaur.name}`, and
`{combat.items.potion.name}`. The same pattern supports moves, statuses, and
`{combat.statNames.defense}`. Hyphenated identifiers are supported.

Local arguments are supplied at each call site. Explicit reference arguments can
compose templates; literal arguments are inserted unchanged. Arithmetic and
conditionals use `Critters.Expression`, for example
`{if(amount == 1, messages.stageSingular, messages.stagePlural)}`.
Missing keys, unknown references, and cycles are errors. There are no built-in
English templates. Technical diagnostics remain code-owned.

Battle logs store message identities, arguments, and presentation independently
of wording. Battle and selected-interaction snapshots retain their catalogue;
NPC name references and legacy conversations retain live lookup behavior.

The following catalogue documents the game-supplied examples and local arguments.
## Catalogue

154 keys: 78 engine/interface/stage keys and 76 distinct authored messages drawn from 90 content locations.


### Battle narration

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `battleTitle` | `{opponent}` | `opponent` | Rival Blue |
| `battleMoveUsed` | `{species} used {move}.` | `species, move` | Bulbasaur used Tackle. |
| `battleMoveMissed` | `The move missed.` | None | The move missed. |
| `battleCriticalHit` | `Critical hit.` | None | Critical hit. |
| `battleDamage` | `{species} lost {amount} HP.` | `species, amount` | Bulbasaur lost 1 HP. |
| `battleEscapeForbidden` | `Escape is not permitted in this battle.` | None | Escape is not permitted in this battle. |
| `battleChooseReplacement` | `Choose another critter.` | None | Choose another critter. |
| `battleEntered` | `{species} entered the battle.` | `species` | Bulbasaur entered the battle. |
| `battleCaptureComplete` | `Capture complete.` | None | Capture complete. |
| `battleDefeat` | `Defeat.` | None | Defeat. |
| `battleEscaped` | `Escaped safely.` | None | Escaped safely. |
| `battleVictory` | `You defeated {opponent}` | `opponent` | You defeated Rival Blue |
| `battleEnemySentOut` | `{opponent} sent out {species}.` | `opponent, species` | Rival Blue sent out Bulbasaur. |
| `battlePlayerSentOut` | `Go, {species}!` | `species` | Go, Bulbasaur! |
| `battleWildName` | `Wild {species}` | `species` | Wild Bulbasaur |
| `battleDefaultName` | `Battle` | None | Battle |
| `battleFallbackMoveName` | `Basic strike` | None | Basic strike |

### Health and status

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `statusDamage` | `{species} lost {amount} HP to {status}.` | `species, amount, status` | Bulbasaur lost 1 HP to poison. |
| `statusRecovered` | `{species} recovered from {status}.` | `species, status` | Bulbasaur recovered from poison. |
| `healthChanged` | `{species} HP changed by {amount}.` | `species, amount` | Bulbasaur HP changed by 1. |
| `statusCleared` | `Status cleared.` | None | Status cleared. |
| `statusNoEffect` | `The status had no effect.` | None | The status had no effect. |
| `statusInflicted` | `{species} is affected by {status}.` | `species, status` | Bulbasaur is affected by poison. |
| `statusActionRecovered` | `Recovered from {status}.` | `status` | Recovered from poison. |
| `statusActionBlocked` | `Unable to act because of {status}.` | `status` | Unable to act because of poison. |

### Items, capture and escape

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `escapeFailed` | `The escape attempt failed.` | None | The escape attempt failed. |
| `itemUsed` | `Used {item} on {species}.` | `item, species` | Used Potion on Bulbasaur. |
| `captureFailed` | `The capture attempt failed.` | None | The capture attempt failed. |
| `captureSucceeded` | `Captured {species}.` | `species` | Captured Bulbasaur. |
| `itemUndefined` | `That item is not defined.` | None | That item is not defined. |
| `itemUnavailable` | `You have none of that item.` | None | You have none of that item. |
| `captureForbidden` | `Capture is not permitted in this battle.` | None | Capture is not permitted in this battle. |
| `partyFull` | `Your party is full.` | None | Your party is full. |
| `captureNoOpponent` | `There is no opponent to capture.` | None | There is no opponent to capture. |
| `itemChoosePartyMember` | `Choose a party member.` | None | Choose a party member. |
| `itemNoEffect` | `That item would have no effect.` | None | That item would have no effect. |

### Progression

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `experienceGained` | `{species} gained {amount} experience.` | `species, amount` | Bulbasaur gained 1 experience. |
| `levelReached` | `{species} reached level {level}.` | `species, level` | Bulbasaur reached level 3. |
| `moveLearned` | `{species} learned {move}.` | `species, move` | Bulbasaur learned Tackle. |
| `moveUnknownName` | `a move` | None | a move |

### Action feedback displayed in the game

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `battleUndefined` | `That battle is not defined.` | None | That battle is not defined. |
| `encounterSpeciesUndefined` | `That encounter species is not defined.` | None | That encounter species is not defined. |
| `battleAlreadyActive` | `A battle is already in progress.` | None | A battle is already in progress. |
| `battleCloseParty` | `Close your party before starting a battle.` | None | Close your party before starting a battle. |
| `battleNoConsciousCritter` | `You need a conscious critter to battle.` | None | You need a conscious critter to battle. |
| `battleNoConsciousOpponent` | `There are no conscious opponents.` | None | There are no conscious opponents. |
| `battleOpponentPartyTooLarge` | `Opposing party exceeds the game party limit.` | None | Opposing party exceeds the game party limit. |
| `battleInvalidParticipant` | `A participant has an invalid species, move, status, resource count or level.` | None | A participant has an invalid species, move, status, resource count or level. |
| `effectInvalidCritter` | `That critter has invalid combat data.` | None | That critter has invalid combat data. |
| `effectInvalidItem` | `That item or count is invalid.` | None | That item or count is invalid. |
| `effectObjectUnavailable` | `That object is no longer available.` | None | That object is no longer available. |
| `effectNpcUnavailable` | `That NPC is not available.` | None | That NPC is not available. |
| `effectNpcPathBlocked` | `That NPC walking path is blocked or has a nonadjacent step.` | None | That NPC walking path is blocked or has a nonadjacent step. |

### Menus and interface text

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `menuMoves` | `Moves` | None | Moves |
| `menuParty` | `Party` | None | Party |
| `menuItems` | `Items` | None | Items |
| `menuEscape` | `Escape` | None | Escape |
| `menuItem` | `{item} x{count}` | `item, count` | Potion x2 |
| `menuPartyMember` | `{species}  HP {hp}/{maxHP}` | `species, hp, maxHP` | Bulbasaur  HP 18/20 |
| `menuMove` | `{move}{resource}` | `move, resource` | Tackle  4/6 |
| `menuMoveResource` | `  {remaining}/{maximum}` | `remaining, maximum` |   4/6 |
| `combatantDetails` | `{species}  Lv {level}  HP {hp}/{maxHP}{statusSuffix}` | `species, level, hp, maxHP, statusSuffix` | Bulbasaur  Lv 3  HP 18/20  [poison] |
| `combatantStatus` | `  [{status}]` | `status` |   [poison] |
| `partyEmpty` | `Your party is empty` | None | Your party is empty |
| `partyHeading` | `Party — {count} / {maximum}` | `count, maximum` | Party — 2 / 6 |
| `partyMember` | `{index}. {species}  Lv {level}  HP {hp}/{maxHP}` | `index, species, level, hp, maxHP` | 1. Bulbasaur  Lv 3  HP 18/20 |
| `debugBattleHeading` | `Battle: {opponent}` | `opponent` | Battle: Rival Blue |
| `debugCombatant` | `{species} HP {hp}/{maxHP}` | `species, hp, maxHP` | Bulbasaur HP 18/20 |
| `debugUnrecognizedInput` | `Unrecognized input.` | None | Unrecognized input. |

### Stage messages

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `stageEnemySubject` | `Opposing {species}` | `species` | Opposing Bulbasaur |
| `stageFell` | `{subject}'s {stat} fell by {amount} {if(amount == 1, messages.stageSingular, messages.stagePlural)} ({beforeStage} → {afterStage}).` | `after, afterMultiplier, afterStage, amount, before, beforeMultiplier, beforeStage, requested, species, stat, subject` | Your Bulbasaur's Defence fell by 1 stage (0 → -1). |
| `stageMaximum` | `{subject}'s {stat} cannot go any higher.` | `after, afterMultiplier, afterStage, amount, before, beforeMultiplier, beforeStage, requested, species, stat, subject` | Your Bulbasaur's Defence cannot go any higher. |
| `stageMinimum` | `{subject}'s {stat} cannot go any lower.` | `after, afterMultiplier, afterStage, amount, before, beforeMultiplier, beforeStage, requested, species, stat, subject` | Your Bulbasaur's Defence cannot go any lower. |
| `stagePlayerSubject` | `Your {species}` | `species` | Your Bulbasaur |
| `stagePlural` | `stages` | None | stages |
| `stageRose` | `{subject}'s {stat} rose by {amount} {if(amount == 1, messages.stageSingular, messages.stagePlural)} ({beforeStage} → {afterStage}).` | `after, afterMultiplier, afterStage, amount, before, beforeMultiplier, beforeStage, requested, species, stat, subject` | Your Bulbasaur's Defence rose by 1 stage (0 → +1). |
| `stageSingular` | `stage` | None | stage |
| `stageUnchanged` | `{subject}'s {stat} stayed unchanged ({afterStage}).` | `after, afterMultiplier, afterStage, amount, before, beforeMultiplier, beforeStage, requested, species, stat, subject` | Your Bulbasaur's Defence stayed unchanged (-1). |

### Authored red.json text

| Key | Example template | Local arguments / used content references | Example output |
|---|---|---|---|
| `rivalBattleName` | `Rival {npcs.blue.name}` | `npcs.blue.name` | Rival Blue |
| `labRivalBulbasaurIntroduction0` | `{npcs.blue.name}: Hey! Let's check out our new partners!` | `npcs.blue.name` | Blue: Hey! Let's check out our new partners! |
| `labRivalBulbasaurIntroduction1` | `{npcs.blue.name}: Come on, I'll take you on!` | `npcs.blue.name` | Blue: Come on, I'll take you on! |
| `npcBlueInteraction0Line0` | `Our first battle is settled. I'll see you on the road!` | None | Our first battle is settled. I'll see you on the road! |
| `npcBlueInteraction1Line0` | `Choose your companion first. Then we can battle!` | None | Choose your companion first. Then we can battle! |
| `npcBlueInteraction2Line0` | `We have not settled our battle yet. Which companion did you choose?` | None | We have not settled our battle yet. Which companion did you choose? |
| `choiceBulbasaur` | `{combat.species.bulbasaur.name}` | `combat.species.bulbasaur.name` | Bulbasaur |
| `choiceCharmander` | `{combat.species.charmander.name}` | `combat.species.charmander.name` | Charmander |
| `choiceSquirtle` | `{combat.species.squirtle.name}` | `combat.species.squirtle.name` | Squirtle |
| `npcLabScientistAideLine0` | `{npcs.oak.name} studies how critters live and grow.` | `npcs.oak.name` | Professor Oak studies how critters live and grow. |
| `npcLabWorkerAideLine0` | `Welcome to the lab. {npcs.oak.name} is usually near the workbench.` | `npcs.oak.name` | Welcome to the lab. Professor Oak is usually near the workbench. |
| `npcOakInteraction0Line0` | `You made it back with the parcel. Thank you!` | None | You made it back with the parcel. Thank you! |
| `npcOakInteraction0Line1` | `There's something important I want to show you.` | None | There's something important I want to show you. |
| `npcOakInteraction1Line0` | `Take good care of your new companion!` | None | Take good care of your new companion! |
| `npcOakInteraction2Line0` | `Go ahead and choose one of the three creatures.` | None | Go ahead and choose one of the three creatures. |
| `npcOakInteraction3Line0` | `Welcome to my lab!` | None | Welcome to my lab! |
| `npcOakInteraction3Line1` | `You may choose one of these three creatures.` | None | You may choose one of these three creatures. |
| `npcPalletDaisyInteraction0Line0` | `{npcs.blue.name} has been at the lab all morning.` | `npcs.blue.name` | Blue has been at the lab all morning. |
| `npcPalletDaisyInteraction0Line1` | `He is sure he will become a great trainer someday.` | None | He is sure he will become a great trainer someday. |
| `npcPalletDaisyInteraction1Line0` | `I heard you and {npcs.blue.name} had your first battle. He will want a rematch!` | `npcs.blue.name` | I heard you and Blue had your first battle. He will want a rematch! |
| `npcPalletGuardInteraction0Line0` | `{npcs.oak.name} asked me to keep this route closed until you and {npcs.blue.name} settle your battle.` | `npcs.oak.name, npcs.blue.name` | Professor Oak asked me to keep this route closed until you and Blue settle your battle. |
| `npcPalletGuardInteraction1Line0` | `You've beaten Blue. The way north is clear now.` | None | You've beaten Blue. The way north is clear now. |
| `npcPalletMotherInteraction0Line0` | `You were in a hurry to get downstairs this morning.` | None | You were in a hurry to get downstairs this morning. |
| `npcPalletMotherInteraction0Line1` | `Take your time and get ready before heading out.` | None | Take your time and get ready before heading out. |
| `npcPalletMotherInteraction1Line0` | `You and {npcs.blue.name} had quite a day. Rest whenever you need to.` | `npcs.blue.name` | You and Blue had quite a day. Rest whenever you need to. |
| `npcPalletSignReaderLine0` | `I like reading the signs around town before I set out.` | None | I like reading the signs around town before I set out. |
| `npcPalletSignReaderLine1` | `There is always something useful to learn.` | None | There is always something useful to learn. |
| `npcPalletTravelerLine0` | `Pallet may be small, but the road north is full of surprises.` | None | Pallet may be small, but the road north is full of surprises. |
| `npcPartyHealerInteraction0Line0` | `Let me restore your party.` | None | Let me restore your party. |
| `npcRoute1ClerkInteraction0Line0` | `A delivery for the shop ran late, so I have a spare {combat.items.potion.name}.` | `combat.items.potion.name` | A delivery for the shop ran late, so I have a spare Potion. |
| `npcRoute1ClerkInteraction0Line1` | `Take it for the road.` | None | Take it for the road. |
| `npcRoute1ClerkInteraction1Line0` | `Take care on the road. Wild critters are common in the tall grass.` | None | Take care on the road. Wild critters are common in the tall grass. |
| `npcRoute1YoungsterLine0` | `The path north leads to Viridian City.` | None | The path north leads to Viridian City. |
| `npcRoute1YoungsterLine1` | `Keep an eye on your party while crossing the grass.` | None | Keep an eye on your party while crossing the grass. |
| `npcViridianCenterAttendantLeftLine0` | `These rooms are set aside for trainers to meet.` | None | These rooms are set aside for trainers to meet. |
| `npcViridianCenterAttendantMiddleLeftLine0` | `Travelers come here to share stories and advice.` | None | Travelers come here to share stories and advice. |
| `npcViridianCenterAttendantMiddleRightLine0` | `There is plenty to learn from meeting other trainers.` | None | There is plenty to learn from meeting other trainers. |
| `npcViridianCenterAttendantRightLine0` | `I hope you enjoy your stay in Viridian City.` | None | I hope you enjoy your stay in Viridian City. |
| `npcViridianCenterBoyLine0` | `It is reassuring to know there is a clinic in town.` | None | It is reassuring to know there is a clinic in town. |
| `npcViridianCenterGentlemanLine0` | `Travelers can rest here before continuing their journey.` | None | Travelers can rest here before continuing their journey. |
| `npcViridianCenterNurseInteraction0Line0` | `Welcome. Let me restore your party.` | None | Welcome. Let me restore your party. |
| `npcViridianCenterYoungsterLine0` | `The stairs lead to rooms where trainers can meet.` | None | The stairs lead to rooms where trainers can meet. |
| `npcViridianGymFanLine0` | `The town gym is closed today. I hope its leader returns soon.` | None | The town gym is closed today. I hope its leader returns soon. |
| `npcViridianHouseGirlLine0` | `Spearow always chirps when I talk to it.` | None | Spearow always chirps when I talk to it. |
| `npcViridianHouseManLine0` | `My Spearow loves hearing stories about the roads beyond town.` | None | My Spearow loves hearing stories about the roads beyond town. |
| `npcViridianMartClerkInteraction0Line0` | `You must be helping {npcs.oak.name}.` | `npcs.oak.name` | You must be helping Professor Oak. |
| `npcViridianMartClerkInteraction0Line1` | `Please deliver this parcel to him in Pallet Town.` | None | Please deliver this parcel to him in Pallet Town. |
| `npcViridianMartClerkInteraction1Line0` | `Thanks for delivering that parcel. Safe travels!` | None | Thanks for delivering that parcel. Safe travels! |
| `npcViridianMartCustomerALine0` | `I always check my supplies before setting out on a long trip.` | None | I always check my supplies before setting out on a long trip. |
| `npcViridianMartCustomerBLine0` | `A good supply of healing items can make a big difference on the road.` | None | A good supply of healing items can make a big difference on the road. |
| `npcViridianMoveTutorLine0` | `A thoughtful choice of moves can help a critter overcome a tough challenge.` | None | A thoughtful choice of moves can help a critter overcome a tough challenge. |
| `npcViridianOldManLine0` | `Travelers from all over pass through this town.` | None | Travelers from all over pass through this town. |
| `npcViridianResidentLine0` | `Viridian is a quiet town where travelers stop before heading farther north.` | None | Viridian is a quiet town where travelers stop before heading farther north. |
| `npcViridianSchoolStudentLine0` | `I am practicing the type chart before the next lesson.` | None | I am practicing the type chart before the next lesson. |
| `npcViridianSchoolTeacherLine0` | `Today we are learning how different critters use their moves and types.` | None | Today we are learning how different critters use their moves and types. |
| `npcViridianShopperLine0` | `The shop has supplies for the road. It's a good place to prepare for a journey.` | None | The shop has supplies for the road. It's a good place to prepare for a journey. |
| `npcViridianTutorialManLine0` | `The tall grass south of town can be rough going. Keep your party ready.` | None | The tall grass south of town can be rough going. Keep your party ready. |
| `objectLabBulbasaurInteraction0Line0` | `You have already chosen your companion.` | None | You have already chosen your companion. |
| `objectLabBulbasaurInteraction1Line0` | `These belong to Oak. You should ask him first.` | None | These belong to Oak. You should ask him first. |
| `objectLabBulbasaurInteraction2Line0` | `This Poké Ball contains {combat.species.bulbasaur.name}. Choose it?` | `combat.species.bulbasaur.name` | This Poké Ball contains Bulbasaur. Choose it? |
| `choiceYes` | `Yes` | None | Yes |
| `choiceNo` | `No` | None | No |
| `objectLabCharmanderInteraction2Line0` | `This Poké Ball contains {combat.species.charmander.name}. Choose it?` | `combat.species.charmander.name` | This Poké Ball contains Charmander. Choose it? |
| `objectLabSquirtleInteraction2Line0` | `This Poké Ball contains {combat.species.squirtle.name}. Choose it?` | `combat.species.squirtle.name` | This Poké Ball contains Squirtle. Choose it? |
| `objectPalletLabSignInteraction0Line0` | `{npcs.oak.name}'s Laboratory` | `npcs.oak.name` | Professor Oak's Laboratory |
| `objectPalletPlayerHouseSignInteraction0Line0` | `Player's House` | None | Player's House |
| `objectPalletRivalHouseSignInteraction0Line0` | `{npcs.blue.name}'s House` | `npcs.blue.name` | Blue's House |
| `objectPalletRouteRattataInteraction0Line0` | `A wild {combat.species.rattata.name} is blocking the narrow path north.` | `combat.species.rattata.name` | A wild Rattata is blocking the narrow path north. |
| `objectPalletTownSignInteraction0Line0` | `Pallet Town — A quiet place to begin a journey.` | None | Pallet Town — A quiet place to begin a journey. |
| `objectPalletTrainerTipsSignInteraction0Line0` | `Trainer Tips: Talk to people around town for advice.` | None | Trainer Tips: Talk to people around town for advice. |
| `objectRoute1NorthSignInteraction0Line0` | `Viridian City is just ahead.` | None | Viridian City is just ahead. |
| `objectRoute1SouthSignInteraction0Line0` | `Route 1 — The road between Pallet Town and Viridian City.` | None | Route 1 — The road between Pallet Town and Viridian City. |
| `objectViridianGymSignInteraction0Line0` | `Viridian City Gym — Closed for now.` | None | Viridian City Gym — Closed for now. |
| `objectViridianHouseSpearowInteraction0Line0` | `Spearow chirps brightly.` | None | Spearow chirps brightly. |
| `objectViridianNorthSignInteraction0Line0` | `Route 2 lies to the north.` | None | Route 2 lies to the north. |
| `objectViridianTownSignInteraction0Line0` | `Viridian City — A peaceful town for travelers.` | None | Viridian City — A peaceful town for travelers. |


## red.json content references

Each content location below now contains `{messages.KEY}`. Repeated identical messages share a key. NPC and content name definitions stay where they are. References to Blue/Oak and defined species/items in the migrated dialogue use explicit curly-brace references. Names without a corresponding existing definition (for example, Spearow or town names) remain authored literal text.

| Message key | Existing content locations |
|---|---|
| `rivalBattleName` | `combat.battles.lab-rival-bulbasaur.name`<br>`combat.battles.lab-rival-charmander.name`<br>`combat.battles.lab-rival-squirtle.name` |
| `labRivalBulbasaurIntroduction0` | `combat.battles.lab-rival-bulbasaur.introduction.0`<br>`combat.battles.lab-rival-charmander.introduction.0`<br>`combat.battles.lab-rival-squirtle.introduction.0` |
| `labRivalBulbasaurIntroduction1` | `combat.battles.lab-rival-bulbasaur.introduction.1`<br>`combat.battles.lab-rival-charmander.introduction.1`<br>`combat.battles.lab-rival-squirtle.introduction.1` |
| `npcBlueInteraction0Line0` | `npcs.blue.interactions.0.dialogue.0` |
| `npcBlueInteraction1Line0` | `npcs.blue.interactions.1.dialogue.0` |
| `npcBlueInteraction2Line0` | `npcs.blue.interactions.2.dialogue.0` |
| `choiceBulbasaur` | `npcs.blue.interactions.2.choices.0.label` |
| `choiceCharmander` | `npcs.blue.interactions.2.choices.1.label` |
| `choiceSquirtle` | `npcs.blue.interactions.2.choices.2.label` |
| `npcLabScientistAideLine0` | `npcs.lab-scientist-aide.dialogue.0` |
| `npcLabWorkerAideLine0` | `npcs.lab-worker-aide.dialogue.0` |
| `npcOakInteraction0Line0` | `npcs.oak.interactions.0.dialogue.0` |
| `npcOakInteraction0Line1` | `npcs.oak.interactions.0.dialogue.1` |
| `npcOakInteraction1Line0` | `npcs.oak.interactions.1.dialogue.0` |
| `npcOakInteraction2Line0` | `npcs.oak.interactions.2.dialogue.0` |
| `npcOakInteraction3Line0` | `npcs.oak.interactions.3.dialogue.0` |
| `npcOakInteraction3Line1` | `npcs.oak.interactions.3.dialogue.1` |
| `npcPalletDaisyInteraction0Line0` | `npcs.pallet-daisy.interactions.0.dialogue.0` |
| `npcPalletDaisyInteraction0Line1` | `npcs.pallet-daisy.interactions.0.dialogue.1` |
| `npcPalletDaisyInteraction1Line0` | `npcs.pallet-daisy.interactions.1.dialogue.0` |
| `npcPalletGuardInteraction0Line0` | `npcs.pallet-guard.interactions.0.dialogue.0` |
| `npcPalletGuardInteraction1Line0` | `npcs.pallet-guard.interactions.1.dialogue.0` |
| `npcPalletMotherInteraction0Line0` | `npcs.pallet-mother.interactions.0.dialogue.0` |
| `npcPalletMotherInteraction0Line1` | `npcs.pallet-mother.interactions.0.dialogue.1` |
| `npcPalletMotherInteraction1Line0` | `npcs.pallet-mother.interactions.1.dialogue.0` |
| `npcPalletSignReaderLine0` | `npcs.pallet-sign-reader.dialogue.0` |
| `npcPalletSignReaderLine1` | `npcs.pallet-sign-reader.dialogue.1` |
| `npcPalletTravelerLine0` | `npcs.pallet-traveler.dialogue.0` |
| `npcPartyHealerInteraction0Line0` | `npcs.party-healer.interactions.0.dialogue.0` |
| `npcRoute1ClerkInteraction0Line0` | `npcs.route-1-clerk.interactions.0.dialogue.0` |
| `npcRoute1ClerkInteraction0Line1` | `npcs.route-1-clerk.interactions.0.dialogue.1` |
| `npcRoute1ClerkInteraction1Line0` | `npcs.route-1-clerk.interactions.1.dialogue.0` |
| `npcRoute1YoungsterLine0` | `npcs.route-1-youngster.dialogue.0` |
| `npcRoute1YoungsterLine1` | `npcs.route-1-youngster.dialogue.1` |
| `npcViridianCenterAttendantLeftLine0` | `npcs.viridian-center-attendant-left.dialogue.0` |
| `npcViridianCenterAttendantMiddleLeftLine0` | `npcs.viridian-center-attendant-middle-left.dialogue.0` |
| `npcViridianCenterAttendantMiddleRightLine0` | `npcs.viridian-center-attendant-middle-right.dialogue.0` |
| `npcViridianCenterAttendantRightLine0` | `npcs.viridian-center-attendant-right.dialogue.0` |
| `npcViridianCenterBoyLine0` | `npcs.viridian-center-boy.dialogue.0` |
| `npcViridianCenterGentlemanLine0` | `npcs.viridian-center-gentleman.dialogue.0` |
| `npcViridianCenterNurseInteraction0Line0` | `npcs.viridian-center-nurse.interactions.0.dialogue.0` |
| `npcViridianCenterYoungsterLine0` | `npcs.viridian-center-youngster.dialogue.0` |
| `npcViridianGymFanLine0` | `npcs.viridian-gym-fan.dialogue.0` |
| `npcViridianHouseGirlLine0` | `npcs.viridian-house-girl.dialogue.0` |
| `npcViridianHouseManLine0` | `npcs.viridian-house-man.dialogue.0` |
| `npcViridianMartClerkInteraction0Line0` | `npcs.viridian-mart-clerk.interactions.0.dialogue.0` |
| `npcViridianMartClerkInteraction0Line1` | `npcs.viridian-mart-clerk.interactions.0.dialogue.1` |
| `npcViridianMartClerkInteraction1Line0` | `npcs.viridian-mart-clerk.interactions.1.dialogue.0` |
| `npcViridianMartCustomerALine0` | `npcs.viridian-mart-customer-a.dialogue.0` |
| `npcViridianMartCustomerBLine0` | `npcs.viridian-mart-customer-b.dialogue.0` |
| `npcViridianMoveTutorLine0` | `npcs.viridian-move-tutor.dialogue.0` |
| `npcViridianOldManLine0` | `npcs.viridian-old-man.dialogue.0` |
| `npcViridianResidentLine0` | `npcs.viridian-resident.dialogue.0` |
| `npcViridianSchoolStudentLine0` | `npcs.viridian-school-student.dialogue.0` |
| `npcViridianSchoolTeacherLine0` | `npcs.viridian-school-teacher.dialogue.0` |
| `npcViridianShopperLine0` | `npcs.viridian-shopper.dialogue.0` |
| `npcViridianTutorialManLine0` | `npcs.viridian-tutorial-man.dialogue.0` |
| `objectLabBulbasaurInteraction0Line0` | `objects.lab-bulbasaur.interactions.0.dialogue.0`<br>`objects.lab-charmander.interactions.0.dialogue.0`<br>`objects.lab-squirtle.interactions.0.dialogue.0` |
| `objectLabBulbasaurInteraction1Line0` | `objects.lab-bulbasaur.interactions.1.dialogue.0`<br>`objects.lab-charmander.interactions.1.dialogue.0`<br>`objects.lab-squirtle.interactions.1.dialogue.0` |
| `objectLabBulbasaurInteraction2Line0` | `objects.lab-bulbasaur.interactions.2.dialogue.0` |
| `choiceYes` | `objects.lab-bulbasaur.interactions.2.choices.0.label`<br>`objects.lab-charmander.interactions.2.choices.0.label`<br>`objects.lab-squirtle.interactions.2.choices.0.label` |
| `choiceNo` | `objects.lab-bulbasaur.interactions.2.choices.1.label`<br>`objects.lab-charmander.interactions.2.choices.1.label`<br>`objects.lab-squirtle.interactions.2.choices.1.label` |
| `objectLabCharmanderInteraction2Line0` | `objects.lab-charmander.interactions.2.dialogue.0` |
| `objectLabSquirtleInteraction2Line0` | `objects.lab-squirtle.interactions.2.dialogue.0` |
| `objectPalletLabSignInteraction0Line0` | `objects.pallet-lab-sign.interactions.0.dialogue.0` |
| `objectPalletPlayerHouseSignInteraction0Line0` | `objects.pallet-player-house-sign.interactions.0.dialogue.0` |
| `objectPalletRivalHouseSignInteraction0Line0` | `objects.pallet-rival-house-sign.interactions.0.dialogue.0` |
| `objectPalletRouteRattataInteraction0Line0` | `objects.pallet-route-rattata.interactions.0.dialogue.0` |
| `objectPalletTownSignInteraction0Line0` | `objects.pallet-town-sign.interactions.0.dialogue.0` |
| `objectPalletTrainerTipsSignInteraction0Line0` | `objects.pallet-trainer-tips-sign.interactions.0.dialogue.0` |
| `objectRoute1NorthSignInteraction0Line0` | `objects.route-1-north-sign.interactions.0.dialogue.0` |
| `objectRoute1SouthSignInteraction0Line0` | `objects.route-1-south-sign.interactions.0.dialogue.0` |
| `objectViridianGymSignInteraction0Line0` | `objects.viridian-gym-sign.interactions.0.dialogue.0` |
| `objectViridianHouseSpearowInteraction0Line0` | `objects.viridian-house-spearow.interactions.0.dialogue.0` |
| `objectViridianNorthSignInteraction0Line0` | `objects.viridian-north-sign.interactions.0.dialogue.0` |
| `objectViridianTownSignInteraction0Line0` | `objects.viridian-town-sign.interactions.0.dialogue.0` |

## Stored messages

`MessageKey`, `Messages`, and `Template` distinguish catalogue identities and
source text. A `Message` contains a key and local arguments. An argument is either
a literal text/number or an explicit reference such as
`{"reference":"{combat.species.bulbasaur.name}"}`. `BattleMessage` adds the
`dialogue` or `health-change` presentation kind, independently of wording.
Catalogue/content/local-reference cycles are rejected. Unused conditional branches
are not evaluated. Do not interpolate argument text a second time in a renderer.
