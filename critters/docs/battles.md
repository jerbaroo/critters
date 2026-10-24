# Battles

Battles use the same commands → facts → state flow as exploration. The engine
is pure; menus render the resulting state. A battle has one active critter on
each side, with replacements drawn from each party. Both parties obey the
game's `maxPartySize`.

## Controls

| Input                | Action                                                        |
|----------------------|---------------------------------------------------------------|
| Up / `k`, Down / `j` | Select an entry                                               |
| Enter                | Open a submenu, confirm an action, or acknowledge the outcome |
| Escape               | Return to battle actions                                      |
| `q`                  | Quit; the event history already contains the current state    |

Choose Moves, Party, or Items. Escape appears when the battle permits it.
Selecting a move resolves both sides' actions. A voluntary switch, item, or
failed escape uses a turn. When an active critter faints, choose a conscious
replacement; that replacement does not spend another turn. The opposing party
replaces fainted members automatically. Long menus scroll with the selection.

After victory, defeat, capture, or escape, Enter returns to exploration and
applies the corresponding outcome effects. Effects are validated together,
then recorded once. An invalid outcome effect closes the battle and displays
its error without applying part of the effect list.

## Game data

Add `combat` and optionally `inventory` and `randomSeed` to the existing game
file. Identifiers, names, affinities, species, moves, statuses, items, encounter
tables, and numeric rules belong to that file. Omitted fields use defaults.
Existing games without `combat` continue to support exploration and parties.

This example is a set of root fields to merge into a game containing an
`overworld` map:

```json
{
  "combat": {
    "battles": {
      "practice": {
        "name": "Practice match",
        "opponentPolicy": "strongest",
        "opponents": [
          {"hitPoints": 26, "level": 3, "maxHitPoints": 26, "species": "stonekin"}
        ],
        "victoryEffects": [{"type": "set-flag", "flag": "practice.won", "value": true}],
        "defeatEffects": [{"type": "heal-party"}]
      },
      "wild": {
        "allowCapture": true,
        "allowEscape": true,
        "name": "Wild encounter",
        "opponentPolicy": "random",
        "opponents": [
          {"hitPoints": 24, "level": 2, "maxHitPoints": 24, "species": "leafling"}
        ]
      }
    },
    "encounters": [{
      "chance": 0.15,
      "entries": [{"battle": "wild", "minLevel": 2, "maxLevel": 4, "weight": 1}],
      "mapId": "overworld",
      "minimumSteps": 3,
      "tiles": ["tall-grass"]
    }],
    "items": {
      "capsule": {"captureBonus": 1.5, "name": "Capsule"},
      "remedy": {"cureStatus": true, "name": "Remedy"},
      "restorer": {"name": "Restorer", "restoreUses": 5},
      "reviver": {"name": "Reviver", "reviveFraction": 0.5},
      "tonic": {"healing": 20, "name": "Tonic"}
    },
    "moves": {
      "focus": {"effects": [{"type": "change-stage", "target": "self", "stat": "attack", "amount": 1}], "maxUses": 10, "name": "Focus"},
      "strike": {"maxUses": 20, "name": "Strike", "power": 20},
      "vine": {
        "accuracy": 0.9,
        "affinity": "leaf",
        "attackStat": "spirit",
        "criticalRate": 0.05,
        "defenseStat": "resistance",
        "effects": [{"type": "chance", "probability": 0.2, "effect": {"type": "inflict-status", "status": "wither"}}],
        "maxUses": 15,
        "name": "Vine lash",
        "power": 25
      }
    },
    "rules": {
      "affinityChart": {"leaf": {"stone": 2}, "stone": {"leaf": 0.5}},
      "criticalMultiplierFormula": "1.5",
      "damageFormula": "max(1, floor((2 * level / 5 + 2) * power * attack / defense / 50 + 2)) * critical * affinity * sameAffinity * variance",
      "experienceFormula": "level ^ 3",
      "experienceSharing": "participants",
      "learningPolicy": "keep-newest",
      "maxKnownMoves": 4,
      "maxLevel": 100,
      "randomDamageMax": 1,
      "randomDamageMin": 0.85,
      "statusCaptureBonuses": {"wither": 1.5},
      "tieBreak": "random"
    },
    "species": {
      "leafling": {
        "affinities": ["leaf"],
        "captureRate": 0.5,
        "experienceYield": 12,
        "learnset": {"1": ["strike"], "3": ["focus"], "5": ["vine"]},
        "name": "Leafling",
        "stats": {"attack": 12, "defense": 10, "health": 20, "resistance": 11, "speed": 13, "spirit": 15}
      },
      "stonekin": {
        "affinities": ["stone"],
        "experienceYield": 15,
        "learnset": {"1": ["strike"], "3": ["focus"]},
        "name": "Stonekin",
        "stats": {"attack": 14, "defense": 16, "health": 20, "speed": 8}
      }
    },
    "statuses": {
      "wither": {"damageFraction": 0.125, "immuneAffinities": ["leaf"], "name": "Wither", "statMultipliers": {"attack": 0.5}}
    }
  },
  "inventory": {"capsule": 5, "remedy": 2, "restorer": 1, "reviver": 1, "tonic": 3},
  "randomSeed": 73
}
```

Species `stats` can contain any stat identifiers. Moves select their attack
and defense stats independently; changing the split between stats requires
data changes. Missing stats fall back to 10. `health` supplies the base for
generated maximum health and level changes. Existing individuals retain their
authored HP until a level change. Multiple defending affinities multiply.
Missing chart entries have multiplier 1; multiplier 0 prevents direct damage.

## Starting and rewarding battles

Use ordinary interaction conditions, dialogue, and choices. For example, an
interaction's choice can contain:

```json
{"label": "Practice", "effects": [{"type": "start-battle", "battle": "practice"}]}
```

The battle identifier resolves through `combat.battles`. Start requires a
conscious player and opponent, valid combat data, and a closed party menu.
There is no special story or starter path in battle code.

Each definition accepts `victoryEffects`, `defeatEffects`, `captureEffects`,
and `escapeEffects`. Lists contain the same effects used by interactions,
including `set-flag`, `give-critter`, `remove-object`, `start-battle`, and:

```json
[
  {"type": "give-item", "item": "tonic", "count": 2},
  {"type": "heal-party"}
]
```

`heal-party` restores HP, clears persistent statuses, and restores move uses.
Defeat also applies `defeatRecovery`, a fraction of maximum health (default 1);
zero disables automatic recovery. Authored defeat effects can provide further
recovery or story consequences.

## Individual state and progression

A critter's existing four fields remain valid. The optional `battle` object
stores individual progress:

```json
{
  "hitPoints": 24,
  "level": 2,
  "maxHitPoints": 24,
  "species": "leafling",
  "battle": {
    "experience": 8,
    "knownMoves": ["strike"],
    "remainingUses": {"strike": 18},
    "status": {"remaining": 2, "status": "wither"}
  }
}
```

When `knownMoves` is empty, moves derive from the species learnset up to the
current level. `maxKnownMoves` limits the result. Learning is automatic:
`keep-newest` retains the latest moves; `keep-oldest` retains the earliest.
An absent remaining-use entry means fully restored. `maxUses: 0` means
unlimited uses. When no learned move is usable, `fallbackMove` is available;
it must have unlimited uses.

Experience is awarded once per defeated opponent. `experienceSharing` selects
`participants` or `party`; `experienceForFainted` and `captureGivesExperience`
default to false. Older individuals with no experience value start earning
from the threshold for their current level. Level gains preserve damage taken,
recalculate maximum HP, and learn new moves up to the configured cap.

## Moves, statuses, and items

Moves support `accuracy` (default 1), `priority` (0), `criticalRate` (0), and
multi-hit ranges `minHits`/`maxHits` (both 1). Optional `hitWeights`, for example
`{"2": 3, "3": 3, "4": 1}`, replaces uniform hit-count sampling. Set the
range to include every weighted hit count. `accuracyPerHit` and
`criticalPerHit` control independent rolls. Damage variance is sampled per hit.

Effects run in order after a connected move, including a zero-power move:

| Effect           | Fields                                    |
|------------------|-------------------------------------------|
| `chance`         | `probability`, nested `effect`            |
| `change-health`  | `target`, signed formula `amount`         |
| `change-stage`   | `target`, `stat`, signed integer `amount` |
| `clear-status`   | `target`                                  |
| `inflict-status` | `target`, configured `status`             |

Targets are `opponent` (default) and `self`. Health effects can implement drain
(`damage / 2`), recoil (`-damage / 4`), or healing (`maxHP / 2`). `damage` is the
actual total HP removed across all hits. Health effects do not revive fainted
critters; use an item for revival.

One persistent status can affect each critter. Definitions configure
`blockChance`, `cureChance` before acting, residual `damageFraction`,
`statMultipliers`, and `immuneAffinities`. Both duration bounds zero means
persistent; otherwise `durationMin`/`durationMax` choose a positive duration.
`tickOnApply` controls whether the application turn counts for residual damage
and duration. Stat stages are separate and clamp to `stageLimit`.

Ordinary items target a party member. Healing, fractional healing, curing,
restoring uses, and revival can be combined. An ineffective item uses neither
stock nor a turn. Positive `captureBonus` makes an item a capture item;
`allowCapture` must be true and the party must have room. A successful capture
preserves the opponent's current HP, level, status, and move resources. Failed
captures spend the item and permit retaliation. `allowEscape` defaults to false;
failed escapes allow retaliation and increase the `attempts` formula variable.

## Rule formulas

Expressions support `+ - * / ^`, unary signs, parentheses, comparisons
`< <= > >= == !=`, and `abs`, `ceil`, `clamp(low, high, value)`, `floor`,
`if(condition, yes, no)`, `max`, `min`, `round`, and `sqrt`. Exponentiation is
right-associative and binds before unary negation. Comparisons return 0 or 1.
`if` evaluates only the chosen branch. Values must be finite; probability
results clamp to 0–1. Invalid syntax, unknown variables, and non-increasing
experience curves are rejected while loading. Runtime arithmetic errors use
the relevant engine fallback, so guard possible zero denominators with `max`
or `if`.

| Formula                                          | Variables                                                                                                                             |
|--------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| `accuracyFormula`                                | `accuracy`, `accuracyStage`, `evasionStage`, `level`, `targetLevel`                                                                   |
| `accuracyStageFormula`, `stageMultiplierFormula` | `stage`                                                                                                                               |
| `aiScoreFormula`                                 | `damage`, `accuracy`, `heal`, `hpRatio`, `status`, `priority`, `targetHP`                                                             |
| `captureFormula`                                 | `rate`, `itemBonus`, `hp`, `maxHP`, `statusBonus`, `level`, `targetLevel`                                                             |
| `criticalFormula`                                | `criticalRate`, `speed`, `level`, `targetLevel`                                                                                       |
| `criticalMultiplierFormula`                      | `level`, `targetLevel`                                                                                                                |
| `damageFormula`                                  | `power`, `attack`, `defense`, `baseAttack`, `baseDefense`, `critical`, `affinity`, `sameAffinity`, `variance`, `level`, `targetLevel` |
| `escapeFormula`                                  | `speed`, `enemySpeed`, `attempts`, `level`, `enemyLevel`                                                                              |
| `experienceFormula`                              | `level`                                                                                                                               |
| `experienceRewardFormula`                        | `yield`, `level`, `participants`                                                                                                      |
| `healthFormula`, `statFormula`                   | `base`, `level`                                                                                                                       |
| Move `change-health` amount                      | `damage`, `hp`, `maxHP`, `level`, `targetHP`, `targetMaxHP`                                                                           |

`baseAttack` and `baseDefense` include level and persistent status multipliers
but exclude temporary stages. `criticalIgnoresStages` controls whether critical
damage uses these values instead of staged stats. `minimumDamage` defaults to 1
for a damaging, nonimmune move. `sameAffinityBonus` defaults to 1.5.

Actions sort by priority, then the stat named by `speedStat` (default `speed`),
then `tieBreak` (`player`, `opponent`, or `random`). Move priority defaults to 0;
`itemPriority`, `switchPriority`, and `escapePriority` default to 6, 7, and 8.
`resetStagesOnSwitch` defaults to true. `simultaneousDefeatWins` defaults to false.

Opponent policies are `first` (first available move), `random` (uniform available
move), and `strongest` (highest `aiScoreFormula` result). All respect remaining
uses and the fallback move. The score estimates direct damage, recovery, and
status effects; it does not search future turns.

## Encounters and persistence

After successful movement, the first enabled table matching the map and
destination tile is checked. Blocked movement and parties without a conscious
critter do not roll. `minimumSteps` counts eligible movement steps, and entry
`weight` selects among configured battle definitions. Optional level bounds
generate opponent levels and HP using the health rule. A triggered encounter
resets the step count. `encountersAfterWarp` defaults to false.

`randomSeed` is an integer from 0 through 2147483647. Entry records the concrete
opponents, rule snapshot, and random state. Resolved turns record changed
party, inventory, random state, messages, and menu state as facts. Replaying a
save never rerolls an encounter or turn. Editing root combat rules affects new
battles; an active battle continues using its snapshot. Completed outcomes
remain visible until acknowledged, including after a restart.

The generated `critters-docs` schema lists all field defaults. Serializable
types are re-exported through `Critters.State`; battle modules expose pure
entry and action resolution functions.
