# Critters

A program for running custom collecting and battling games.

## Architecture

- `State`: serializable snapshot of a game.
- `Save`: serializable history of `Event`.
- `Event`: something that happened in the game.
- `Engine`: update game state based on events.
- `Interface`: input and output (e.g. rendering)

## Temporary stat stages

Games define temporary modifiers in `combat.stages`, keyed by stat name:

```json
{
  "stages": {
    "defense": {
      "limit": 6,
      "multiplier": "if(stage >= 0, (2 + stage) / 2, 2 / (2 - stage))"
    },
    "accuracy": {
      "limit": 6,
      "multiplier": "if(stage >= 0, (3 + stage) / 3, 3 / (3 - stage))"
    }
  }
}
```

Each definition requires its own limit and multiplier expression. The `stage`
placeholder supplies the current temporary value, initially zero. Changes are
clamped to the stat's configured limit. The effective stat is its current value
multiplied by the evaluated stage multiplier; accuracy and evasion also use their
own definitions, without special multiplier formulas in the engine.

Define every stage used by your game's calculations or move effects, including
attack, defense, speed, accuracy and evasion when using the standard rules.
There are no built-in stage definitions. Accessing an undefined stage raises a
runtime error naming the missing stat. Saved battle stage values use the
configuration snapshot captured when that battle began.

## Stage-change messages

Games supply stage-change wording in `combat.messages` and readable stat labels
in `combat.statNames`. For example:

```json
{
  "messages": {
    "stageEnemySubject": "Opposing {species}",
    "stageFell": "{subject}'s {stat} fell by {amount} {if(amount == 1, stageSingular, stagePlural)} ({beforeStage} → {afterStage}).",
    "stageMaximum": "{subject}'s {stat} cannot go any higher.",
    "stageMinimum": "{subject}'s {stat} cannot go any lower.",
    "stagePlayerSubject": "Your {species}",
    "stagePlural": "stages",
    "stageRose": "{subject}'s {stat} rose by {amount} {if(amount == 1, stageSingular, stagePlural)} ({beforeStage} → {afterStage}).",
    "stageSingular": "stage",
    "stageUnchanged": "{subject}'s {stat} stayed unchanged ({afterStage})."
  },
  "statNames": {
    "defense": "Defence"
  }
}
```

Templates use the general `Critters.Expression` syntax inside braces. The engine
chooses the template using the actual change after clamping. A blocked request
uses `stageMinimum` or `stageMaximum`; a request of zero uses `stageUnchanged`.
Subject templates receive the species display name as `{species}`.

| Placeholder | Meaning |
|---|---|
| `subject`, `species`, `stat` | Affected team and critter, critter display name, stat display label |
| `amount` | Actual number of stages changed, always nonnegative |
| `before`, `after` | Numeric stages, usable in expressions |
| `beforeStage`, `afterStage` | Stage text with a `+` for positive values |
| `beforeMultiplier`, `afterMultiplier` | Multipliers evaluated using the game's stage definition |
| `requested` | Signed change requested by the move |
| `stageSingular`, `stagePlural` | Game-defined unit wording |

The rendered text is recorded in the existing battle log and event history.
Missing message templates or stat labels, malformed expressions, and unknown
placeholders raise runtime errors instead of substituting fallback text.
