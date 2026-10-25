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

## Messages

All game-facing wording lives in the top-level `messages` catalogue, including
battle logs, menus, party displays, dialogue, choices, and action feedback.
Content refers to it with `{messages.KEY}`; local arguments and entity names use
the same curly-brace syntax:

```json
{
  "messages": {
    "battleMoveUsed": "{species} used {move}.",
    "rivalBattleName": "Rival {npcs.blue.name}",
    "choiceBulbasaur": "{combat.species.bulbasaur.name}"
  }
}
```

See [the complete message catalogue](critters/docs/messages.md) for each template,
supported arguments, reference syntax, and rendered examples. Technical startup
and invalid-config diagnostics remain in code.
