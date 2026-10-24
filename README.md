# Critters

A program for running custom collecting and battling games.

## Architecture

- `State`: serializable snapshot of a game.
- `Save`: serializable history of `Event`.
- `Event`: something that happened in the game.
- `Engine`: update game state based on events.
- `Interface`: input and output (e.g. rendering)
