# Critters

A tool for writing your own collecting and battling game.

## Architecture

Clean separation of:
- **State:** serializable snapshot of game state.
- **Save:** serializable history of events.
- **Render:** render game state (currently only TUI).
- **Engine:** update game state based on events.
