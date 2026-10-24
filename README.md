# Critters

A tool for writing your own collecting and battling game.

## Architecture

Clean separation of:
- **State:** serializable snapshot of a game.
- **Save:** serializable history of events.
- **Rendering:** render game state (currently only TUI).
- **Engine:** update game state based on events.
