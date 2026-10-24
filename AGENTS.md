# Project conventions

- Write the game in Haskell, keeping serializable state, the pure game engine,
  and rendering separate.
- Keep top-level definitions, record fields, and export lists alphabetical.
- Set the language and all `default-extensions` in a shared Cabal common stanza
  imported by every component. All components must use the same language and
  extensions; do not add module-level `LANGUAGE` pragmas.
- Always put `qualified` after the module name, e.g.
  `import Data.Aeson qualified as Aeson`.
- Every import must be qualified or have an explicit import list.
- Import `Data.Aeson` qualified, except that operators may be unqualified.
- Use `Witch.from` for conversions, including lazy/strict `Text`, instead of
  conversion-specific functions such as `Data.Text.pack`.
- Import `Autodocodec` functions qualified and types/constructors unqualified.
- Define `HasCodec` instances and derive Aeson's `FromJSON` and `ToJSON` through
  `Autodocodec`.
- Use Cabal to build and type-check, and `tasty` with `tasty-hunit` for tests.
- Leave Cabal dependency versions unconstrained; use the Nix package set's
  versions.
- Leave pre-commit checks to the user; do not run or troubleshoot them unless
  explicitly requested.
