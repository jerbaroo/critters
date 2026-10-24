# Project conventions

- Write the game in Haskell, keeping serializable state, the pure game engine,
  and rendering separate.
- Put each state type in `State.<Type>`, with `State` re-exporting the modules.
- Keep top-level definitions, record fields, and export lists alphabetical.
- Set the language and all `default-extensions` in a shared Cabal common stanza
  imported by every component. All components must use the same language and
  extensions; do not add module-level `LANGUAGE` pragmas.
- Always put `qualified` after the module name, e.g.
  `import Data.Aeson qualified as Aeson`.
- Import `Data.Aeson` qualified, except that operators may be unqualified.
- Import `Autodocodec` functions qualified and types/constructors unqualified.
- Define `HasCodec` instances and derive Aeson's `FromJSON` and `ToJSON` through
  `Autodocodec`.
- Use Cabal to build and type-check, and `tasty` with `tasty-hunit` for tests.
