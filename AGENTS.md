# Project conventions

- Write the game in Haskell, keeping serializable state, the pure game engine,
  and rendering separate.
- Follow this workflow: plan -> user approval -> one review unit -> status,
  staging, and user approval -> next review unit. Stop at each approval step;
  do not present a plan and implement it in the same handoff.
- Present a concise plan before implementation, using Haskell types and type
  signatures and identifying the proposed review units. Where appropriate,
  include a table mapping inputs to commands and events. Wait for explicit
  user approval of the plan before implementing the first unit. If the plan
  changes, present the revised plan and wait for approval again.
- Divide work into small, sensible units for review. Keep each unit focused on
  one coherent change with its relevant tests. Implement and check only one
  approved unit per handoff, then report what changed, validation results, and
  the proposed next unit. Stop and wait for the user to review, stage, and
  explicitly approve continuing before starting the next unit. Leave staging
  and committing to the user.
- Ground behavior changes in explicit requirements, reproduced bugs, or
  established invariants supported by existing code or tests. Identify that
  evidence in the plan. Hypothetical future needs are not sufficient grounds
  for a change.
- Preserve unrelated behavior and state. In particular, event handlers should
  update only the fields required by the event; additional resets or side
  effects need their own evidence and approval. Do not add tests merely to
  justify speculative behavior. When evidence is missing, leave the behavior
  unchanged and raise the proposed change in the plan for discussion.
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
