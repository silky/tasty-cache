# Changelog

## 0.1.2.0 — 2026-04-29

Close three implicit-dispatch false negatives surfaced by an
extension-coverage audit, and ship a per-extension regression suite
that classifies every documented GHC language extension against the
caching strategy.

- New heuristic in `buildFileIndex`: parse the file's single-line
  `{-# LANGUAGE … #-}` pragmas and conservatively augment every
  binding's `ddUsedClasses` / `ddUsedNames` based on which implicit
  dispatch the extension enables.  GHC's HIE does not record an
  `EvidenceVarUse` (or any other Use ident) for several desugarings —
  string literals under `OverloadedStrings`, list literals under
  `OverloadedLists`, or `if-then-else` / `do` / numeric-literal forms
  under `RebindableSyntax` — at the binding's span, so the existing
  class-edge / name-edge BFS could not reach the relevant instance
  methods or local rebindings.  Augmentation:
    * `OverloadedStrings` → add `IsString` to `ddUsedClasses`
    * `OverloadedLists`   → add `IsList`   to `ddUsedClasses`
    * `RebindableSyntax`  → add the standard rebound names
      (`ifThenElse`, `>>=`, `>>`, `return`, `fail`, `fromInteger`,
      `fromRational`, `fromString`, `fromList`, `fromLabel`, `negate`,
      `arr`, `>>>`, `first`) to `ddUsedNames`
  The augmentation is over-approximating (a binding with no string
  literal still claims `IsString`), consistent with the project's
  existing "false positives over false negatives" stance.
- New helper `pragmaExtensions :: ByteString -> Set String` parses
  single-line `{-# LANGUAGE … #-}` pragmas (multi-line blocks remain a
  documented limitation, same as the existing pragma-line filter).
- New `ExtensionCoverage` test module — `test/ExtensionCoverage.hs` —
  splits every documented GHC extension into five buckets:
    * **CAT-A** source-byte (any syntactic edit propagates),
    * **CAT-B** single-line pragma capture,
    * **CAT-C** cabal hash captures `default-extensions` / `ghc-options`,
    * **CAT-D** implicit dispatch / synthesised binding / type-level
      edge — one mutation test per extension,
    * **CAT-E** known limitations pinned as regression tests.
  A new fixture module per CAT-D extension lives at the top of `test/`
  so the cache loader (which uses non-recursive `listDirectory ".hie"`)
  finds the resulting `.hie` files.  Fixtures: `OvStrFix`, `OvListFix`,
  `OvLabFix`, `RebindFix`, `DefSigFix`, `AnyClassFix`, `ViaFix`,
  `GNDFix`, `StdDerivData` + `StdDerivFix`, `PatSynData` + `PatSynFix`,
  `TyFamFix`, `DKindsFix`, `ImpParFix`, plus `MultiLinePragmaFix` for
  the multi-line pragma pinning test.
- Refactor: extract `assertFingerprintChanged` and
  `assertFingerprintUnchanged` from `FalseNegatives.hs` into a new
  shared helper module `test/FixtureHelpers.hs`, used by both
  `FalseNegatives` and `ExtensionCoverage`.
- Confirmed via fingerprint-mutation tests that the existing strategy
  already handles `OverloadedLabels`, `DefaultSignatures`,
  `DeriveAnyClass`, `DerivingVia`, `GeneralizedNewtypeDeriving`,
  `StandaloneDeriving`, `PatternSynonyms`, `TypeFamilies`, `DataKinds`,
  and `ImplicitParams` correctly — these now have explicit regression
  coverage rather than relying on incidental behaviour.
- Reorganise the test suite into two top-level groups:
    * `test/Library.hs` — genuine library tests (source-navigation
      helpers, cache-key logic, pragma & cabal hashing, fingerprint
      mutation tests).  The whole tree is wrapped in `cacheable`; the
      project dogfoods its own cache.
    * `test/Demos.hs` — demonstration tests that show what the cache
      supports (cacheable per-area and per-extension demos) and where
      it breaks (always-run demos for mutual recursion, Template
      Haskell, CPP, multi-line pragmas).
  The previous `FalseNegatives` and `ExtensionCoverage` modules are
  removed; their content lives in `Library` and `Demos` respectively.

## 0.1.1.0 — 2026-04-28

Fix the instance-resolution false negative: editing the body of an
`instance C T` no longer leaves cached test results for tests that use
`T` polymorphically through a `C` constraint.

- Extend the dependency BFS in `transitiveDeps` with **class edges** in
  addition to the existing name edges.  Test bodies (and the bodies of
  bindings reached during BFS) are now scanned for `EvidenceVarUse`
  identifiers; each is resolved to a class by chasing the
  `EvLetBind → … → EvInstBind` chain in a global evidence-variable
  index built from every HIE file.  Visiting a class then enqueues
  every binding declared inside any `instance C T` of that class
  across the project.
- Add `EvBind`, `EvBindIndex`, `ClassIndex`, `resolveEvClass`,
  `collectEvBinds`, `getUsedClasses`, and `buildFileIndex` to
  `Test.Tasty.HieCache` (internal but exported).
- Add `internalComputeFingerprint` for testing — loads HIE files,
  applies in-memory source-byte overrides per file, and returns the
  fingerprint for a single named test.  Used by the new end-to-end
  regression test in `test/FalseNegatives.hs`.
- New test fixtures:
    * `test/Instances.hs` — `Num Foo`, `Num Bar`, `Greet Foo`, and
      `Insult Foo` instances, all defined in a module the polymorphic
      helpers never name directly.
    * `test/Polymorphic.hs` — `poly :: Num a => a -> a`,
      `roast :: (Greet a, Insult a) => a -> String` (multi-class
      constraint), and `pair :: forall a b. a -> b -> (a, b)`
      (explicit `forall`).
    * The `Polymorphic` test group in `test/Main.hs` exercises all
      three.
- The `Instance resolution` group in `FalseNegatives` carries seven
  fingerprint-level checks against the real HIE files (using
  `internalComputeFingerprint` with per-file source-byte overrides):
    1. *Editing `Num Foo`'s `fromInteger` invalidates `poly @Foo`* —
       the core regression test that fails on 0.1.0.0 and passes
       here.
    2. *Editing an unused method (`signum`) of `Num Foo` invalidates
       `poly @Foo`* — visiting class `Num` enqueues every method of
       every instance, including ones the test never invokes.
    3. *Editing `Num Bar` invalidates `poly @Foo`* — class-edge
       resolution is class-only, not class+type, so any instance of a
       used class invalidates.
    4. *Editing `Num Foo`'s `fromInteger` does **not** invalidate
       `add 1 2 == 3`* — precision boundary: tests whose BFS goes
       through bindings whose `ddUsedClasses` is empty (e.g. `(+)`,
       which dispatches on `Num Int` from `base` and so resolves
       outside `globalEvBinds`) do not trigger class-edge BFS at
       user-defined classes.
    5. *Editing `Greet Foo` invalidates `roast (Foo 3)`* — multi-class
       evidence A.
    6. *Editing `Insult Foo` invalidates `roast (Foo 3)`* —
       multi-class evidence B; together with #5 this proves that
       multiple classes on a test body's evidence frontier are all
       tracked.
    7. *Swapping `forall a b` to `forall b a` invalidates the
       corresponding `pair` test* — the type-signature line is now
       part of each binding's chunks, so semantically-meaningful
       changes to a signature (forall ordering matters under
       `TypeApplications`) invalidate.
- Include `HIE.TyDecl`-context bindings in `isDeclCtx` so type
  signatures contribute their source bytes to a binding's chunks
  (needed for the forall test above).
- The class-edge BFS is conservative: visiting class `Num` pulls in
  *every* `Num` instance's methods, not only the one specialised at
  the relevant type.  This is consistent with the existing `occName`
  collision behaviour — it produces false positives, never false
  negatives.  Note that the conservatism extends transitively: GHC's
  default-method body for `(-)` (`x - y = x + negate y`) dispatches
  through the user-defined instance dict, so visiting `(-)` from a
  monomorphic test like `factorial 5 == 120` also pulls in every
  `Num` method via the class edge.  This is documented and exercised
  by the `Instance resolution` test group.

## 0.1.0.0 — 2026-04-22

Initial release.

- `defaultMainWithHieCache`: drop-in replacement for `defaultMain`
- `hieCacheIngredient`: the ingredient for manual composition
- `cacheable`: opt-in wrapper; only marked tests are ever skipped
- Transitive dependency BFS over the GHC HIE identifier graph
- Whole-file hashing for CPP modules (`#define` detection)
- Pragma-line hashing for language extension changes
- Cabal file hashing so `default-extensions` changes invalidate the cache
- `OK (cached)` output for skipped tests
- Disable with `cabal test --test-options="--disable-tasty-cache"`
