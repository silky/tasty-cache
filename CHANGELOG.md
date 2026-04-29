# Changelog

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
