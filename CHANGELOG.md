# Changelog

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
