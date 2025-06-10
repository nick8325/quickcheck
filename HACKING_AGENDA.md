
### Instances

- Arbitrary instances for everything in base (and other qc dependencies)
- Super clever TH magic to find all the types and generate properties
  - Enumerate all modules in base and get all the data types
  - Minimal: type check `arbitrary @TheType`
  - Better: Monomorphise and generate a properties to test the arbitrary instance
  - Manually exclude modules/types (to avoid writing clever rules for which
    types to support)

### Parallel fork

- Robert gives a nice presentation of what's going on

### Crazy Koen ideas

- Faster splittable generators

- Constrained generation

### API for test runners

- Tasty and HSpec install their own callbacks
  - Can we make PRs for them using Robert's new API?
  - Robert has the code, but needs some version juggling to work with different
    versions of QuickCheck.

### Cleanup

- Drop ghc 7 support
- Clean up flag mess in cabal file
- Add MicroHs support
  - Lennart: "needs random numbers, this is hard!"
