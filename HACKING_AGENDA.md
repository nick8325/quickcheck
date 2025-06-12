
### Instances

- Arbitrary instances for everything in base (and other qc dependencies)
- Super clever TH magic to find all the types and generate properties
  - Enumerate all modules in base and get all the data types
  - Minimal: type check `arbitrary @TheType`
  - Better: Monomorphise and generate a properties to test the arbitrary instance
  - Manually exclude modules/types (to avoid writing clever rules for which
    types to support)
- See CollectDataTypes.hs and RunCollectDataTypes.hs.

### Parallel fork

- Robert gives a nice presentation of what's going on

- We should decide on some design principles:
    - we care only about locally minimal counterexamples
    - we want the features module
    - we don't care about parallel quickcheck exploring sizes
      deterministically
    - we do care about sizes being explored in order small to big

### Crazy Koen ideas

- Constrained generation
    - Let's make a little language that let's us talk about the kind of
    constraints we want to evaluate in a closed universe of functions and a
    closed universe of types (therefore no crazy typeclass nonsense etc)

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
