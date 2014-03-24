{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck
  (
    -- * Running tests
    quickCheck
  , Args(..), Result(..)
  , stdArgs
  , quickCheckWith
  , quickCheckWithResult
  , quickCheckResult
    -- ** Running tests verbosely
  , verboseCheck
  , verboseCheckWith
  , verboseCheckWithResult
  , verboseCheckResult
#ifndef NO_TEMPLATE_HASKELL
    -- ** Testing all properties in a module
  , quickCheckAll
  , verboseCheckAll
  , forAllProperties
    -- ** Testing polymorphic properties
  , polyQuickCheck
  , polyVerboseCheck
  , monomorphic
#endif

    -- * Random generation
  , Gen
    -- ** Generator combinators
  , choose
  , oneof
  , frequency
  , elements
  , growingElements
  , sized
  , resize
  , suchThat
  , suchThatMaybe
  , listOf
  , listOf1
  , vectorOf
  , infiniteListOf
    -- ** Generators which use Arbitrary
  , vector
  , orderedList
  , infiniteList
    -- ** Running a generator
  , generate
    -- ** Generator debugging
  , sample
  , sample'

    -- * Arbitrary and CoArbitrary classes
  , Arbitrary(..)
  , CoArbitrary(..)

    -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral
  , arbitrarySizedFractional
  , arbitrarySizedBoundedIntegral
  , arbitraryBoundedIntegral
  , arbitraryBoundedRandom
  , arbitraryBoundedEnum
    -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericShrink
  , subterms
  , recursivelyShrink
#endif
  , shrinkNothing
  , shrinkList
  , shrinkIntegral
  , shrinkRealFrac
  , shrinkRealFracToInteger
    -- ** Helper functions for implementing coarbitrary
  , variant
  , coarbitraryIntegral
  , coarbitraryReal
  , coarbitraryShow
  , coarbitraryEnum
  , (><)

    -- ** Type-level modifiers for changing generator behavior
  , Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , Positive(..)
  , NonZero(..)
  , NonNegative(..)
  , Large(..)
  , Small(..)
  , Smart(..)
  , Shrink2(..)
#ifndef NO_MULTI_PARAM_TYPE_CLASSES
  , Shrinking(..)
  , ShrinkState(..)
#endif

    -- * Properties
  , Property, Testable(..)
    -- ** Property combinators
  , forAll
  , forAllShrink
  , shrinking
  , (==>)
  , (===)
  , ioProperty
    -- *** Controlling property execution
  , verbose
  , once
  , within
  , noShrinking
    -- *** Conjunction and disjunction
  , (.&.)
  , (.&&.)
  , conjoin
  , (.||.)
  , disjoin
    -- *** What to do on failure
  , counterexample
  , printTestCase
  , whenFail
  , whenFail'
  , expectFailure
    -- *** Analysing test distribution
  , label
  , collect
  , classify
  , cover
    -- *** Miscellaneous
  , Discard(..)
  , discard
  , mapSize
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property hiding ( Result(..) )
import Test.QuickCheck.Test
import Test.QuickCheck.Text
import Test.QuickCheck.Exception
#ifndef NO_TEMPLATE_HASKELL
import Test.QuickCheck.All
#endif

--------------------------------------------------------------------------
-- the end.
