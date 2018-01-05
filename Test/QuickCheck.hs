{-|
The <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual>
gives detailed information about using QuickCheck effectively.

To start using QuickCheck, write down your property as a function returning @Bool@.
For example, to check that reversing a list twice gives back the same list you can write:

@
import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
@

You can then use QuickCheck to test @prop_reverse@ on 100 random lists:

>>> quickCheck prop_reverse
+++ OK, passed 100 tests.

To run more tests you can use the 'withMaxSuccess' combinator:

>>> quickCheck (withMaxSuccess 10000 prop_reverse)
+++ OK, passed 10000 tests.

To use QuickCheck on your own data types you will need to write 'Arbitrary'
instances for those types. See the
<http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual> for
details about how to do that.

This module exports most of QuickCheck's functionality, but see also
"Test.QuickCheck.Monadic" which helps with testing impure or monadic code.
-}
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
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
  , allProperties
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
  , getSize
  , resize
  , scale
  , suchThat
  , suchThatMap
  , suchThatMaybe
  , listOf
  , listOf1
  , vectorOf
  , infiniteListOf
  , shuffle
  , sublistOf
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

  -- ** Unary and Binary classes
  , Arbitrary1(..)
  , arbitrary1
  , shrink1
  , Arbitrary2(..)
  , arbitrary2
  , shrink2

    -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral
  , arbitrarySizedNatural
  , arbitrarySizedFractional
  , arbitrarySizedBoundedIntegral
  , arbitraryBoundedIntegral
  , arbitraryBoundedRandom
  , arbitraryBoundedEnum
  , arbitraryUnicodeChar
  , arbitraryASCIIChar
  , arbitraryPrintableChar
    -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericCoarbitrary
  , genericShrink
  , subterms
  , recursivelyShrink
#endif
  , shrinkNothing
  , shrinkList
  , shrinkMap
  , shrinkMapBy
  , shrinkIntegral
  , shrinkRealFrac
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
  , InfiniteList(..)
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
  , ASCIIString(..)
  , UnicodeString(..)
  , PrintableString(..)

    -- ** Functions
  , Fun (..)
  , applyFun
  , applyFun2
  , applyFun3
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
#endif
  , Function (..)
  , functionMap

    -- * Properties
  , Property, Testable(..)
    -- ** Property combinators
  , forAll
  , forAllShrink
  , shrinking
  , (==>)
  , (===)
#ifndef NO_DEEPSEQ
  , total
#endif
  , ioProperty
    -- *** Controlling property execution
  , verbose
  , once
  , again
  , withMaxSuccess
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
import Test.QuickCheck.Exception
import Test.QuickCheck.Function
#ifndef NO_TEMPLATE_HASKELL
import Test.QuickCheck.All
#endif

--------------------------------------------------------------------------
-- the end.
