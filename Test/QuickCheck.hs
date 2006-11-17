module Test.QuickCheck
  ( 
    -- * Random generation
    Gen
    -- ** Generator combinators
  , sized
  , resize, choose
  , promote
  , suchThat, suchThatMaybe
  , oneof, frequency
  , elements, growingElements
  , listOf, listOf1, vectorOf
  -- ** Generators which use Arbitrary
  , vector, orderedList
    -- ** Generator debugging
  , sample

  -- * Arbitrary and CoArbitrary classes.
  , Arbitrary(..)
  , CoArbitrary(..)
  
  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral, arbitrarySizedFractional
  , arbitraryBoundedIntegral, arbitraryBoundedRandom
  -- ** Helper functions for implementing shrink
  , shrinkNothing, shrinkIntegral, shrinkRealFrac
  -- ** Helper functions for implementing coarbitrary
  , variant, (><)
  , coarbitraryIntegral, coarbitraryReal

  -- ** Type-level modifiers for changing generator behavior
  , Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , Positive(..)
  , NonNegative(..)
  , Smart(..)
  , Shrinking(..)
  , ShrinkState(..)

    -- * Properties
  , Property, Prop, Testable(..)
  , Result(..)
    -- ** Property combinators
  , mapSize, shrinking
  , (==>)
  , forAll, forAllBlind, forAllShrink
  , (.&.), (.&&.)
  , forSeveral, forSeveralShrink
    -- *** Handling failure
  , whenFail, whenFail'
  , expectFailure, within
    -- *** Test distribution
  , label, collect, classify, cover
         
    -- * Running tests
  , quickCheck, quickCheck'
  , quickCheckWith, quickCheckIO
  )
 where

-- the above exports should probably be a bit more restrictive

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test

--------------------------------------------------------------------------
-- the end.
