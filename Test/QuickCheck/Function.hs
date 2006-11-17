module Test.QuickCheck.Function
  -- "magic" functions
  ( Function(..) -- :: * -> * -> *; Show, Arbitrary
  , function     -- :: (a -> b) -> Function a b
  , getTable     -- :: Function a b -> IO [(a,b)]
  , showTable    -- :: [(a,b)] -> String
  
  -- monotonic functions
  , MonotonicFunction(..)         -- :: *; Show, Arbitrary
  , StrictlyMonotonicFunction(..) -- :: *; Show, Arbitrary
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property

import Data.IORef
import Data.List

import System.IO.Unsafe
  ( unsafePerformIO -- yes, it's popping up its evil head again!
  )

--------------------------------------------------------------------------
-- Function _ f: keeps track of arguments that f is applied to,
--               so that f can be shown and shrunk

data Function a b = Function (FunctionTable a b) (a -> b)

newtype FunctionTable a b = MkTable (IORef [(a,b)])

function :: (a -> b) -> Function a b
function f =
  unsafePerformIO $
    do ref <- newIORef []
       return $ Function (MkTable ref) $ \x ->
         unsafePerformIO $
           let y = f x in
             do tab <- readIORef ref
                writeIORef ref ((x,y):tab)
                return y

getTable :: Function a b -> IO [(a,b)]
getTable (Function (MkTable ref) _) =
  do xys <- readIORef ref
     return (reverse xys)

showTable :: (Show a, Show b) => [(a,b)] -> String
showTable xys =
     "{"
  ++ concat (intersperse ", " (tabulate (reverse xys)))
  ++ "}"
 where
  tabulate = map (\((x,y):_) -> x ++ " -> " ++ y)
           . groupBy (\(x1,_) (x2,_) -> x1 == x2)
           . sortBy (\(x1,_) (x2,_) -> x1 `compare` x2)
           . map (\(x,y) -> (show x, show y))

instance (Show a, Show b) => Show (Function a b) where
  show fun =
    unsafePerformIO $
      do xys <- getTable fun
         return (showTable xys)

instance (Eq a, CoArbitrary a, Arbitrary b) => Arbitrary (Function a b) where
  arbitrary =
    function `fmap` arbitrary

  shrink fun@(Function _ f) =
    unsafePerformIO $
      do xys <- getTable fun
         return [ function (update x y' f)
                | (x,y) <- xys
                , y' <- shrink y
                ]
     where
      update x' y' f x
        | x == x'   = y'
        | otherwise = f x

--------------------------------------------------------------------------
-- monotonicity

-- Monotonic fun: guarantees that fun is monotonic

newtype MonotonicFunction = Monotonic (Function Int Int)
 deriving ( Show )

instance Arbitrary MonotonicFunction where
  arbitrary = Monotonic `fmap` arbMonotonicFunction (\(NonNegative x) -> x)

-- StrictlyMonotonic fun: guarantees that fun is strictly monotonic

newtype StrictlyMonotonicFunction = StrictlyMonotonic (Function Int Int)
 deriving ( Show )

instance Arbitrary StrictlyMonotonicFunction where
  arbitrary = StrictlyMonotonic `fmap` arbMonotonicFunction (\(Positive x) -> x)

-- helper functions

arbMonotonicFunction :: Arbitrary a => (a -> Int) -> Gen (Function Int Int)
arbMonotonicFunction val =
  do ups   <- arbIncSeq
     downs <- arbIncSeq
     y0    <- arbitrary
     return $ function $ \x ->
       case x of
         0             -> y0
         _ | x > 0     -> y0 + (ups !! (x-1))
           | otherwise -> y0 - (downs !! (-x-1))
 where
  arbIncSeq =
    do as <- sequence [ arbitrary | _ <- [1..] ]
       let sums s (x:xs) = s `seq` (s : sums (val x+s) xs)
       return (tail (sums 0 as))

--------------------------------------------------------------------------
-- properties

prop_Monotonic x y (Monotonic (Function _ f)) =
  x <= y ==>
    f x <= f y

prop_StrictlyMonotonic x y (StrictlyMonotonic (Function _ f)) =
  x < y ==>
    f x < f y

prop_StrictlyMonotonic_Wrong x y (Monotonic (Function _ f)) =
  expectFailure $
    x < y ==>
      f x < f y

--------------------------------------------------------------------------
-- the end.
