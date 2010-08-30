{-# LANGUAGE FlexibleInstances #-}
module Test.QuickCheck.Property where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Text( showErr )
import Test.QuickCheck.Exception
import Test.QuickCheck.State

import Control.Concurrent
  ( forkIO
  , threadDelay
  , killThread
  , newEmptyMVar
  , takeMVar
  , putMVar
  )

import System.IO
  ( hFlush
  , stdout
  )

--------------------------------------------------------------------------
-- fixeties

infixr 0 ==>
infixr 1 .&.
-- infixr 1 .&&.

--------------------------------------------------------------------------
-- * Property and Testable types

type Property = Gen Prop

-- | The class of things which can be tested, i.e. turned into a property.
class Testable prop where
  property :: prop -> Property

instance Testable () where
  property _ = property rejected

instance Testable Bool where
  property = property . liftBool

instance Testable Result where
  property = return . MkProp . return . return

instance Testable Prop where
  property = return . protectProp

instance Testable prop => Testable (Gen prop) where
  property mp = do p <- mp; property p

instance Testable prop => Testable (IO prop) where
  property = fmap (MkProp . IORose . fmap unProp) . promote . fmap property

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property f = forAllShrink arbitrary shrink f

--------------------------------------------------------------------------
-- ** Type Prop

-- is this the right level to be abstract at?

newtype Prop = MkProp{ unProp :: Rose (IO Result) }

protectProp :: Prop -> Prop
protectProp (MkProp r) =
  MkProp . IORose $ do
    (x, rs) <- unpackRose r
    return (MkRose x rs)

-- ** type Rose

-- We never allow a rose tree to be _|_. This makes avoiding
-- exceptions easier.
-- This relies on the fact that the 'property' function never returns _|_.
data Rose a = MkRose a [Rose a] | IORose (IO (Rose a))

join :: Rose (Rose a) -> Rose a
join (IORose rs) = IORose (fmap join rs)
join (MkRose (IORose rm) rs) = IORose $ do r <- rm; return (join (MkRose r rs))
join (MkRose (MkRose x ts) tts) =
  -- first shrinks outer quantification; makes most sense
  MkRose x (map join tts ++ ts)
  -- first shrinks inner quantification
  --MkRose x (ts ++ map join tts)

instance Functor Rose where
  fmap f (IORose rs) = IORose (fmap (fmap f) rs)
  fmap f (MkRose x rs) = MkRose (f x) [ fmap f r | r <- rs ]

instance Monad Rose where
  return x = MkRose x []
  m >>= k  = join (fmap k m)

unpackRose :: Rose (IO Result) -> IO (IO Result, [Rose (IO Result)])
unpackRose rose = either (\e -> (return (exception "Exception" e), [])) id
                  `fmap` tryEvaluateIO (unpack rose)
  where unpack (MkRose x xs) = return (x, xs)
        unpack (IORose m) = m >>= unpack

-- ** Result type

-- | Different kinds of callbacks
data Callback
  = PostTest (State -> Result -> IO ())         -- ^ Called just after a test
  | PostFinalFailure (State -> Result -> IO ()) -- ^ Called with the final failing test-case

-- | The result of a single test.
data Result
  = MkResult
  { ok          :: Maybe Bool     -- ^ result of the test case; Nothing = discard
  , expect      :: Bool           -- ^ indicates what the expected result of the property is
  , reason      :: String         -- ^ a message indicating what went wrong
  , interrupted :: Bool           -- ^ indicates if the test case was cancelled by pressing ^C
  , stamp       :: [(String,Int)] -- ^ the collected values for this test case
  , callbacks   :: [Callback]     -- ^ the callbacks for this test case
  }

result :: Result
result =
  MkResult
  { ok          = undefined
  , expect      = True
  , reason      = ""
  , interrupted = False
  , stamp       = []
  , callbacks   = []
  }

failed :: Result -> Result
failed res = res{ ok = Just False }

exception res err = failed res{ reason = "Exception: '" ++ showErr err ++ "'",
                                interrupted = isInterrupt err }

protectResult :: IO Result -> IO Result
protectResult m = either (exception "Exception") id `fmap` tryEvaluateIO (fmap force m)
  where force res = ok res == Just False `seq` res

succeeded :: Result 
succeeded = result{ ok = Just True }

failed :: Result
failed = result{ ok = Just False }

rejected :: Result
rejected = result{ ok = Nothing }

--------------------------------------------------------------------------
-- ** Lifting and mapping functions

liftBool :: Bool -> Property
liftBool b = liftResult $
  result
  { ok     = Just b
  , reason = if b then "" else "Falsifiable"
  }

liftResult :: Result -> Property
liftResult r = liftIOResult (return r)

liftIOResult :: IO Result -> Property
liftIOResult m = property (MkProp (return m))

mapResult :: Testable prop => (Result -> Result) -> prop -> Property
mapResult f = mapIOResult (fmap f)

mapIOResult :: Testable prop => (IO Result -> IO Result) -> prop -> Property
mapIOResult f = mapRoseIOResult (fmap (f . protectResult))

-- f here has to be total.
mapRoseIOResult :: Testable prop => (Rose (IO Result) -> Rose (IO Result)) -> prop -> Property
mapRoseIOResult f = mapProp (\(MkProp t) -> MkProp (f t))

mapProp :: Testable prop => (Prop -> Prop) -> prop -> Property
mapProp f = fmap f . property

--------------------------------------------------------------------------
-- ** Property combinators

-- | Changes the maximum test case size for a property.
mapSize :: Testable prop => (Int -> Int) -> prop -> Property
mapSize f p = sized ((`resize` property p) . f)

-- | Shrinks the argument to property if it fails. Shrinking is done
-- automatically for most types. This is only needed when you want to
-- override the default behavior.
shrinking :: Testable prop =>
             (a -> [a])  -- ^ 'shrink'-like function.
          -> a           -- ^ The original argument
          -> (a -> prop) -> Property
shrinking shrinker x0 pf = fmap (MkProp . join . fmap unProp) (promote (props x0))
 where
  props x =
    MkRose (property (pf x)) [ props x' | x' <- shrinker x ]

-- | Disables shrinking for a property altogether.
noShrinking :: Testable prop => prop -> Property
noShrinking = mapRoseIOResult f
  where f (MkRose mres _ts) = MkRose mres []

-- | Adds a callback
callback :: Testable prop => Callback -> prop -> Property
callback cb = mapResult (\res -> res{ callbacks = cb : callbacks res })

-- | Performs an 'IO' action after the last failure of a property.
whenFail :: Testable prop => IO () -> prop -> Property
whenFail m =
  callback $ PostFinalFailure $ \_st _res ->
    m

-- | Performs an 'IO' action every time a property fails. Thus,
-- if shrinking is done, this can be used to keep track of the 
-- failures along the way.
whenFail' :: Testable prop => IO () -> prop -> Property
whenFail' m =
  callback $ PostTest $ \_st res ->
    if ok res == Just False
      then m
      else return ()

-- | Modifies a property so that it is expected to fail for some test cases.
expectFailure :: Testable prop => prop -> Property
expectFailure = mapResult (\res -> res{ expect = False })

-- | Attaches a label to a property. This is used for reporting
-- test case distribution.
label :: Testable prop => String -> prop -> Property
label s = classify True s

-- | Labels a property with a value:
--
-- > collect x = label (show x)
collect :: (Show a, Testable prop) => a -> prop -> Property
collect x = label (show x)

-- | Conditionally labels test case.
classify :: Testable prop => 
            Bool    -- ^ @True@ if the test case should be labelled.
         -> String  -- ^ Label.
         -> prop -> Property
classify b s = cover b 0 s

-- | Checks that at least the given proportion of the test cases belong
-- to the given class.
cover :: Testable prop => 
         Bool   -- ^ @True@ if the test case belongs to the class.
      -> Int    -- ^ The required percentage (0-100) of test cases.
      -> String -- ^ Label for the test case class.
      -> prop -> Property
cover b n s = mapResult $ \res ->
        case b of
         True  -> res{ stamp  = (s,n) : stamp res }
         False -> res

-- | Implication for properties: The resulting property holds if
-- the first argument is 'False', or if the given property holds.
(==>) :: Testable prop => Bool -> prop -> Property
False ==> _ = property ()
True  ==> p = property p

-- | Considers a property failed if it does not complete within
-- the given number of microseconds.
within :: Testable prop => Int -> prop -> Property
within n = mapIOResult race
 where
  race ior =
    do put "Race starts ..."
       resV <- newEmptyMVar
       
       let waitAndFail =
             do put "Waiting ..."
                threadDelay n
                put "Done waiting!"
                putMVar resV (failed {reason = "Time out"})
           
           evalProp =
             do put "Evaluating Result ..."
                res <- protectResult ior
                put "Evaluating OK ..."
                putMVar resV res
       
       pid1  <- forkIO evalProp
       pid2  <- forkIO waitAndFail

       put "Blocking ..."
       res <- takeMVar resV
       put "Killing threads ..."
       killThread pid1
       killThread pid2
       put ("Got Result: " ++ show (ok res))
       return res
         

  put s | True      = do return ()
        | otherwise = do putStrLn s
                         hFlush stdout

-- | Explicit universal quantification: uses an explicitly given
-- test case generator.
forAll :: (Show a, Testable prop)
       => Gen a -> (a -> prop) -> Property
forAll gen pf =
  gen >>= \x ->
    whenFail (putStrLn (show x)) $
      property (pf x)

-- | Like 'forAll', but tries to shrink the argument for failing test cases.
forAllShrink :: (Show a, Testable prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrink gen shrinker pf =
  gen >>= \x ->
    shrinking shrinker x $ \x' ->
      whenFail (putStrLn (show x')) $
        property (pf x')

(.&.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .&. p2 =
  arbitrary >>= \b ->
    whenFail (putStrLn (if b then "LHS" else "RHS")) $
      if b then property p1 else property p2

{-
-- TODO

(.&&.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .&&. p2 = error "not implemented yet"
-}

--------------------------------------------------------------------------
-- the end.
