-- | Combinators for constructing properties.
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Property where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Text( showErr, isOneLine, putLine )
import Test.QuickCheck.Exception
import Test.QuickCheck.State hiding (labels)

#ifndef NO_TIMEOUT
import System.Timeout(timeout)
#endif
import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)

--------------------------------------------------------------------------
-- fixities

infixr 0 ==>
infixr 1 .&.
infixr 1 .&&.
infixr 1 .||.

-- The story for exception handling:
--
-- To avoid insanity, we have rules about which terms can throw
-- exceptions when we evaluate them:
--   * A rose tree must evaluate to WHNF without throwing an exception
--   * The 'ok' component of a Result must evaluate to Just True or
--     Just False or Nothing rather than raise an exception
--   * IORose _ must never throw an exception when executed
--
-- Both rose trees and Results may loop when we evaluate them, though,
-- so we have to be careful not to force them unnecessarily.
--
-- We also have to be careful when we use fmap or >>= in the Rose
-- monad that the function we supply is total, or else use
-- protectResults afterwards to install exception handlers. The
-- mapResult function on Properties installs an exception handler for
-- us, though.
--
-- Of course, the user is free to write "error "ha ha" :: Result" if
-- they feel like it. We have to make sure that any user-supplied Rose
-- Results or Results get wrapped in exception handlers, which we do by:
--   * Making the 'property' function install an exception handler
--     round its argument. This function always gets called in the
--     right places, because all our Property-accepting functions are
--     actually polymorphic over the Testable class so they have to
--     call 'property'.
--   * Installing an exception handler round a Result before we put it
--     in a rose tree (the only place Results can end up).

--------------------------------------------------------------------------
-- * Property and Testable types

-- | The type of properties.
--
-- Backwards combatibility note: in older versions of QuickCheck
-- 'Property' was a type synonym for @'Gen' 'Prop'@, so you could mix
-- and match property combinators and 'Gen' monad operations. Code
-- that does this will no longer typecheck.
-- However, it is easy to fix: because of the 'Testable' typeclass, any
-- combinator that expects a 'Property' will also accept a @'Gen' 'Property'@.
-- If you have a 'Property' where you need a @'Gen' 'a'@, simply wrap
-- the property combinator inside a 'return' to get a @'Gen' 'Property'@, and
-- all should be well.
newtype Property = MkProperty { unProperty :: Gen Prop }

-- | The class of things which can be tested, i.e. turned into a property.
class Testable prop where
  -- | Convert the thing to a property.
  property :: prop -> Property

-- | If a property returns 'Discard', the current test case is discarded,
-- the same as if a precondition was false.
data Discard = Discard

instance Testable Discard where
  property _ = property rejected

instance Testable Bool where
  property = property . liftBool

instance Testable Result where
  property = MkProperty . return . MkProp . protectResults . return

instance Testable Prop where
  property (MkProp r) = MkProperty . return . MkProp . ioRose . return $ r

instance Testable prop => Testable (Gen prop) where
  property mp = MkProperty $ do p <- mp; unProperty (property p)

instance Testable Property where
  property = property . unProperty

-- | Do I/O inside a property. This can obviously lead to unrepeatable
-- testcases, so use with care.
{-# DEPRECATED morallyDubiousIOProperty "Use ioProperty instead" #-}
morallyDubiousIOProperty :: Testable prop => IO prop -> Property
morallyDubiousIOProperty = ioProperty -- Silly names aren't all they're cracked up to be :)

-- | Do I/O inside a property. This can obviously lead to unrepeatable
-- testcases, so use with care.
--
-- For more advanced monadic testing you may want to look at
-- "Test.QuickCheck.Monadic".
--
-- Note that if you use 'ioProperty' on a property of type @IO Bool@,
-- or more generally a property that does no quantification, the property
-- will only be executed once. To test the property repeatedly you must
-- use the 'again' combinator.
ioProperty :: Testable prop => IO prop -> Property
ioProperty = MkProperty . fmap (MkProp . ioRose . fmap unProp) . promote . fmap (unProperty . property)

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property f = forAllShrink arbitrary shrink f

-- ** Exception handling
protect :: (AnException -> a) -> IO a -> IO a
protect f x = either f id `fmap` tryEvaluateIO x

--------------------------------------------------------------------------
-- ** Type Prop

newtype Prop = MkProp{ unProp :: Rose Result }

-- ** type Rose

data Rose a = MkRose a [Rose a] | IORose (IO (Rose a))
-- Only use IORose if you know that the argument is not going to throw an exception!
-- Otherwise, try ioRose.
ioRose :: IO (Rose Result) -> Rose Result
ioRose = IORose . protectRose

joinRose :: Rose (Rose a) -> Rose a
joinRose (IORose rs) = IORose (fmap joinRose rs)
joinRose (MkRose (IORose rm) rs) = IORose $ do r <- rm; return (joinRose (MkRose r rs))
joinRose (MkRose (MkRose x ts) tts) =
  -- first shrinks outer quantification; makes most sense
  MkRose x (map joinRose tts ++ ts)
  -- first shrinks inner quantification: terrible
  --MkRose x (ts ++ map joinRose tts)

instance Functor Rose where
  -- f must be total
  fmap f (IORose rs)   = IORose (fmap (fmap f) rs)
  fmap f (MkRose x rs) = MkRose (f x) [ fmap f r | r <- rs ]

instance Applicative Rose where
  pure = return
  -- f must be total
  (<*>) = liftM2 ($)

instance Monad Rose where
  return x = MkRose x []
  -- k must be total
  m >>= k  = joinRose (fmap k m)

-- | Execute the "IORose" bits of a rose tree, returning a tree
-- constructed by MkRose.
reduceRose :: Rose Result -> IO (Rose Result)
reduceRose r@(MkRose _ _) = return r
reduceRose (IORose m) = m >>= reduceRose

-- | Apply a function to the outermost MkRose constructor of a rose tree.
-- The function must be total!
onRose :: (a -> [Rose a] -> Rose a) -> Rose a -> Rose a
onRose f (MkRose x rs) = f x rs
onRose f (IORose m) = IORose (fmap (onRose f) m)

-- | Wrap a rose tree in an exception handler.
protectRose :: IO (Rose Result) -> IO (Rose Result)
protectRose = protect (return . exception "Exception")

-- | Wrap all the Results in a rose tree in exception handlers.
protectResults :: Rose Result -> Rose Result
protectResults = onRose $ \x rs ->
  IORose $ do
    y <- protectResult (return x)
    return (MkRose y (map protectResults rs))

-- ** Result type

-- | Different kinds of callbacks
data Callback
  = PostTest CallbackKind (State -> Result -> IO ())         -- ^ Called just after a test
  | PostFinalFailure CallbackKind (State -> Result -> IO ()) -- ^ Called with the final failing test-case
data CallbackKind = Counterexample    -- ^ Affected by the 'verbose' combinator
                  | NotCounterexample -- ^ Not affected by the 'verbose' combinator

-- | The result of a single test.
data Result
  = MkResult
  { ok           :: Maybe Bool        -- ^ result of the test case; Nothing = discard
  , expect       :: Bool              -- ^ indicates what the expected result of the property is
  , reason       :: String            -- ^ a message indicating what went wrong
  , theException :: Maybe AnException -- ^ the exception thrown, if any
  , abort        :: Bool              -- ^ if True, the test should not be repeated
  , labels       :: Map String Int    -- ^ all labels used by this property
  , stamp        :: Set String        -- ^ the collected values for this test case
  , callbacks    :: [Callback]        -- ^ the callbacks for this test case
  }

exception :: String -> AnException -> Result
exception msg err
  | isDiscard err = rejected
  | otherwise = failed{ reason = formatException msg err,
                        theException = Just err }

formatException :: String -> AnException -> String
formatException msg err = msg ++ ":" ++ format (show err)
  where format xs | isOneLine xs = " '" ++ xs ++ "'"
                  | otherwise = "\n" ++ unlines [ "  " ++ l | l <- lines xs ]

protectResult :: IO Result -> IO Result
protectResult = protect (exception "Exception")

succeeded, failed, rejected :: Result
(succeeded, failed, rejected) =
  (result{ ok = Just True },
   result{ ok = Just False },
   result{ ok = Nothing })
  where
    result =
      MkResult
      { ok           = undefined
      , expect       = True
      , reason       = ""
      , theException = Nothing
      , abort        = True
      , labels       = Map.empty
      , stamp        = Set.empty
      , callbacks    = []
      }

--------------------------------------------------------------------------
-- ** Lifting and mapping functions

liftBool :: Bool -> Result
liftBool True = succeeded
liftBool False = failed { reason = "Falsifiable" }

mapResult :: Testable prop => (Result -> Result) -> prop -> Property
mapResult f = mapRoseResult (protectResults . fmap f)

mapTotalResult :: Testable prop => (Result -> Result) -> prop -> Property
mapTotalResult f = mapRoseResult (fmap f)

-- f here mustn't throw an exception (rose tree invariant).
mapRoseResult :: Testable prop => (Rose Result -> Rose Result) -> prop -> Property
mapRoseResult f = mapProp (\(MkProp t) -> MkProp (f t))

mapProp :: Testable prop => (Prop -> Prop) -> prop -> Property
mapProp f = MkProperty . fmap f . unProperty . property

--------------------------------------------------------------------------
-- ** Property combinators

-- | Changes the maximum test case size for a property.
mapSize :: Testable prop => (Int -> Int) -> prop -> Property
mapSize f p = MkProperty (sized ((`resize` unProperty (property p)) . f))

-- | Shrinks the argument to property if it fails. Shrinking is done
-- automatically for most types. This is only needed when you want to
-- override the default behavior.
shrinking :: Testable prop =>
             (a -> [a])  -- ^ 'shrink'-like function.
          -> a           -- ^ The original argument
          -> (a -> prop) -> Property
shrinking shrinker x0 pf = MkProperty (fmap (MkProp . joinRose . fmap unProp) (promote (props x0)))
 where
  props x =
    MkRose (unProperty (property (pf x))) [ props x' | x' <- shrinker x ]

-- | Disables shrinking for a property altogether.
noShrinking :: Testable prop => prop -> Property
noShrinking = mapRoseResult (onRose (\res _ -> MkRose res []))

-- | Adds a callback
callback :: Testable prop => Callback -> prop -> Property
callback cb = mapTotalResult (\res -> res{ callbacks = cb : callbacks res })

-- | Adds the given string to the counterexample.
counterexample :: Testable prop => String -> prop -> Property
counterexample s =
  callback $ PostFinalFailure Counterexample $ \st _res -> do
    res <- tryEvaluateIO (putLine (terminal st) s)
    case res of
      Left err ->
        putLine (terminal st) (formatException "Exception thrown while printing test case" err)
      Right () ->
        return ()

-- | Adds the given string to the counterexample.
{-# DEPRECATED printTestCase "Use counterexample instead" #-}
printTestCase :: Testable prop => String -> prop -> Property
printTestCase = counterexample

-- | Performs an 'IO' action after the last failure of a property.
whenFail :: Testable prop => IO () -> prop -> Property
whenFail m =
  callback $ PostFinalFailure NotCounterexample $ \_st _res ->
    m

-- | Performs an 'IO' action every time a property fails. Thus,
-- if shrinking is done, this can be used to keep track of the
-- failures along the way.
whenFail' :: Testable prop => IO () -> prop -> Property
whenFail' m =
  callback $ PostTest NotCounterexample $ \_st res ->
    if ok res == Just False
      then m
      else return ()

-- | Prints out the generated testcase every time the property is tested.
-- Only variables quantified over /inside/ the 'verbose' are printed.
verbose :: Testable prop => prop -> Property
verbose = mapResult (\res -> res { callbacks = newCallbacks (callbacks res) ++ callbacks res })
  where newCallbacks cbs =
          PostTest Counterexample (\st res -> putLine (terminal st) (status res ++ ":")):
          [ PostTest Counterexample f | PostFinalFailure Counterexample f <- cbs ]
        status MkResult{ok = Just True} = "Passed"
        status MkResult{ok = Just False} = "Failed"
        status MkResult{ok = Nothing} = "Skipped (precondition false)"

-- | Indicates that a property is supposed to fail.
-- QuickCheck will report an error if it does not fail.
expectFailure :: Testable prop => prop -> Property
expectFailure = mapTotalResult (\res -> res{ expect = False })

-- | Modifies a property so that it only will be tested once.
once :: Testable prop => prop -> Property
once = mapTotalResult (\res -> res{ abort = True })

-- | Undoes the effect of 'once'.
again :: Testable prop => prop -> Property
again = mapTotalResult (\res -> res{ abort = False })

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

-- | Checks that at least the given proportion of /successful/ test
-- cases belong to the given class. Discarded tests (i.e. ones
-- with a false precondition) do not affect coverage.
cover :: Testable prop =>
         Bool   -- ^ @True@ if the test case belongs to the class.
      -> Int    -- ^ The required percentage (0-100) of test cases.
      -> String -- ^ Label for the test case class.
      -> prop -> Property
cover x n s =
  x `seq` n `seq` s `listSeq`
  mapTotalResult $
    \res -> res {
      labels = Map.insertWith max s n (labels res),
      stamp = if x then Set.insert s (stamp res) else stamp res }
  where [] `listSeq` z = z
        (x:xs) `listSeq` z = x `seq` xs `listSeq` z

-- | Implication for properties: The resulting property holds if
-- the first argument is 'False' (in which case the test case is discarded),
-- or if the given property holds.
(==>) :: Testable prop => Bool -> prop -> Property
False ==> _ = property Discard
True  ==> p = property p

-- | Considers a property failed if it does not complete within
-- the given number of microseconds.
within :: Testable prop => Int -> prop -> Property
within n = mapRoseResult f
  -- We rely on the fact that the property will catch the timeout
  -- exception and turn it into a failed test case.
  where
    f rose = ioRose $ do
      let m `orError` x = fmap (fromMaybe (error x)) m
      MkRose res roses <- timeout n (reduceRose rose) `orError`
                          "within: timeout exception not caught in Rose Result"
      res' <- timeout n (protectResult (return res)) `orError`
              "within: timeout exception not caught in Result"
      return (MkRose res' (map f roses))
#ifdef NO_TIMEOUT
    timeout _ = fmap Just
#endif

-- | Explicit universal quantification: uses an explicitly given
-- test case generator.
forAll :: (Show a, Testable prop)
       => Gen a -> (a -> prop) -> Property
forAll gen pf =
  again $
  MkProperty $
  gen >>= \x ->
    unProperty (counterexample (show x) (pf x))

-- | Like 'forAll', but tries to shrink the argument for failing test cases.
forAllShrink :: (Show a, Testable prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrink gen shrinker pf =
  again $
  MkProperty $
  gen >>= \x ->
    unProperty $
    shrinking shrinker x $ \x' ->
      counterexample (show x') (pf x')

-- | Nondeterministic choice: 'p1' '.&.' 'p2' picks randomly one of
-- 'p1' and 'p2' to test. If you test the property 100 times it
-- makes 100 random choices.
(.&.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .&. p2 =
  again $
  MkProperty $
  arbitrary >>= \b ->
    unProperty $
    counterexample (if b then "LHS" else "RHS") $
      if b then property p1 else property p2

-- | Conjunction: 'p1' '.&&.' 'p2' passes if both 'p1' and 'p2' pass.
(.&&.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .&&. p2 = conjoin [property p1, property p2]

-- | Take the conjunction of several properties.
conjoin :: Testable prop => [prop] -> Property
conjoin ps =
  again $
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . property) ps
     return (MkProp (conj id roses))
 where
  conj k [] =
    MkRose (k succeeded) []

  conj k (p : ps) = IORose $ do
    rose@(MkRose result _) <- reduceRose p
    case ok result of
      _ | not (expect result) ->
        return (return failed { reason = "expectFailure may not occur inside a conjunction" })
      Just True -> return (conj (addLabels result . addCallbacks result . k) ps)
      Just False -> return rose
      Nothing -> do
        rose2@(MkRose result2 _) <- reduceRose (conj (addCallbacks result . k) ps)
        return $
          -- Nasty work to make sure we use the right callbacks
          case ok result2 of
            Just True -> MkRose (result2 { ok = Nothing }) []
            Just False -> rose2
            Nothing -> rose2

  addCallbacks result r =
    r { callbacks = callbacks result ++ callbacks r }
  addLabels result r =
    r { labels = Map.unionWith max (labels result) (labels r),
        stamp = Set.union (stamp result) (stamp r) }

-- | Disjunction: 'p1' '.||.' 'p2' passes unless 'p1' and 'p2' simultaneously fail.
(.||.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .||. p2 = disjoin [property p1, property p2]

-- | Take the disjunction of several properties.
disjoin :: Testable prop => [prop] -> Property
disjoin ps =
  again $
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . property) ps
     return (MkProp (foldr disj (MkRose failed []) roses))
 where
  disj :: Rose Result -> Rose Result -> Rose Result
  disj p q =
    do result1 <- p
       case ok result1 of
         _ | not (expect result1) -> return expectFailureError
         Just True -> return result1
         Just False -> do
           result2 <- q
           return $
             case ok result2 of
               _ | not (expect result2) -> expectFailureError
               Just True -> result2
               Just False ->
                 MkResult {
                   ok = Just False,
                   expect = True,
                   reason = sep (reason result1) (reason result2),
                   theException = theException result1 `mplus` theException result2,
                   -- The following three fields are not important because the
                   -- test case has failed anyway
                   abort = False,
                   labels = Map.empty,
                   stamp = Set.empty,
                   callbacks =
                     callbacks result1 ++
                     [PostFinalFailure Counterexample $ \st _res -> putLine (terminal st) ""] ++
                     callbacks result2 }
               Nothing -> result2
         Nothing -> do
           result2 <- q
           return (case ok result2 of
                     _ | not (expect result2) -> expectFailureError
                     Just True -> result2
                     _ -> result1)

  expectFailureError = failed { reason = "expectFailure may not occur inside a disjunction" }
  sep [] s = s
  sep s [] = s
  sep s s' = s ++ ", " ++ s'

-- | Like '==', but prints a counterexample when it fails.
infix 4 ===
(===) :: (Eq a, Show a) => a -> a -> Property
x === y =
  counterexample (show x ++ " /= " ++ show y) (x == y)


--------------------------------------------------------------------------
-- the end.
