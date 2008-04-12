module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property
import Test.QuickCheck.Text
import Test.QuickCheck.State
import Test.QuickCheck.Exception

import System.Random
  ( RandomGen(..)
  , newStdGen
  , StdGen
  )

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , group
  , groupBy
  , intersperse
  )

-- * Running tests

--------------------------------------------------------------------------
-- quickCheck

-- | Tests a property and prints the results to 'stdout'.
quickCheck :: Testable prop => prop -> IO ()
quickCheck p = quickCheck' p >> return ()

-- | Tests a property returned by an 'IO' action.
quickCheckIO :: Testable prop => IO prop -> IO ()
quickCheckIO iop =
  do p <- iop
     quickCheck p
     
-- | Tests a property and prints the results to 'stdout'.
quickCheck' :: Testable prop => 
               prop 
            -> IO Bool -- ^ 'True' if the property held for all the tests.
                       -- 'False' if some test failed, or if the test data 
                       -- generator gave up.
quickCheck' p = quickCheckWith maxSuccessTests maxTryTests maxSize p
 where
  maxSuccessTests = 100
  maxTryTests     = 5 * maxSuccessTests
  maxSize         = 100

-- | Tests a property and prints the results to 'stdout'.
-- Allows control of the test parameters.
quickCheckWith :: Testable prop => 
                  Int  -- ^ Maximum number of tests to run.
               -> Int  -- ^ Maximum number of attempts to make to 
                       -- when trying to generate test data.
               -> Int  -- ^ The maximum size of the generated test cases.
               -> prop -- ^ The property to test.
               -> IO Bool -- ^ 'True' if the property held for all the tests.
                          -- 'False' if some test failed, or if the test data 
                          -- generator gave up.
quickCheckWith maxSuccessTests maxTryTests maxSize p =
  do tm  <- newTerminal
     rnd <- newStdGen
     test MkState{ terminal          = tm
                 , maxSuccessTests   = maxSuccessTests
                 , maxTryTests       = maxTryTests
                 , maxSize           = maxSize
                 , numSuccessTests   = 0
                 , numTryTests       = 0
                 , collected         = []
                 , expectedFailure   = False
                 , randomSeed        = rnd
                 , isShrinking       = False
                 , numSuccessShrinks = 0
                 , numTryShrinks     = 0
                 } (unGen (property p))

--------------------------------------------------------------------------
-- main test loop

test :: State -> (StdGen -> Int -> Prop) -> IO Bool
test st f
  | numSuccessTests st >= maxSuccessTests st = doneTesting st f
  | numSuccessTests st >= maxTryTests st     = giveUp st f
  | otherwise                                = runATest st f

doneTesting :: State -> (StdGen -> Int -> Prop) -> IO Bool
doneTesting st f =
  do -- PRE-CALL final
     if expectedFailure st then
       putPart (terminal st)
         ( "+++ OK, passed "
        ++ show (numSuccessTests st)
        ++ " tests"
         )
      else
       putPart (terminal st)
         ( bold ("*** Failed!")
        ++ " Passed "
        ++ show (numSuccessTests st)
        ++ " tests (expected failure)"
         )
     success st
     return (expectedFailure st)
  
giveUp :: State -> (StdGen -> Int -> Prop) -> IO Bool
giveUp st f =
  do putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     return False

runATest :: State -> (StdGen -> Int -> Prop) -> IO Bool
runATest st f =
  do -- PRE-CALL before_test
     putTemp (terminal st)
        ( "("
       ++ number (numSuccessTests st) "test"
       ++ concat [ "; " ++ show (numTryTests st) ++ " discarded" | numTryTests st > 0 ]
       ++ ")"
        )
     let size = (numSuccessTests st * maxSize st) `div` maxSuccessTests st
              + (numTryTests st `div` 10)
     (res, ts) <- run (unProp (f rnd1 size))
     -- POST-CALL after_test
     callbackPostTest st res
     case ok res of
       Just True -> -- successful test
         do test st{ numSuccessTests = numSuccessTests st + 1
                   , randomSeed      = rnd2
                   , collected       = stamp res : collected st
                   , expectedFailure = expect res
                   } f
       
       Nothing -> -- discarded test
         do test st{ numTryTests     = numTryTests st + 1
                   , randomSeed      = rnd2
                   , expectedFailure = expect res
                   } f
         
       Just False -> -- failed test
         do if expect res
              then putPart (terminal st) (bold "*** Failed! ")
              else putPart (terminal st) "+++ OK, failed as expected. "
            putTemp (terminal st)
              ( short 30 (reason res)
             ++ " (after "
             ++ number (numSuccessTests st+1) "test"
             ++ ")..."
              )
            foundFailure st res ts
            return (not (expect res))
 where
  (rnd1,rnd2) = split (randomSeed st)

success :: State -> IO ()
success st =
  case labels ++ covers of
    []    -> do putLine (terminal st) "."
    [pt]  -> do putLine (terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (terminal st) ":"
                sequence_ [ putLine (terminal st) pt | pt <- cases ]
 where
  labels = reverse
         . sort
         . map (\ss -> (showP ((length ss * 100) `div` numSuccessTests st) ++ head ss))
         . group
         . sort
         $ [ concat (intersperse ", " s')
           | s <- collected st
           , let s' = [ t | (t,0) <- s ]
           , not (null s')
           ]
  
  covers = [ ("only " ++ show occurP ++ "% " ++ fst (head lps) ++ "; not " ++ show reqP ++ "%")
           | lps <- groupBy first
                  . sort
                  $ [ lp
                    | lps <- collected st
                    , lp <- maxi lps
                    , snd lp > 0
                    ]
           , let occurP = (100 * length lps) `div` maxSuccessTests st
                 reqP   = maximum (map snd lps)
           , occurP < reqP
           ]
  
  (x,_) `first` (y,_) = x == y 

  maxi = map (\lps -> (fst (head lps), maximum (map snd lps)))
       . groupBy first
       . sort

  showP p = (if p < 10 then " " else "") ++ show p ++ "% "

-- this was there to take care of exceptions, but it does not seem to be
-- needed anymore?
run rose =
  do MkRose mres ts <- return rose `orElseErr` ("rose", errRose)
     res <- mres `orElseErr` ("mres", errResult failed)
     res <- return (strictOk res) `orElseErr` ("ok", errResult res{ ok = Just False })
     ts <- repairList ts
     return (res, ts)
 where
  errRose       err = MkRose (return (errResult failed err)) []
  errResult res err = res{ reason = "Exception: '" ++ showErr err ++ "'" }

  m `orElseErr` (s,f) = -- either f id `fmap` try m
    do eex <- tryEvaluateIO m
       case eex of
         Left err -> do --putStrLn ("EX: [" ++ s ++ "]")
                        return s -- to make warning go away
                        return (f err)
         Right x  -> do return x
  
  strictOk res =
    (ok res == Just False) `seq` res
  
  repairList xs =
    return xs
    {-
    unsafeInterleaveIO $
      do eexs <- tryEvaluate xs
         case eexs of
           Right (x:xs) -> do xs' <- repairList xs; return (x:xs')
           _            -> do return []
    -}
    
--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: State -> Result -> [Rose (IO Result)] -> IO ()
foundFailure st res ts =
  do localMin st{ numTryShrinks = 0, isShrinking = True } res ts

localMin :: State -> Result -> [Rose (IO Result)] -> IO ()
localMin st res [] =
  do putLine (terminal st)
       ( reason res
      ++ " (after " ++ number (numSuccessTests st+1) "test"
      ++ concat [ " and " ++ number (numSuccessShrinks st) "shrink"
                | numSuccessShrinks st > 0
                ]
      ++ "):  "
       )
     callbackPostFinalFailure st res
     -- POST-CALL final_failure

localMin st res (t : ts) =
  do -- PRE-CALL before_test
     (res',ts') <- run t
     putTemp (terminal st)
       ( short 35 (reason res)
      ++ " (after " ++ number (numSuccessTests st+1) "test"
      ++ concat [ " and "
               ++ show (numSuccessShrinks st)
               ++ concat [ "." ++ show (numTryShrinks st) | numTryShrinks st > 0 ]
               ++ " shrink"
               ++ (if numSuccessShrinks st == 1
                   && numTryShrinks st == 0
                   then "" else "s")
                | numSuccessShrinks st > 0 || numTryShrinks st > 0
                ]
      ++ ")..."
       )
     -- POST-CALL after_test
     callbackPostTest st res'
     if ok res' == Just False
       then foundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } res' ts'
       else localMin st{ numTryShrinks = numTryShrinks st + 1 } res ts

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: State -> Result -> IO ()
callbackPostTest st res =
  sequence_ [ f st res | PostTest f <- callbacks res ]

callbackPostFinalFailure :: State -> Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ f st res | PostFinalFailure f <- callbacks res ]

--------------------------------------------------------------------------
-- the end.
