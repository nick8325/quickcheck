module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property
import Test.QuickCheck.Text
import Test.QuickCheck.Exception

import System.Random
  ( RandomGen(..)
  , newStdGen
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
     test tm 0 0 rnd True [] (unGen (property p))
 where
  test tm n m rnd exp stamps f
    | n >= maxSuccessTests =
      do if exp then
           putPart tm ("+++ OK, passed " ++ show n ++ " tests")
          else
           putPart tm (bold ("*** Failed!") ++ " Passed " ++ show n ++ " tests (expected failure)")
         success tm n stamps
         return exp
    
    | m >= maxTryTests =
      do putPart tm (bold ("*** Gave up!") ++ " Passed only " ++ show n ++ " tests")
         success tm n stamps
         return False
         
    | otherwise =
      do putTemp tm ( "("
                   ++ number n "test"
                   ++ concat [ "; " ++ show m ++ " discarded" | m > 0 ]
                   ++ ")"
                    )
         (res, ts) <- run (unProp (f rnd1 ((n * maxSize) `div` maxSuccessTests + m `div` 10))) -- (2*n `div` 3)))
         case ok res of
           Just True -> -- successful test
             do test tm (n+1) m rnd2 (expect res) (stamp res:stamps) f
           
           Nothing -> -- discarded test
             do test tm n (m+1) rnd2 (expect res) stamps f
             
           Just False -> -- failed test
             do if expect res
                  then putPart tm (bold "*** Failed! ")
                  else putPart tm "+++ OK, failed as expected. "
                putTemp tm ( short 30 (reason res)
                          ++ " (after "
                          ++ number (n+1) "test"
                          ++ ")..."
                           )
                foundFailure tm n 0 0 res ts
                return (not (expect res))
   where
    (rnd1,rnd2) = split rnd

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
      
    foundFailure tm n k d res ts =
      do callback' res -- callback that is called for every failure
         localMin tm n k d res ts

    localMin tm n k _ res [] =
      do putLine tm ( reason res
                   ++ " (after " ++ number (n+1) "test"
                   ++ concat [ " and " ++ number k "shrink" | k > 0 ]
                   ++ "):  "
                    )
         callback res -- callback that is called for last failure
    
    localMin tm n k d res (t : ts) =
      do (res',ts') <- run t
         putTemp tm ( short 35 (reason res)
                   ++ " (after " ++ number (n+1) "test"
                   ++ concat [ " and "
                            ++ show k
                            ++ concat [ "." ++ show d | d > 0 ]
                            ++ " shrink"
                            ++ (if k == 1 && d == 0 then "" else "s")
                             | k > 0 || d > 0
                             ]
                   ++ ")..."
                    )
         if ok res' == Just False
           then foundFailure tm n (k+1) 0 res' ts'
           else localMin tm n k (d+1) res ts

    success tm n stamps =
      case labels ++ covers of
        []    -> do putLine tm "."
        [pt]  -> do putLine tm ( " ("
                              ++ dropWhile isSpace pt
                              ++ ")."
                               )
        cases -> do putLine tm ":"
                    sequence_ [ putLine tm pt | pt <- cases ]
     where
      labels = reverse
             . sort
             . map (\ss -> (showP ((length ss * 100) `div` n) ++ head ss))
             . group
             . sort
             $ [ concat (intersperse ", " s')
               | s <- stamps
               , let s' = [ t | (t,0) <- s ]
               , not (null s')
               ]
      
      covers = [ ("only " ++ show occurP ++ "% " ++ fst (head lps) ++ "; not " ++ show reqP ++ "%")
               | lps <- groupBy first
                      . sort
                      $ [ lp
                        | lps <- stamps
                        , lp <- maxi lps
                        , snd lp > 0
                        ]
               , let occurP = (100 * length lps) `div` maxSuccessTests
                     reqP   = maximum (map snd lps)
               , occurP < reqP
               ]
      
      (x,_) `first` (y,_) = x == y 

      maxi = map (\lps -> (fst (head lps), maximum (map snd lps)))
           . groupBy first
           . sort

      showP p = (if p < 10 then " " else "") ++ show p ++ "% "

-- | Tests a property returned by an 'IO' action.
quickCheckIO :: Testable prop => IO prop -> IO ()
quickCheckIO iop =
  do p <- iop
     quickCheck p
     
--------------------------------------------------------------------------
-- the end.
