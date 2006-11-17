module ExFol where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

import List
import System

data Intp =
  MkIntp
  { size  :: Int
  , preds :: [ (Int, [Int] -> Bool) ]
  , funs  :: [ (Int, [Int] -> Int) ]
  }

instance Arbitrary Intp where
  arbitrary =
    do n  <- choose (2,25)
       ps <- listOf pred
       fs <- listOf (fun n)
       return (MkIntp n ps fs)
   where
    arity =
      elements [0,1,1,1,2,2,3,4]
   
    pred =
      do a <- arity
         p <- arbitrary
         return (a,p)

    fun n =
      do a <- arity
         f <- arbitrary
         return (a, (`mod` n) . f)

-- terms

type Name
  = Int

data Term
  = Var Name
  | Fun Name [Term]
 deriving ( Eq )

instance Show Term where
  showsPrec n (Var v)    = showString "X" . showsPrec n v
  showsPrec n (Fun c []) = showString "c" . showsPrec n c
  showsPrec n (Fun f ts) = showString "f" . showsPrec n f
                         . showString "("
                         . foldr (.) id
                         ( intersperse (showString ",") (map (showsPrec n) ts) )
                         . showString ")"

termOver :: Intp -> [Name] -> Gen Term
termOver intp vs = sized arbTerm
 where
  fs = [0..] `zip` funs intp
  cs = [ ff | ff@(_,(0,_)) <- fs ]
  
  arbTerm n =
    frequency $
      [ (3, do v <- elements vs
               return (Var v))
      | not (null vs)
      ] ++
      [ (2, do (c,_) <- elements cs
               return (Fun c []))
      | not (null cs)
      ] ++
      [ (i, do ts <- sequence [ arbTerm (n `div` (a+1)) | i <- [1..a] ]
               return (Fun f ts))
      | n > 0
      , (f,(a,_)) <- fs
      , a > 0
      , let i = ([4,4,3,1] ++ repeat 1) !! a
      ]

evalTerm :: Intp -> [Int] -> Term -> Int
evalTerm intp ds (Var v)    = ds !!? ("var",v)
evalTerm intp ds (Fun f ts) = fd [ evalTerm intp ds t | t <- ts ]
 where
  (_,fd) = funs intp !!? ("fun",f)

-- atoms

data Atom
  = Pred Name [Term]
  | Term :=: Term
 deriving ( Eq )

instance Show Atom where
  showsPrec n (Pred p []) = showString "p" . showsPrec n p
  showsPrec n (Pred p ts) = showString "p" . showsPrec n p
                          . showString "("
                          . foldr (.) id
                          ( intersperse (showString ",") (map (showsPrec n) ts) )
                          . showString ")"
  showsPrec n (s :=: t)   = showString "equal("
                          . showsPrec n s
                          . showString ","
                          . showsPrec n t
                          . showString ")"

atomOver :: Intp -> [Name] -> Gen Atom
atomOver intp vs =
  frequency $
    [ (2, do s <- termOver intp vs
             t <- termOver intp vs
             return (s :=: t))
    ] ++
    [ (3, do (p,(a,_)) <- elements ps
             ts <- sequence [ termOver intp vs | i <- [1..a] ]
             return (Pred p ts))
    | not (null ps)
    ]
 where
  ps = [0..] `zip` preds intp

evalAtom :: Intp -> [Int] -> Atom -> Bool
evalAtom intp ds (s :=: t)   = evalTerm intp ds s == evalTerm intp ds t
evalAtom intp ds (Pred p ts) = pd [ evalTerm intp ds t | t <- ts ]
 where
  (_,pd) = preds intp !!? ("pred", p)

xs !!? (s,n)
  | n >= 0 && n < length xs = xs !! n
  | otherwise               = error ("!!?: " ++ show (length xs,s,n))

-- literals

data Lit
  = Pos Atom
  | Neg Atom
 deriving ( Eq )

instance Show Lit where
  showsPrec n (Pos a) = showsPrec n a
  showsPrec n (Neg a) = showString "-" . showsPrec n a

litOver :: Intp -> [Name] -> Gen Lit
litOver intp vs =
  do sgn <- elements [Pos,Neg]
     a <- atomOver intp vs
     return (sgn a)

evalLit :: Intp -> [Int] -> Lit -> Bool
evalLit intp ds (Pos a) = evalAtom intp ds a
evalLit intp ds (Neg a) = not (evalAtom intp ds a)

neg :: Lit -> Lit
neg (Pos a) = Neg a
neg (Neg a) = Pos a

-- clauses

type Clause = [Lit]

evalClause :: Intp -> [Int] -> Clause -> Bool
evalClause intp ds cls = or [ evalLit intp ds l | l <- cls ]

clauseSatisfying :: Intp -> Gen Clause
clauseSatisfying intp =
  do nv <- choose (if hasConst then 0 else 1, maxNv)
     let vs = [0..nv-1]
         
         assigns 0 = [[]]
         assigns n = [ d:ds | d <- [0..size intp-1], ds <- assigns (n-1) ]
     
     k <- elements [0,0,0,0,1,1,1,1,2,2,3,4]
     cls <- vectorOf k (litOver intp vs)
     let fix cls [] =
           do return cls
         
         fix cls as =
           do l <- litOver intp vs
              let (as1,as0) = partition (\ds -> evalLit intp ds l) as
              if not (null as1)
                then fix (l:cls) as0
                else fix (neg l:cls) as1
     fix cls [ a | a <- assigns nv, not (evalClause intp a cls) ]
 where
  maxNv = 1 `max` floor (log 5000 / log (fromIntegral (size intp)))
 
  hasConst = or [ a == 0 | (a,_) <- funs intp ]

-- clause sets

clauseSetSat :: Gen [Clause]
clauseSetSat =
  do intp <- arbitrary
     listOf (clauseSatisfying intp)

-- property

prop_Sound =
  mapSize (`div` 4) $
  forAllBlind clauseSetSat $ \clss ->
    monadicIO $
      do mn <- run $
           do --runParadox clss
              --runGandalf clss
              runEprover clss
         maybe (assert False) (monitor . collect) (mn :: Maybe Int)

runParadox clss =
  do writeFile "problem.in" $ unlines $
                [ "set(prolog_style_variables)."
                , "assign(max_seconds,3)."
                , "set(auto)."
                , "list(sos)."
                , "equal(X,X)."
                ] ++
                [ concat (intersperse " | " (map show cls)) ++ "."
                | cls <- clss
                ] ++
                [ "end_of_list."
                ]
     system (paradox ++ " --contr problem.in > paradox.out")
     s <- readFile "paradox.out"
     case words (last (lines s)) of
       _:_:"SATISFIABLE":_:_:n':_ -> do return (Just (read (init n')))
       _                          -> do return Nothing
 where
  paradox = "C:\\Program\\Cygwin\\home\\koen\\Code\\Haskell\\Paradox\\paradox.exe"

runEprover clss =
  do writeFile "problem.in" $ unlines $
       [ "input_clause(axiom,axiom,[++equal(X,X)])."
       ] ++
       [ "input_clause(conjecture,conjecture,[" ++
           concat (intersperse ", " (map showLit cls)) ++ "])."
       | cls <- clss
       , let showLit (Pos a) = "++" ++ show a
             showLit (Neg a) = "--" ++ show a
       ]
     system (eprover ++ " -tAuto -xAuto -C1000 -P1000 -U1000 -T2000 --tptp-in problem.in > eprover.out")
     s <- readFile "eprover.out"
     (s==s) `seq` return ()
{-
     if any (== "# Proof found!") (lines s)
       then return Nothing
       else return (Just 0)
-}
     if any (`elem` lines s) ["# No proof found!", "# Failure: User resource limit exceeded!"]
       then return (Just 0)
       else return Nothing
 where
  eprover = "C:\\Program\\Cygwin\\usr\\local\\packages\\Eprover\\E\\PROVER\\eprover.exe"

runGandalf clss =
  do writeFile "problem.in" $ unlines $
                [ "set(prolog_style_variables)."
                , "assign(max_seconds,3)."
                , "set(auto)."
                , "list(sos)."
                , "equal(X,X)."
                ] ++
                [ concat (intersperse " | " (map show cls)) ++ "."
                | cls <- clss
                ] ++
                [ "end_of_list."
                ]
     system (gandalf ++ " problem.in > gandalf.out 2> gandalf.err")
     s <- readFile "gandalf.out"
{-
     if any (== "***GANDALF_FOUND_A_REFUTATION***") (words s)
       then return Nothing
       else return (Just 0)
-}
     if any (`elem` words s) ["terminated.", "exhausted."]
       then return (Just 0)
       else return Nothing
 where
  gandalf = "C:\\Program\\Cygwin\\usr\\local\\packages\\Gandalf\\gandalf26\\GPLGandalf\\gandalf.exe"
