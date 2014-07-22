{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck

import Control.Monad
  ( liftM
  , liftM2
  )

import Data.Char
  ( toUpper
  )

import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------
-- types for lambda expressions

-- variables

newtype Var = MkVar String
  deriving ( Eq, Ord )

instance Show Var where
  show (MkVar s) = s

varList :: [Var]
varList = [ MkVar s
          | let vs = [ c:v | v <- "" : vs, c <- ['a'..'z'] ]
          , s <- vs
          ]

instance Arbitrary Var where
  arbitrary = growingElements [ MkVar [c] | c <- ['a'..'z'] ]

-- constants

newtype Con = MkCon String
  deriving ( Eq, Ord )

instance Show Con where
  show (MkCon s) = s

instance Arbitrary Con where
  arbitrary = growingElements [ MkCon [c] | c <- ['A'..'Z'] ]

-- expressions

data Exp
  = Lam Var Exp
  | App Exp Exp
  | Var Var
  | Con Con
 deriving ( Eq, Ord )

instance Show Exp where
  showsPrec n (Lam x t) = showParen (n>0) (showString "\\" . shows x . showString "." . shows t)
  showsPrec n (App s t) = showParen (n>1) (showsPrec 1 s . showString " " . showsPrec 2 t)
  showsPrec _ (Var x)   = shows x
  showsPrec _ (Con c)   = shows c

instance Arbitrary Exp where
  arbitrary = sized arbExp
   where
    arbExp n =
      frequency $
        [ (2, liftM Var arbitrary)
        , (1, liftM Con arbitrary)
        ] ++
        concat
        [ [ (5, liftM2 Lam arbitrary arbExp1)
          , (5, liftM2 App arbExp2 arbExp2)
          ]
        | n > 0
        ]
       where
        arbExp1 = arbExp (n-1)
        arbExp2 = arbExp (n `div` 2)

  shrink (Lam x a) = [ a ]
                  ++ [ Lam x a' | a' <- shrink a ]
  shrink (App a b) = [ a, b ]
                  ++ [ ab
                     | Lam x a' <- [a]
                     , let ab = subst x b a'
                     , length (show ab) < length (show (App a b))
                     ]
                  ++ [ App a' b | a' <- shrink a ]
                  ++ [ App a b' | b' <- shrink b ]
  shrink (Var x)   = [Con (MkCon (map toUpper (show x)))]
  shrink _         = []

--------------------------------------------------------------------------
-- functions for lambda expressions

free :: Exp -> Set Var
free (Lam x a) = Set.delete x (free a)
free (App a b) = free a `Set.union` free b
free (Var x)   = Set.singleton x
free (Con _)   = Set.empty

subst :: Var -> Exp -> Exp -> Exp
subst x c (Var y)   | x == y = c
subst x b (Lam y a) | x /= y = Lam y (subst x b a)
subst x c (App a b)          = App (subst x c a) (subst x c b)
subst x c a                  = a

fresh :: Var -> Set Var -> Var
fresh x ys = head (filter (`Set.notMember` ys) (x:varList))

rename :: Var -> Var -> Exp -> Exp
rename x y a | x == y    = a
             | otherwise = subst x (Var y) a

-- different bugs:
--subst x b (Lam y a) | x /= y = Lam y (subst x b a) -- bug 1
--subst x b (Lam y a) | x /= y = Lam y' (subst x b (rename y y' a)) where y':_ = (y:varList) \\ free b -- bug 2
--subst x b (Lam y a) | x /= y = Lam y' (subst x b (rename y y' a)) where y' = (y:varList) \\ (x:free b) -- bug 3
--subst x b (Lam y a) | x /= y = Lam y' (subst x b (rename y y' a)) where y' = fresh y (x:free b) -- bug 4
--subst x c (Lam y a) | x /= y = Lam y' (subst x c (rename y y' a)) where y' = fresh y (x `insert` delete y (free a) `union` free c)

--------------------------------------------------------------------------
-- properties for substitutions

showResult :: (Show a, Testable prop) => a -> (a -> prop) -> Property
showResult x f =
  whenFail (putStrLn ("Result: " ++ show x)) $
    f x

prop_SubstFreeNoVarCapture a x b =
  showResult (subst x b a) $ \subst_x_b_a ->
    x `Set.member` free_a ==>
      free subst_x_b_a == (Set.delete x free_a `Set.union` free b)
 where
  free_a = free a

prop_SubstNotFreeSame a x b =
  showResult (subst x b a) $ \subst_x_b_a ->
    x `Set.notMember` free a ==>
      subst_x_b_a == a

prop_SubstNotFreeSameVars a x b =
  showResult (subst x b a) $ \subst_x_b_a ->
    x `Set.notMember` free a ==>
      free subst_x_b_a == free a

main1 =
  do quickCheck prop_SubstFreeNoVarCapture
     quickCheck prop_SubstNotFreeSame
     quickCheck prop_SubstNotFreeSameVars

--expectFailure $








--------------------------------------------------------------------------
-- eval

eval :: Exp -> Exp
eval (Var x)   = error "eval: free variable"
eval (App a b) =
  case eval a of
    Lam x a' -> eval (subst x b a')
    a'       -> App a' (eval b)
eval a         = a

--------------------------------------------------------------------------
-- closed lambda expressions

newtype ClosedExp = Closed Exp deriving ( Show )

instance Arbitrary ClosedExp where
  arbitrary = Closed `fmap` sized (arbExp [])
   where
    arbExp xs n =
      frequency $
        [ (8, liftM Var (elements xs))
        | not (null xs)
        ] ++
        [ (2, liftM Con arbitrary)
        ] ++
        [ (20, do x <- arbitrary
                  t <- arbExp (x:xs) n'
                  return (Lam x t))
        | n > 0 || null xs
        ] ++
        [ (20, liftM2 App (arbExp xs n2) (arbExp xs n2))
        | n > 0
        ]
     where
      n' = n-1
      n2 = n `div` 2

  shrink (Closed a) =
    [ Closed a' | a' <- shrink a, Set.null (free a') ]

--------------------------------------------------------------------------
-- properties for closed lambda expressions

isValue :: Exp -> Bool
isValue (Var _)           = False
isValue (App (Lam _ _) _) = False
isValue (App a b)         = isValue a && isValue b
isValue _                 = True

prop_ClosedExpIsClosed (Closed a) =
  Set.null (free a)

prop_EvalProducesValue (Closed a) =
  within 1000 $
    isValue (eval a)

main2 =
  do quickCheck prop_ClosedExpIsClosed
     quickCheck prop_EvalProducesValue

--  expectFailure $

--------------------------------------------------------------------------
-- main

main =
  do main1
     main2

--------------------------------------------------------------------------
-- the end.

{-
instance Arbitrary Exp where
  arbitrary = sized (arbExp [])
   where

  arbitrary = repair [] `fmap` sized arbExp
   where
    arbExp n =
      frequency $
        [ (1, liftM Var arbitrary)
        ] ++ concat
        [ [ (3, liftM2 Lam arbitrary   (arbExp n'))
          , (4, liftM2 App (arbExp n2) (arbExp n2))
          ]
        | n > 0
        ]
     where
      n' = n-1
      n2 = n `div` 2

    repair xs (Var x)
      | x `elem` xs = Var x
      | null xs     = Lam x (Var x)
      | otherwise   = Var (xs !! (ord (last (show x)) `mod` length xs))
    repair xs (App a b) = App (repair xs a) (repair xs b)
    repair xs (Lam x a) = Lam x (repair (x:xs) a)

  -- lots of clever shrinking added
  shrinkRec (Lam x a) = [ a | x `notElem` free a ]
  shrinkRec (App a b) = [ a, b ]
                     ++ [ red
                        | Lam x a' <- [a]
                        , let red = subst x b a'
                        , length (show red) < length (show (App a b))
                        ]
  shrinkRec (Var x)   = [Con (MkCon (map toUpper (show x)))]
  shrinkRec _         = []

-- types

data Type
  = Base Con
  | Type :-> Type
 deriving ( Eq, Show )

instance Arbitrary Type where
  arbitrary = sized arbType
   where
    arbType n =
      frequency $
        [ (1, liftM Base arbitrary)
        ] ++
        [ (4, liftM2 (:->) arbType2 arbType2)
        | n > 0
        ]
     where
      arbType2 = arbType (n `div` 2)

newtype WellTypedExp = WellTyped Exp
 deriving ( Eq, Show )

arbExpWithType n env t =
  frequency $
    [ (2, liftM Var (elements xs))
    | let xs = [ x | (x,t') <- env, t == t' ]
    , not (null xs)
    ] ++
    [ (1, return (Con b))
    | Base b <- [t]
    ] ++
    [ (if n > 0 then 5 else 1
        , do x <- arbitrary
             b <- arbExpWithType n1 ((x,ta):[ xt | xt <- env, fst xt /= x ]) tb
             return (Lam x b))
    | ta :-> tb <- [t]
    ] ++
    [ (5, do tb <- arbitrary
             a <- arbExpWithType n2 env (tb :-> t)
             b <- arbExpWithType n2 env tb
             return (App a b))
    | n > 0
    ]
   where
    n1 = n-1
    n2 = n `div` 2

instance Arbitrary WellTypedExp where
  arbitrary =
    do t <- arbitrary
       e <- sized (\n -> arbExpWithType n [] t)
       return (WellTyped e)

  shrink _ = []

newtype OpenExp = Open Exp
 deriving ( Eq, Show )

instance Arbitrary OpenExp where
  arbitrary = Open `fmap` sized arbExp
   where
    arbExp n =
      frequency $
        [ (2, liftM Var arbitrary)
        , (1, liftM Con arbitrary)
        ] ++
        concat
        [ [ (5, liftM2 Lam arbitrary arbExp1)
          , (5, liftM2 App arbExp2 arbExp2)
          ]
        | n > 0
        ]
       where
        arbExp1 = arbExp (n-1)
        arbExp2 = arbExp (n `div` 2)

  shrink (Open a) = map Open (shrink a)

prop_EvalProducesValueWT (WellTyped a) =
  isValue (eval a)

-}

x = MkVar "x"
y = MkVar "y"

