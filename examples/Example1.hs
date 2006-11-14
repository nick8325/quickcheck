module Main where

--------------------------------------------------------------------------
-- imports

-- QuickCheck
import Chalmers.QuickCheck.Gen
import Chalmers.QuickCheck.Arbitrary
import Chalmers.QuickCheck.Property
import Chalmers.QuickCheck.Test
import Chalmers.QuickCheck.Monadic

-- other
import Data.List
import Control.Monad hiding ( join )

--------------------------------------------------------------------------
-- example

newtype Var = MkVar String deriving ( Eq )

instance Show Var where
  show (MkVar s) = s

data Exp
  = Lam Var Exp
  | App Exp Exp
  | Var Var
 deriving ( Eq )

instance Show Exp where
  showsPrec n (Lam x t) = showParen (n>0) (showString "\\" . shows x . showString "." . shows t)
  showsPrec n (App s t) = showParen (n>1) (showsPrec 1 s . showString " " . showsPrec 2 t)
  showsPrec _ (Var x)   = shows x

free :: Exp -> [Var]
free (Lam x t) = free t \\ [x]
free (App s t) = free s `union` free t
free (Var x)   = [x]

instance Arbitrary Var where
  arbitrary =
    sized $ \n ->
      elements [ MkVar [c] | c <- take (n `div` 2 + 1) ['a'..'z'] ]

  shrinkPrim = fromList . shrinkNothing

instance Arbitrary Exp where
  arbitrary = sized (arbExp [])
   where
    arbExp xs n =
      frequency $
        [ (1, liftM Var (elements xs))
        | not (null xs)
        ] ++
        [ (9, do x <- arbitrary
                 t <- arbExp (x:xs) n'
                 return (Lam x t))
        | n > 0 || null xs
        ] ++
        [ (8, liftM2 App (arbExp xs n2) (arbExp xs n2))
        | n > 0
        ]
     where
      n' = n-1
      n2 = n `div` 2

{-
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
-}

  shrinkRec (Lam x a) = [ a | x `notElem` free a ]
  shrinkRec (App a b) = [ a, b ]
  shrinkRec _         = []

look x xys = head [ y | (x',y) <- xys, x == x' ]
extend x y xys = (x,y) : xys

{-

eval :: [(Var,Exp)] -> Exp -> Exp
eval env a@(Lam _ _) = a
--eval env (Var x)     = look x env -- original bug
eval env (Var x)     = eval env (look x env)
eval env (App a b)   = eval (extend x b env) a'
 where
  Lam x a' = eval env a
-}

{-
eval' :: [(Var,Exp)] -> Exp -> Exp
eval' env a@(Lam _ _) = a
--eval' env (Var x)     = look x env -- original bug
eval' env (Var x)     = eval env (look x env)
eval' env (App a b)   = eval (extend x' b env) c'
 where
  Lam x c = eval env a
  
  c'      = rename 
-}

subst :: Var -> Exp -> Exp -> Exp
subst x b t@(Var y)
  | x == y    = b
  | otherwise = t
subst x b (App s t) = App (subst x b s) (subst x b t)
subst x b t@(Lam y a)
  | x == y    = t
  | otherwise = Lam y' (subst x b (subst y (Var y') a))
 where
  y'   = head ((y : [ MkVar v | v <- tail vars]) \\ free b)
  vars = "" : [ c:v | v <- vars, c <- ['a'..'z'] ]

eval :: Exp -> Exp
eval a@(Lam _ _) = a
eval (App a b)   = eval (subst x b a')
 where
  Lam x a' = eval a

prop_ExpIsClosed a =
  cover (length (show a) < 10) 10 "small" $
    cover (length (show a) > 100) 70 "big" $
      null (free a)
    
prop_ValueIsLambda a =
  within 500 $
    case eval a of
      Lam _ _ -> True
      _       -> False
    
prop_ValueIsLambda' =
  forAllShrink arbitrary (shrink2 shrink) $ \a ->
    within 500 $
      case eval a of
        Lam _ _ -> True
        _       -> False
    
main =
  do quickCheck prop_ValueIsLambda'
     --quickCheck prop_ExpIsClosed
     --quickCheck prop2
     --quickCheck prop_ValueIsLambda'

--------------------------------------------------------------------------
-- the end.
