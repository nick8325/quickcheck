import Control.Applicative
import Control.Monad (guard)
import Data.Foldable (for_)
import Data.List (elemIndices)
import Data.Maybe (isJust)
import Data.Tree (Tree (..))
import Test.QuickCheck
import Test.QuickCheck.Rose

data Ty = TyNat | TyArr Ty Ty
  deriving (Show, Eq, Ord)

instance Arbitrary Ty where
    arbitrary = sized arb
      where
        arb 0 = pure TyNat
        arb _ = oneof [ pure TyNat, TyArr <$> arb' <*> arb' ]

        arb' = scale (`div` 2) arbitrary

    shrink TyNat = []
    shrink (TyArr a b) =
        a : b : [ TyArr a' b | a' <- shrink a ] ++ [ TyArr a b' | b' <- shrink b ]

-- de bruijn indexes
data Exp = V Int | Lam Ty Exp | App Exp Exp | Z | S
  deriving (Show, Eq)

-- Generator of types
rty :: RGen Ty
rty = rarbitrary

rexp :: RGen Exp
rexp = rty >>= rExpFromTy []
  where
    rExpFromTy :: [Ty] -> Ty -> RGen Exp
    rExpFromTy ctx ty = rsized (rExpFromTy' ctx ty)

    rExpFromTy' :: [Ty] -> Ty -> Int -> RGen Exp
    rExpFromTy' ctx ty s = roneof' $
        -- variables
        map (pure . V) (elemIndices ty ctx) ++
        -- Z
        [ pure Z | ty == TyNat ] ++
        -- S
        [ pure S | ty == TyArr TyNat TyNat ] ++
        -- App
        [ app
        | s /= 0
        ] ++
        -- Lam
        [ Lam a <$> rExpFromTy' (a : ctx) b s
        | TyArr a b <- [ty]
        ]

      where
        app = do
            ty' <- rty
            App <$> rExpFromTy' ctx (TyArr ty' ty) (s `div` 2)
                <*> rExpFromTy' ctx ty' (s `div` 2)

-- Property which obviosly doesn't holds
expSize :: Exp -> Int
expSize (V _)     = 1
expSize (Lam _ e) = 1 + expSize e
expSize (App f x) = expSize f + expSize x
expSize Z         = 1
expSize S         = 1

-- | Property which holds non-obviously
wellTyped :: Exp -> Maybe Ty
wellTyped = go []
  where
    go :: [Ty] -> Exp -> Maybe Ty
    go ctx (V n) = nth n ctx
    go _   Z     = Just TyNat
    go ctx S     = Just $ TyArr TyNat TyNat
    go ctx (App f x) = do
        TyArr a b <- go ctx f
        a' <- go ctx x
        guard (a == a')
        pure b
    go ctx (Lam t e) = do
        b <- go (t : ctx) e
        pure $ TyArr t b

    nth :: Int -> [a] -> Maybe a
    nth _ []       = Nothing
    nth 0 (x : _)  = Just x
    nth n (_ : xs) = nth (n - 1) xs

main :: IO ()
main = do
    Success {} <- quickCheckResult $ rForAll rexp $ isJust . wellTyped
    Failure {} <- quickCheckResult $ rForAll rexp $ \exp -> expSize exp < 5
    putStrLn "Samples"
    putStrLn "======="
    xs <- sample' (runRGen rexp)
    for_ (take 3 xs) $ \x -> do
        putStrLn $ "expr: " ++ show (rootLabel x)
        putStrLn $ "type: " ++ show (wellTyped $ rootLabel x)
        for_ (subForest x) $ \y ->
            putStrLn $ "- " ++ show (rootLabel y) ++ " : " ++ show (wellTyped $ rootLabel y)
        putStrLn "------"
    pure ()
