{-# LANGUAGE TemplateHaskell, Rank2Types, CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif
-- | Test all properties in the current module, using Template Haskell.
-- You need to have a @{-\# LANGUAGE TemplateHaskell \#-}@ pragma in
-- your module for any of these to work.
module Test.QuickCheck.All(
  -- ** Testing all properties in a module
  quickCheckAll,
  verboseCheckAll,
  forAllProperties,
  -- ** Testing polymorphic properties
  polyQuickCheck,
  polyVerboseCheck,
  monomorphic) where

import Language.Haskell.TH
import Test.QuickCheck.Property hiding (Result)
import Test.QuickCheck.Test
import Data.Char
import Data.List
import Control.Monad

-- | Test a polymorphic property, defaulting all type variables to 'Integer'.
--
-- Invoke as @$('polyQuickCheck' 'prop)@, where @prop@ is a property.
-- Note that just evaluating @'quickCheck' prop@ in GHCi will seem to
-- work, but will silently default all type variables to @()@!
--
-- @$('polyQuickCheck' \'prop)@ means the same as
-- @'quickCheck' $('monomorphic' \'prop)@.
-- If you want to supply custom arguments to 'polyQuickCheck',
-- you will have to combine 'quickCheckWith' and 'monomorphic' yourself.
polyQuickCheck :: Name -> ExpQ
polyQuickCheck x = [| quickCheck $(monomorphic x) |]

-- | Test a polymorphic property, defaulting all type variables to 'Integer'.
-- This is just a convenience function that combines 'verboseCheck' and 'monomorphic'.
polyVerboseCheck :: Name -> ExpQ
polyVerboseCheck x = [| verboseCheck $(monomorphic x) |]

type Error = forall a. String -> a

-- | Monomorphise an arbitrary property by defaulting all type variables to 'Integer'.
--
-- For example, if @f@ has type @'Ord' a => [a] -> [a]@
-- then @$('monomorphic' 'f)@ has type @['Integer'] -> ['Integer']@.
monomorphic :: Name -> ExpQ
monomorphic t = do
  ty0 <- fmap infoType (reify t)
  let err msg = error $ msg ++ ": " ++ pprint ty0
  (polys, ctx, ty) <- deconstructType err ty0
  case polys of
    [] -> return (VarE t)
    _ -> do
      integer <- [t| Integer |]
      ty' <- monomorphiseType err integer ty
      return (SigE (VarE t) ty')

infoType :: Info -> Type
infoType (ClassOpI _ ty _ _) = ty
infoType (DataConI _ ty _ _) = ty
infoType (VarI _ ty _ _) = ty

deconstructType :: Error -> Type -> Q ([Name], Cxt, Type)
deconstructType err ty0@(ForallT xs ctx ty) = do
  let plain (PlainTV _) = True
      plain _ = False
  unless (all plain xs) $ err "Higher-kinded type variables in type"
  return (map (\(PlainTV x) -> x) xs, ctx, ty)
deconstructType _ ty = return ([], [], ty)

monomorphiseType :: Error -> Type -> Type -> TypeQ
monomorphiseType err mono ty@(VarT n) = return mono
monomorphiseType err mono (AppT t1 t2) = liftM2 AppT (monomorphiseType err mono t1) (monomorphiseType err mono t2)
monomorphiseType err mono ty@(ForallT _ _ _) = err $ "Higher-ranked type"
monomorphiseType err mono ty = return ty

-- | Test all properties in the current module, using a custom
-- 'quickCheck' function. The same caveats as with 'quickCheckAll'
-- apply.
--
-- @$'forAllProperties'@ has type @('Property' -> 'IO' 'Result') -> 'IO' 'Bool'@.
-- An example invocation is @$'forAllProperties' 'quickCheckResult'@,
-- which does the same thing as @$'quickCheckAll'@.
forAllProperties :: Q Exp -- :: (Property -> IO Result) -> IO Bool
forAllProperties = do
  Loc { loc_filename = filename } <- location
  when (filename == "<interactive>") $ error "don't run this interactively"
  ls <- runIO (fmap lines (readFile filename))
  let prefixes = map (takeWhile (\c -> isAlphaNum c || c == '_') . dropWhile (\c -> isSpace c || c == '>')) ls
      idents = nubBy (\x y -> snd x == snd y) (filter (("prop_" `isPrefixOf`) . snd) (zip [1..] prefixes))
      warning x = reportWarning ("Name " ++ x ++ " found in source file but was not in scope")
      quickCheckOne :: (Int, String) -> Q [Exp]
      quickCheckOne (l, x) = do
        exists <- (warning x >> return False) `recover` (reify (mkName x) >> return True)
        if exists then sequence [ [| ($(stringE $ x ++ " from " ++ filename ++ ":" ++ show l),
                                     property $(monomorphic (mkName x))) |] ]
         else return []
  [| runQuickCheckAll $(fmap (ListE . concat) (mapM quickCheckOne idents)) |]

-- | Test all properties in the current module.
-- The name of the property must begin with @prop_@.
-- Polymorphic properties will be defaulted to 'Integer'.
-- Returns 'True' if all tests succeeded, 'False' otherwise.
--
-- Using 'quickCheckAll' interactively doesn't work.
-- Instead, add a definition to your module along the lines of
--
-- > runTests = $quickCheckAll
--
-- and then execute @runTests@.
quickCheckAll :: Q Exp
quickCheckAll = [| $(forAllProperties) quickCheckResult |]

-- | Test all properties in the current module.
-- This is just a convenience function that combines 'quickCheckAll' and 'verbose'.
verboseCheckAll :: Q Exp
verboseCheckAll = [| $(forAllProperties) verboseCheckResult |]

runQuickCheckAll :: [(String, Property)] -> (Property -> IO Result) -> IO Bool
runQuickCheckAll ps qc =
  fmap and . forM ps $ \(xs, p) -> do
    putStrLn $ "=== " ++ xs ++ " ==="
    r <- qc p
    putStrLn ""
    return $ case r of
      Success { } -> True
      Failure { } -> False
      NoExpectedFailure { } -> False
      GaveUp { } -> False
