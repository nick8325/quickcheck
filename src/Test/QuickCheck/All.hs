{-# LANGUAGE Rank2Types, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif

-- | __Note__: the contents of this module are re-exported by
-- "Test.QuickCheck". You do not need to import it directly.
--
-- Test all properties in the current module, using Template Haskell.
-- You need to have a @{-\# LANGUAGE TemplateHaskell \#-}@ pragma in
-- your module for any of these to work.
module Test.QuickCheck.All(
  -- ** Testing all properties in a module
  quickCheckAll,
  verboseCheckAll,
  forAllProperties,
  allProperties,
  -- ** Testing polymorphic properties
  polyQuickCheck,
  polyVerboseCheck,
  monomorphic) where

import Language.Haskell.TH
import Test.QuickCheck.Property hiding (Result)
import Test.QuickCheck.Test
import Data.Char
import Data.List (isPrefixOf, nubBy)
import Control.Monad

import qualified System.IO as S

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
--
-- If you want to use 'polyQuickCheck' in the same file where you defined the
-- property, the same scoping problems pop up as in 'quickCheckAll':
-- see the note there about @return []@.
polyQuickCheck :: Name -> ExpQ
polyQuickCheck x = [| quickCheck |] `appE` monomorphic x

-- | Test a polymorphic property, defaulting all type variables to 'Integer'.
-- This is just a convenience function that combines 'verboseCheck' and 'monomorphic'.
--
-- If you want to use 'polyVerboseCheck' in the same file where you defined the
-- property, the same scoping problems pop up as in 'quickCheckAll':
-- see the note there about @return []@.
polyVerboseCheck :: Name -> ExpQ
polyVerboseCheck x = [| verboseCheck |] `appE` monomorphic x

type Error = forall a. String -> a

-- | Monomorphise an arbitrary property by defaulting all type variables to 'Integer'.
--
-- For example, if @f@ has type @'Ord' a => [a] -> [a]@
-- then @$('monomorphic' 'f)@ has type @['Integer'] -> ['Integer']@.
--
-- If you want to use 'monomorphic' in the same file where you defined the
-- property, the same scoping problems pop up as in 'quickCheckAll':
-- see the note there about @return []@.
monomorphic :: Name -> ExpQ
monomorphic t = do
  ty0 <- fmap infoType (reify t)
  let err msg = error $ msg ++ ": " ++ pprint ty0
  (polys, ctx, ty) <- deconstructType err ty0
  case polys of
    [] -> return (expName t)
    _ -> do
      integer <- [t| Integer |]
      ty' <- monomorphiseType err integer ty
      return (SigE (expName t) ty')

expName :: Name -> Exp
expName n = if isVar n then VarE n else ConE n

-- See section 2.4 of the Haskell 2010 Language Report, plus support for "[]"
isVar :: Name -> Bool
isVar = let isVar' (c:_) = not (isUpper c || c `elem` ":[")
            isVar' _     = True
        in isVar' . nameBase

infoType :: Info -> Type
#if MIN_VERSION_template_haskell(2,11,0)
infoType (ClassOpI _ ty _) = ty
infoType (DataConI _ ty _) = ty
infoType (VarI _ ty _) = ty
#else
infoType (ClassOpI _ ty _ _) = ty
infoType (DataConI _ ty _ _) = ty
infoType (VarI _ ty _ _) = ty
#endif

deconstructType :: Error -> Type -> Q ([Name], Cxt, Type)
deconstructType err (ForallT xs ctx ty) = do
#if MIN_VERSION_template_haskell(2,17,0)
  let plain (PlainTV nm _)        = return nm
      plain (KindedTV nm _ StarT) = return nm
#else
  let plain (PlainTV nm)          = return nm
#  if MIN_VERSION_template_haskell(2,8,0)
      plain (KindedTV nm StarT)   = return nm
#  else
      plain (KindedTV nm StarK)   = return nm
#  endif
#endif
      plain _                     = err "Higher-kinded type variables in type"
  xs' <- mapM plain xs
  return (xs', ctx, ty)
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
--
-- 'forAllProperties' has the same issue with scoping as 'quickCheckAll':
-- see the note there about @return []@.
forAllProperties :: Q Exp -- :: (Property -> IO Result) -> IO Bool
forAllProperties = [| runQuickCheckAll |] `appE` allProperties

-- | List all properties in the current module.
--
-- @$'allProperties'@ has type @[('String', 'Property')]@.
--
-- 'allProperties' has the same issue with scoping as 'quickCheckAll':
-- see the note there about @return []@.
allProperties :: Q Exp
allProperties = do
  Loc { loc_filename = filename } <- location
  when (filename == "<interactive>") $ error "don't run this interactively"
  ls <- runIO (fmap lines (readUTF8File filename))
  let prefixes = map (takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') . dropWhile (\c -> isSpace c || c == '>')) ls
      idents = nubBy (\x y -> snd x == snd y) (filter (("prop_" `isPrefixOf`) . snd) (zip [1..] prefixes))
#if MIN_VERSION_template_haskell(2,8,0)
      warning x = reportWarning ("Name " ++ x ++ " found in source file but was not in scope")
#else
      warning x = report False ("Name " ++ x ++ " found in source file but was not in scope")
#endif
      quickCheckOne :: (Int, String) -> Q [Exp]
      quickCheckOne (l, x) = do
        exists <- (warning x >> return False) `recover` (reify (mkName x) >> return True)
        if exists
         then sequence
          [ tupE
            [ stringE $ x ++ " from " ++ filename ++ ":" ++ show l
            , [| property |] `appE` monomorphic (mkName x)
            ]
          ]
         else return []
  fmap (ListE . concat) (mapM quickCheckOne idents) `sigE` [t| [(String, Property)] |]

readUTF8File name = S.openFile name S.ReadMode >>=
                    set_utf8_io_enc >>=
                    S.hGetContents

-- Deal with UTF-8 input and output.
set_utf8_io_enc :: S.Handle -> IO S.Handle
#if __GLASGOW_HASKELL__ > 611
-- possibly if MIN_VERSION_base(4,2,0)
set_utf8_io_enc h = do S.hSetEncoding h S.utf8; return h
#else
set_utf8_io_enc h = return h
#endif

-- | Test all properties in the current module.
-- The name of the property must begin with @prop_@.
-- Polymorphic properties will be defaulted to 'Integer'.
-- Returns 'True' if all tests succeeded, 'False' otherwise.
--
-- To use 'quickCheckAll', add a definition to your module along
-- the lines of
--
-- > return []
-- > runTests = $quickCheckAll
--
-- and then execute @runTests@.
--
-- Note: the bizarre @return []@ in the example above is needed on
-- GHC 7.8 and later; without it, 'quickCheckAll' will not be able to find
-- any of the properties. For the curious, the @return []@ is a
-- Template Haskell splice that makes GHC insert the empty list
-- of declarations at that point in the program; GHC typechecks
-- everything before the @return []@ before it starts on the rest
-- of the module, which means that the later call to 'quickCheckAll'
-- can see everything that was defined before the @return []@. Yikes!
quickCheckAll :: Q Exp
quickCheckAll = forAllProperties `appE` [| quickCheckResult |]

-- | Test all properties in the current module.
-- This is just a convenience function that combines 'quickCheckAll' and 'verbose'.
--
-- 'verboseCheckAll' has the same issue with scoping as 'quickCheckAll':
-- see the note there about @return []@.
verboseCheckAll :: Q Exp
verboseCheckAll = forAllProperties `appE` [| verboseCheckResult |]

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
