{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Test.QuickCheck.TH where

import Language.Haskell.TH
import Test.QuickCheck.Test
import Test.QuickCheck.Poly
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

polyQuickCheck :: Name -> ExpQ
polyQuickCheck x = [| quickCheck $(mono x) |]

type Error = forall a. String -> a

mono :: Name -> ExpQ
mono t = do
  ty0 <- fmap infoType (reify t)
  let err msg = error $ msg ++ ": " ++ pprint ty0
  (polys, ctx, ty) <- deconstructType err ty0
  case polys of
    [] -> return (VarE t)
    _ -> do
      monos <- sequence [[t| OrdA |], [t| OrdB |], [t| OrdC |]]
      when (length polys > length monos) $ err "Too many type variables in type"
      ty' <- substitute err (zip polys monos) ty
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

substitute :: Error -> [(Name, Type)] -> Type -> TypeQ
substitute err subst ty@(VarT n) = return (fromMaybe ty (lookup n subst))
substitute err subst (AppT t1 t2) = liftM2 AppT (substitute err subst t1) (substitute err subst t2)
substitute err subst ty@(ForallT _ _ _) = err $ "Higher-ranked type"
substitute err subst ty = return ty

addQuickCheckAll :: Q [Dec]
addQuickCheckAll = do
  Loc { loc_filename = filename } <- location
  when (filename == "<interactive>") $ error "don't run this interactively"
  ls <- runIO (fmap lines (readFile filename))
  let prefixes = map (takeWhile (\c -> isAlphaNum c || c == '_') . dropWhile (\c -> isSpace c || c == '>')) ls
      idents = nubBy (\x y -> snd x == snd y) (filter (("prop_" `isPrefixOf`) . snd) (zip [1..] prefixes))
      quickCheckOne :: (Int, String) -> Q [Exp]
      quickCheckOne (l, x) = do
        exists <- return False `recover` (reify (mkName x) >> return True)
        if exists then
          sequence [
            [| putStrLn $(stringE $ "Checking " ++ x ++ " on line " ++ show l ++ "...") |],
            polyQuickCheck (mkName x) ]
          else return []
  [d|
      quickCheckAll :: IO ()
      quickCheckAll = $(fmap (DoE . map NoBindS . concat) (mapM quickCheckOne idents)) |]
