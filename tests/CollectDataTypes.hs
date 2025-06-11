{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveLift, TupleSections #-}
module CollectDataTypes where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter hiding (lift)
import Data.Maybe
import Data.List
import System.Process
import Data.List.Split
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Text.Printf
import Data.Either
import Data.Char

data DataType =
  DataType {
    dt_package :: String,
    dt_module :: String,
    dt_type :: String }
  deriving (Show, Lift)

getPackageDataTypes :: String -> IO [DataType]
getPackageDataTypes pkg = do
  mods <- filter isValidModule <$> getPackageModules pkg
  typess <- mapM getModuleDataTypes mods
  return [DataType pkg mod typ | (mod, types) <- zip mods typess, typ <- types]

getPackageModules :: String -> IO [String]
getPackageModules pkg =
  concatMap (parseWords . words) . splitOn ", " . unwords . words <$> readProcess cmd args ""
  where
    cmd:args = ["ghc-pkg", "field", pkg, "exposed-modules", "--simple-output"]
    parseWords [mod, "from", _] = [mod]
    parseWords xs = xs

getModuleDataTypes :: String -> IO [String]
getModuleDataTypes mod = do
  putStrLn mod
  Right names <- runInterpreter $ getModuleExports mod
  return [x | Data x _ <- names]

haskellName :: DataType -> String
haskellName DataType{..} = printf "%s.%s" dt_module (stripParens dt_type)
  where
    stripParens = reverse . dropWhile (== ')') . reverse . dropWhile (== '(')

dataTypeType :: DataType -> Q (Maybe Type)
dataTypeType dt = do
  mname <- lookupTypeName (haskellName dt)
  case mname of
    Nothing -> return Nothing
    Just name -> Just <$> reifyType name

typeArity :: Type -> Maybe Int
typeArity (AppT (AppT ArrowT StarT) kind) = succ <$> typeArity kind
typeArity StarT = return 0
typeArity _ = Nothing

createProperties :: String -> Q [Dec]
createProperties pkg = do
  datatypes0 <- runIO (getPackageDataTypes pkg)
  let datatypes = [ dt | dt <- datatypes0, not $ haskellName dt `elem` typeBlacklist ]
  missingModules <- fmap (nub . map dt_module) $ filterM (\ dt -> isNothing <$> dataTypeType dt) datatypes
  unless (null missingModules) $ error ("Missing the following imports:\n" ++ unlines [ "import " ++ m | m <- missingModules ])
  namesAndDecs <- fmap concat $ mapM createProperty datatypes
  let (allNames, props) = unzip namesAndDecs
  allPropsDec <- [d| allProps =
                        $(pure $ ListE [ TupE [Just (LitE (StringL $ nameBase name)), Just (VarE name)]
                                              | name <- allNames ]
                         )
                 |]
  return $ allPropsDec ++ props

createProperty :: DataType -> Q [(Name, Dec)]
createProperty dt = do
  mtype <- dataTypeType dt
  -- TODO: monad?!
  case mtype of
    Nothing -> error $ "Can't find type in scope " ++ show dt
    Just typ -> case typeArity typ of
      Nothing -> pure []
      Just arity -> do
        Just name <- lookupTypeName (haskellName dt)
        Just int <- lookupTypeName "Int"
        Just gen <- lookupTypeName "Gen"
        nm <- newName ("prop_" ++ filter isAlphaNum (haskellName dt))
        let propName = pure $ VarP nm
        let ty = pure $ AppT (ConT gen) $ foldl AppT (ConT name) $ replicate arity (ConT int)
        map (nm,) <$> [d| $propName = forAll (arbitrary :: $ty) (\ x -> x `seq` True) |]

typeBlacklist :: [String]
typeBlacklist = [ "Prelude.IO"
                , "Prelude.ReadS"
                , "Prelude.ShowS"
                , "System.IO.Error.IOError"
                , "Prelude.IOError"
                , "Data.Kind.Type"
                , "Data.Array.Byte.MutableByteArray"
                , "Data.IORef.IORef"
                ]

modulePrefixBlacklist :: [String]
modulePrefixBlacklist = [ "GHC"
                        , "Foreign"
                        , "Control.Concurrent"
                        , "Control.Exception"
                        , "Control.Monad.ST"
                        , "System.Posix"
                        ]

isValidModule :: String -> Bool
isValidModule mod = mod == "Prelude" -- TODO: fixme by actually introducing all the relevant instances
isValidModule mod = not $ any (`isPrefixOf` mod) modulePrefixBlacklist
