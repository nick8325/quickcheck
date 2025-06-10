{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module CollectDataTypes where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter hiding (lift)
import System.Process
import Data.List.Split
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Text.Printf
import Data.Either

data DataType =
  DataType {
    dt_package :: String,
    dt_module :: String,
    dt_type :: String }
  deriving (Show, Lift)

getPackageDataTypes :: String -> IO [DataType]
getPackageDataTypes pkg = do
  mods <- getPackageModules pkg
  typess <- mapM getModuleDataTypes mods
  return [DataType pkg mod typ | (mod, types) <- zip mods typess, typ <- types]

getPackageModules :: String -> IO [String]
getPackageModules pkg = 
  concatMap (parseWords . words) . splitOn ", " . unwords . words <$> readProcess cmd args ""
  where
    cmd:args = ["stack", "exec", "--", "ghc-pkg", "field", pkg, "exposed-modules", "--simple-output"]
    parseWords [mod, "from", _] = [mod]
    parseWords xs = xs

getModuleDataTypes :: String -> IO [String]
getModuleDataTypes mod = do
  putStrLn mod
  Right names <- runInterpreter (getModuleExports mod)
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

data Result = NotFound | FunnyKind String | NoArbitrary | HasArbitrary deriving (Show, Lift)

doTheMagicStuff :: String -> Q Exp
doTheMagicStuff pkg = do
  datatypes <- runIO (getPackageDataTypes pkg)
  results <- mapM checkDataType datatypes
  lift (zip datatypes results)

checkDataType :: DataType -> Q Result
checkDataType dt = do
  mtype <- dataTypeType dt
  case mtype of
    Nothing -> return NotFound
    Just typ ->
      case typeArity typ of
        Nothing -> return (FunnyKind (show (pprType 0 typ)))
        Just arity -> do
          hasArb <- runIO $ hasArbitraryInstance dt arity
          case hasArb of
            False -> return NoArbitrary
            True -> return HasArbitrary
        
hasArbitraryInstance :: DataType -> Int -> IO Bool
hasArbitraryInstance dt@DataType{..} arity =
  fmap (either (error . show) id) . runInterpreter $ do
    setImportsQ [("Prelude", Nothing), ("Test.QuickCheck", Nothing), (dt_module, Just dt_module)]
    typeChecks test
  where
    test = printf "arbitrary :: Gen (%s)" monotype
    monotype = haskellName dt ++ concat (replicate arity " Int")
