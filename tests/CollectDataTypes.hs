{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveLift, TupleSections, CPP #-}
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
import Data.Function

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
  let mkImport dt = printf "import %s -- for %s" (dt_module dt) (dt_type dt)
  missingModules <- fmap (map mkImport . nubBy ((==) `on` dt_module)) $ filterM (\ dt -> isNothing <$> dataTypeType dt) datatypes
  unless (null missingModules) $ error ("Missing the following imports:\n" ++ unlines missingModules)
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
        map (nm,) <$> [d| $propName = forAllBlind (arbitrary :: $ty) (\ x -> x `seq` True) |]

typeBlacklist :: [String]
typeBlacklist = [ "Prelude.IO"
                , "Prelude.ReadS"
                , "Prelude.ShowS"
                , "System.IO.IO"
                , "System.IO.Error.IOError"
                , "Prelude.IOError"
                , "Data.Kind.Type"
                , "Data.Array.Byte.MutableByteArray"
                , "Data.IORef.IORef"
                , "Data.Kind.Constraint"
                , "Data.Unique.Unique"
                , "Data.STRef.STRef"
                , "Data.STRef.Lazy.STRef"
                , "Data.STRef.Strict.STRef"
                , "Data.Void.Void"
                , "Data.Proxy.KProxy"
                , "Data.Monoid.Endo"
                , "Data.Semigroup.Endo"
                , "Data.List.[]" -- This is buggy and annoying
                , "System.IO.HandlePosn"
                , "System.IO.Handle"
                , "Text.Printf.FieldFormatter" -- This is a function type and it
                                               -- requires an annoying coarbitrary instance
                ] ++
                -- These are phantom types used for indexing
                [ "Data.Fixed.E" ++ show i | i <- [0,1,2,3,6,9,12] ] ++
#if MIN_VERSION_base(4,15,0)
                -- Exists but is deprecated
                [ "Semigroup.Option" ] ++
#endif
                -- TODO: Some controversial ones?
                [ "System.IO.Error.IOErrorType" ]


modulePrefixBlacklist :: [String]
modulePrefixBlacklist = [ "GHC"
                        , "Foreign"
                          -- Exports things like MVar etc
                        , "Control.Concurrent"
                          -- Exports ST and RealWorld that we can't support
                        , "Control.Monad.ST"
                          -- Existential wrapper around a Typeable thing, could be supported but would
                          -- be a bit artificially limited to wrapping a bunch of types we can list
                        , "Data.Dynamic"
                          -- We _could_ support this, but it would result in the same problem as with Dynamic
                        , "Data.Typeable"
                        , "Type.Reflection"
                          -- System.Mem.Weak and System.Mem.Stable export pointer types
                          -- we don't support
                        , "System.Mem"
                          -- Exports an exception
                        , "System.Timeout"
                        -- Exports types, but not the constructors (or ways of creating them, e.g. Number).
                        -- No feasible way to create meaningful generator
                        , "Text.Read"
                        ] ++
                        -- TODO: Some controversial ones thrown in for now to simplify things, should be removed
                        -- later
                        [ "Control.Exception"
                        , "System.Posix"
                        , "Data.Data"
                        , "Text.ParserCombinators"
                        ]

isValidModule :: String -> Bool
isValidModule mod = not $ any (`isPrefixOf` mod) modulePrefixBlacklist
