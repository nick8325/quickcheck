-- Strictness tests.

{-# LANGUAGE CPP, TemplateHaskell #-}
import Test.QuickCheck

import Control.Exception (Exception (..), throw)

#if MIN_VERSION_containers(0,5,0)
import Control.Exception (evaluate, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
#endif

data Thunk

instance Arbitrary Thunk where
  arbitrary = throw ThunkError

instance Show Thunk where
  show _ = "Thunk"

data ThunkError = ThunkError deriving Show

instance Exception ThunkError

prop_strictMap :: Property
#if MIN_VERSION_containers(0,5,0)
prop_strictMap = again . ioProperty $ do
  m <- generate arbitrary
  result <- try $ evaluate m :: IO (Either ThunkError (Map Int Thunk))
  pure $ case result of
    Right _ | not (Map.null m) -> counterexample ("Thunks in Map: " ++ show m) False
    _ -> property True
#else
prop_strictMap = once $ property True
#endif

return []
main :: IO ()
main = do
  True <- $quickCheckAll
  return ()
