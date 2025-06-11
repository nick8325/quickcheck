{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
import CollectDataTypes
import Language.Haskell.TH
import Test.QuickCheck.All
import Test.QuickCheck

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Exception
import Control.Exception.Base
import Control.Monad.ST
import Control.Monad.ST.Lazy
import Control.Monad.ST.Strict
import Data.Array.Byte
import Data.Bits
import Data.Bool
import Data.Char
import Data.Complex
import Data.Data
import Data.Dynamic
import Data.Either
import Data.Fixed
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.IORef
import Data.Int
import Data.Kind
import Data.List.NonEmpty
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.STRef
import Data.STRef.Lazy
import Data.STRef.Strict
import Data.Semigroup
import Data.String
import Data.Tuple
import Data.Type.Bool
import Data.Type.Coercion
import Data.Type.Equality
import Data.Type.Ord
import Data.Typeable
import Data.Unique
import Data.Version
import Data.Void
import Data.Word
import Numeric.Natural
import System.Console.GetOpt
import System.Exit
import System.IO
import System.IO.Error
import System.Mem.StableName
import System.Mem.Weak
import System.Posix.Internals
import System.Posix.Types
import System.Timeout
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import Text.Printf
import Text.Read
import Text.Read.Lex
import Text.Show
import Type.Reflection
import Type.Reflection.Unsafe
import Unsafe.Coerce

$(createProperties "base")

newDeclarationGroup

-- TODO for some reason this doesn't work?!
main = runQuickCheckAll allProps quickCheckResult
