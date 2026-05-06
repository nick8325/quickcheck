-- This module provides tools to simplify compat code across different compiler and library versions
{-# LANGUAGE CPP #-}
module Test.QuickCheck.Compat where

#if MIN_VERSION_base(4,16,0)
import Data.Tuple

#if !MIN_VERSION_base(4,18,0)

getSolo :: Solo a -> a
getSolo (Solo a) = a

mkSolo :: a -> Solo a
mkSolo = Solo

#elif !MIN_VERSION_base(4,19,0)

getSolo :: Solo a -> a
getSolo (MkSolo a) = a

#endif

mkSolo :: a -> Solo a
mkSolo = MkSolo

#elif defined(__GHC__)

import Data.Tuple.Solo

mkSolo :: a -> Solo a
mkSolo = MkSolo

#endif
