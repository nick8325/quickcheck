-- This module provides tools to simplify compat code across different compiler and library versions
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.QuickCheck.Compat where

#ifdef __GLASGOW_HASKELL__

#if MIN_VERSION_base(4,16,0)
import Data.Tuple

#if !MIN_VERSION_base(4,18,0)

getSolo :: Solo a -> a
getSolo (Solo a) = a

#elif !MIN_VERSION_base(4,19,0)

getSolo :: Solo a -> a
getSolo (MkSolo a) = a

#endif

mkSolo :: a -> Solo a
mkSolo = Solo

#else

import Data.Tuple.Solo

mkSolo :: a -> Solo a
mkSolo = MkSolo

#endif

#endif
