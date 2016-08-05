{-# LANGUAGE CPP #-}

module Test.QuickCheck.Gen.Class (MonadGen (..)) where

import Data.Monoid (Monoid)
import Test.QuickCheck.Gen (Gen)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.Writer.Strict as SW
import qualified Control.Monad.Trans.Identity as I
import qualified Control.Monad.Trans.RWS.Lazy as LRWS
import qualified Control.Monad.Trans.RWS.Strict as SRWS
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Cont as C
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce (Coercible, coerce)
#endif

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a

instance MonadGen Gen where
  liftGen = id

instance MonadGen m => MonadGen (SS.StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (LS.StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (R.ReaderT r m) where
  liftGen = lift . liftGen

instance (MonadGen m, Monoid w) => MonadGen (LW.WriterT w m) where
  liftGen = lift . liftGen

instance (MonadGen m, Monoid w) => MonadGen (SW.WriterT w m) where
  liftGen = lift . liftGen

instance (MonadGen m, Monoid w) => MonadGen (LRWS.RWST r w s m) where
  liftGen = lift . liftGen

instance (MonadGen m, Monoid w) => MonadGen (SRWS.RWST r w s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (M.MaybeT m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (E.ExceptT e m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (I.IdentityT m) where
#if __GLASGOW_HASKELL__ >= 708
  liftGen = lift #. liftGen
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
_ #. g = coerce g
#else
  liftGen = lift . liftGen
#endif

instance MonadGen m => MonadGen (C.ContT r m) where
  liftGen = lift . liftGen
