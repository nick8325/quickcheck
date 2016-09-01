{-# LANGUAGE CPP #-}
-- | Lift generators into other monads.
module Test.QuickCheck.Gen.Class where

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

-- | A typeclass for lifting a generator into another monad.
class Monad m => MonadGen m where
  -- | Lift a generator into the monad.
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
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (C.ContT r m) where
  liftGen = lift . liftGen
