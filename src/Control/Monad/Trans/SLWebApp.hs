{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.Trans.SLWebApp
    (
    -- * The SLWebAppT monad transformer
    SLWebAppT(..),
    runSLWebAppT
    ) where

import Control.Applicative (Alternative)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
       (Handler, LoggingT(..), MonadLog(..), runLoggingT)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Except
       (ExceptT(..), runExceptT)


-- | A monad transformer that adds exceptions, read only state and logging to
-- other monads.
--
-- @SLWebAppT@ construct a monad parameterized over four things:
--
-- *e -- The exception type.
--
-- *r -- The read only state type.
--
-- *message -- Log message type.
--
-- *m -- The inner monad.
--
newtype SLWebAppT e r message m a = SLWebAppT
    { unSLWebAppT :: ExceptT e (ReaderT r (LoggingT message m)) a
    } deriving (Functor,Applicative,Monad,Alternative,MonadIO,MonadReader r,MonadThrow,MonadCatch,MonadError e,MonadLog message)

instance MonadTrans (SLWebAppT e r message) where
    lift = SLWebAppT . lift . lift . lift

deriving instance MonadBase b m => MonadBase b
         (SLWebAppT e r message m)

instance MonadBaseControl IO m =>
         MonadBaseControl IO (SLWebAppT e r message m) where
    type StM (SLWebAppT e r message m) a = StM (ReaderT r (LoggingT message m)) (Either e a)
    liftBaseWith runInBase =
        SLWebAppT
            (ExceptT
                 (liftBaseWith
                      (\runInExcept ->
                            runInBase
                                (\(SLWebAppT (ExceptT m)) ->
                                      runInExcept m) >>=
                            return . Right)))
    restoreM st = SLWebAppT (ExceptT (restoreM st))

-- | Run execution within monad stack
runSLWebAppT :: SLWebAppT e r message m a
             -> r
             -> Handler m message
             -> m (Either e a)
runSLWebAppT app r handler =
    runLoggingT (runReaderT (runExceptT (unSLWebAppT app)) r) handler
