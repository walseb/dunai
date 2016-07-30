{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Transformer.MStreamF where

-- External
import Data.Tuple (swap)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict

-- External (arrows)
import Control.Arrow.Operations


-- Internal
import Control.Monad.Trans.MStreamF
import Data.MonadicStreamFunction


instance Monad m => ArrowReader r (MStreamF (ReaderT r m)) where
    readState = liftMStreamF_ ask
    newReader msf = arr swap >>> liftMStreamFTrans (runReaderS msf)

instance Monad m => ArrowState s (MStreamF (StateT s m)) where
    fetch = liftMStreamF_ get
    store = liftMStreamF  put

instance (Monoid w, Monad m) => ArrowWriter w (MStreamF (WriterT w m)) where
    write = liftMStreamF  tell
    newWriter msf = liftMStreamFTrans (runWriterS msf) >>> arr swap
