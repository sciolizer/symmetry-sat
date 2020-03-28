{-# LANGUAGE FlexibleInstances #-}

module Families.MonadNotify (
  MonadNotify(..),
  runLogging
) where

import Control.Monad.Writer
import Data.Functor.Identity
import System.IO

import Debug.Trace

class (Monad m) => MonadNotify m where
  notify :: String -> m ()

instance MonadNotify Identity where
  notify _ = trace "yeah it's going through the identity monad" (return ())

instance (Monad m) => MonadNotify (WriterT [String] m) where
  notify s = tell [s]

runLogging :: WriterT [String] Identity a -> (a, [String])
runLogging = runIdentity . runWriterT

instance MonadNotify IO where
  notify s = hPutStrLn stderr s

runPrinting :: IO a -> IO a
runPrinting = id