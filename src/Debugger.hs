{-# LANGUAGE  DeriveAnyClass             #-}
{-# LANGUAGE  DerivingStrategies         #-}
{-# LANGUAGE  FunctionalDependencies     #-}
{-# LANGUAGE  ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Debugger
-- Copyright   :  (c) Jeffrey M. Young
--                    Samuel Gélineau
--                    David Thrane Christiansen
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young      <jeffrey.young@iohk.io>
--                Samuel Gélineau    <gelisam@gmail.com>
--                David Christiansen <david@davidchristiansen.dk>
-- Stability   :  experimental
--
-- A Common Lisp style Debugger for klister.
-----------------------------------------------------------------------------


module Debugger
  ( -- enterDebugger
  ) where

import Evaluator

import Data.Bifunctor
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import qualified Control.Monad.Reader as R



-- -----------------------------------------------------------------------------
-- Types

-- conceptually this is a ReaderT (DebugContext e) (ExceptT e) IO a but I've
-- just fused the transformers and to have more control over the monad instance
newtype Debug r e a = Debug { runDebugT :: r -> IO (Either e a)
                              }

runDebug :: Debug r e a -> r -> IO (Either e a)
runDebug = runDebugT

debugRunT :: r -> Debug r e a -> IO (Either e a)
debugRunT = flip runDebugT

mapDebugT :: (a -> b) -> Debug r e a -> Debug r e b
mapDebugT f = Debug . fmap (fmap (second f)) . runDebugT

instance Functor (Debug r e) where
  fmap = mapDebugT

instance Applicative (Debug r e) where
  pure a  = Debug $ const (return (Right a))
  Debug f <*> Debug v = Debug $ \rr -> do
    mf <- f rr
    case mf of
      (Left fer) -> return (Left fer)
      (Right k)  -> do
        mv <- v rr
        case mv of
          (Left ver) -> return (Left ver)
          Right x    -> return (Right (k x))

instance Monad (Debug r e) where
  Debug m >>= f  = Debug $ \r -> do
    ma <- m r
    case ma of
      Left err  -> return (Left err)
      Right val -> fmap (debugRunT r) f val

instance MonadIO (Debug r e) where
  liftIO = Debug . const . fmap Right

class (Monad io, MonadIO io) => MonadDebugger e io | io -> e where
  -- conceptually this is throw
  enter :: e -> m a
  -- conceptually this is catch with a handler
  catch :: m a -> (e -> m b) -> m b


data DebugContext e = DebugContext { _currentError :: Maybe e
                                   , _stackTrace   :: [EState]
                                   }
                      deriving (Semigroup, Monoid)

initialContext :: DebugContext e
initialContext = mempty


-- checkError :: Debug e (Maybe e)
-- checkError = R.asks _currentError


-- -----------------------------------------------------------------------------
-- Top level API

-- enterDebugger :: ExpansionErr -> EState -> Debug Value
-- enterDebugger exp_err st =
