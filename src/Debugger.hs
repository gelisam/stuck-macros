{-# LANGUAGE  DerivingStrategies         #-}
{-# LANGUAGE  FlexibleInstances          #-}
{-# LANGUAGE  FunctionalDependencies     #-}
{-# LANGUAGE  ScopedTypeVariables        #-}
{-# LANGUAGE  UndecidableInstances       #-}

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


module Debugger where
  -- DYG explicit export list

import Evaluator

import Data.Bifunctor
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy   as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Reader as Reader
-- -----------------------------------------------------------------------------
-- Types


-- conceptually this is a ReaderT (DebugContext e) (ExceptT e) IO a but I've
-- just fused the transformers and to have more control over the monad instance
newtype Debug r e a = Debug { runDebug :: r -> IO (Either e a)
                            }

debugRunT :: r -> Debug r e a -> IO (Either e a)
debugRunT = flip runDebug

mapDebugT :: (a -> b) -> Debug r e a -> Debug r e b
mapDebugT f = Debug . fmap (fmap (second f)) . runDebug

withDebug :: (r' -> r) -> Debug r e a -> Debug r' e a
withDebug f m = Debug $ runDebug m . f

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

instance MonadDebugger e m => MonadDebugger e (ReaderT r m) where
  debug = lift . debug
  catch = Reader.liftCatch catch

instance MonadDebugger e m => MonadDebugger e (LazyState.StateT s m) where
  debug = lift . debug
  catch = LazyState.liftCatch catch

instance MonadDebugger e m => MonadDebugger e (StrictState.StateT s m) where
  debug = lift . debug
  catch = StrictState.liftCatch catch
  
class (Monad io, MonadIO io) => MonadDebugger e io | io -> e where
  -- conceptually this is throw
  debug :: e -> io a
  -- conceptually this is catch with a handler
  catch :: io a -> (e -> io a) -> io a

instance MonadDebugger e (Debug r e) where
  debug e = Debug $ const (return (Left e))
  catch (Debug m) hndl  = Debug $ \r -> do
    a <- m r
    case a of
      Left e -> runDebug (hndl e) r
      v@Right{} -> return v

data DebugContext = DebugContext { _stackTrace   :: [EState]
                                 }

initialContext :: DebugContext
initialContext = DebugContext mempty


-- checkError :: Debug e (Maybe e)
-- checkError = R.asks _currentError


-- -----------------------------------------------------------------------------
-- Top level API

-- enterDebugger :: ExpansionErr -> EState -> Debug Value
-- enterDebugger exp_err st =
