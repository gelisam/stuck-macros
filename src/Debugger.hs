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
  ( enterDebugger
  ) where


-- -----------------------------------------------------------------------------
-- Types

newtype Debug a = Debug
  { runDebug :: ReaderT DebugContext (ExceptT ExpansionErr IO) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError ExpansionErr
           , MonadIO, MonadReader DebugContext
           )

-- -----------------------------------------------------------------------------
-- Top level API

enterDebugger :: ExpansionErr -> EState -> Debug Value
enterDebugger exp_err st = undefined
