-----------------------------------------------------------------------------
-- |
-- Module      :  StackTraces
-- Copyright   :  (c) David Thrane Christiansen
--                    Samuel Gélineau
--                    Jeffrey M. Young
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Samuel Gélineau <gelisam@gmail.com>
--                David Christiansen <david@davidchristiansen.dk>
-- Stability   :  experimental
--
-- Converting state from the CEK machine to stack trace
-----------------------------------------------------------------------------


module StackTraces where

import Evaluator
import Pretty


-- -----------------------------------------------------------------------------
-- Top level API

type StackTrace = EState

printStack :: StackTrace -> Doc ann
printStack (Er err env k) = hang 2 $
  printErr err

printKont :: Kont -> Doc ann
printKont = align . vsep

printErr :: EvalError -> Doc ann
printErr = pretty

printEnv :: VEnv -> Doc ann
printEnv = pretty
