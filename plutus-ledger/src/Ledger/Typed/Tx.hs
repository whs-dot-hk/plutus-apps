{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Typed transaction inputs and outputs. This module defines typed versions
--   of various ledger types. The ultimate goal is to make sure that the script
--   types attached to inputs and outputs line up, to avoid type errors at
--   validation time.
module Ledger.Typed.Tx
  ( module Ledger.Typed.Tx
  , module Plutus.Script.Utils.V1.Typed.Scripts
  ) where

import Control.Lens (preview)
import Control.Monad.Except (MonadError, throwError)
import Ledger.Tx (ChainIndexTxOut, TxIn (TxIn, txInRef, txInType), TxInType (ConsumeScriptAddress), TxOutRef,
                  _ScriptChainIndexTxOut)
import Plutus.Script.Utils.V1.Typed.Scripts
import PlutusTx (FromData, ToData)

