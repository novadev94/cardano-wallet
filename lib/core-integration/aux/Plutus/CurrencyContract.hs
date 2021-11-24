{-# LANGUAGE OverloadedStrings #-}
{- Serialise the contract

    Plutus.Contracts.Currency

from the

    plutus-use-cases

repository. Running this code requires a working Plutus environment.
This includes, but is not limited to, the following packages:

    plutus-core, plutus-tx, plutus-contract, plutus-ledger, plutus-ledger-api

-}
module CurrencyContract where

import Prelude

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Codec.Serialise (serialise)
import Ledger (TxId (..), TxOutRef (..), Script, MintingPolicy (..))
import PlutusTx.Builtins.Class

-- Specific example contract
import PlutusTx.AssocMap qualified as AssocMap
import Plutus.Contracts.Currency as Example (curPolicy, OneShotCurrency (..))

{-----------------------------------------------------------------------------
    Serialize the example script
------------------------------------------------------------------------------}
myscript :: Script
myscript = getMintingPolicy $ Example.curPolicy mycurrency

mycurrency :: OneShotCurrency
mycurrency = OneShotCurrency (h,i) amounts
  where
    TxOutRef h i = dummyTxOutRef
    amounts = AssocMap.fromList [("apfel", 1000), ("banana", 1)]

{-----------------------------------------------------------------------------
    Utility functions for serialization
------------------------------------------------------------------------------}
-- | Hex encoded bytes
type Base16 = String

-- | 'serialise' produces a CBOR representation of the binary script
rawScript :: Script -> Base16
rawScript = BS8.unpack . Base16.encode . BSL.toStrict . serialise

-- | A dummy TxOutRef that is easy to copy & replace.
dummyTxOutRef :: TxOutRef
dummyTxOutRef = TxOutRef (mkTxId s32) 31
    where s32 = mconcat $ replicate 8 "DEADBEEF" -- 32 = 4*8

-- | TxId corresponds to 32 bytes
mkTxId :: Base16 -> TxId
mkTxId = TxId . toBuiltin . Base16.decodeLenient . BS8.pack
