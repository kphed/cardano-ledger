{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Elaboration.UTxO
  ( UTxO
  , TxWits
  , Tx
  , TxId
  , TxIn
  , Wit
  , elaborateUTxOEnv
  , elaborateUTxO
  , elaborateTxWitsBS
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Formatting hiding (bytes)

import qualified Cardano.Binary.Class as CBOR
import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Txp as Concrete
import qualified Cardano.Chain.Txp.UTxO as Concrete.UTxO
import qualified Cardano.Chain.Txp.Validation as Concrete.UTxO
import qualified Cardano.Chain.Update as Concrete
import Cardano.Crypto
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Hedgehog

import Test.Cardano.Chain.Elaboration.Keys
import Test.Cardano.Chain.Genesis.Dummy
import Test.Cardano.Crypto.Dummy


type TxId v = Either Abstract.Addr (Var Concrete.TxId v)

type UTxO v = Abstract.UTxO (TxId v)

type TxWits v = Abstract.TxWits (TxId v)

type Tx v = Abstract.Tx (TxId v)

type TxIn v = Abstract.TxIn (TxId v)

type Wit v = Abstract.Wit (TxId v)


elaborateUTxOEnv
  :: Abstract.UTxOEnv (TxId Concrete) -> Concrete.UTxO.Environment
elaborateUTxOEnv _abstractEnv = Concrete.UTxO.Environment
  { Concrete.UTxO.envProtocolParameters = dummyProtocolParameters
    { Concrete.ppTxFeePolicy =
      Concrete.TxFeePolicyTxSizeLinear $ Concrete.TxSizeLinear
        (Concrete.mkKnownLovelace @0)
        (Concrete.mkKnownLovelace @0)
    }
  }

elaborateUTxO :: UTxO Concrete -> Concrete.UTxO
elaborateUTxO (Abstract.UTxO utxo) =
  Concrete.UTxO.fromList . fmap elaborateUTxOEntry $ M.toList utxo

elaborateUTxOEntry
  :: (TxIn Concrete, Abstract.TxOut) -> (Concrete.TxIn, Concrete.TxOut)
elaborateUTxOEntry (abstractTxIn, abstractTxOut) =
  (concreteTxIn, concreteTxOut)
 where
  concreteTxOut = elaborateTxOut abstractTxOut
  concreteTxIn  = elaborateUTxOInput concreteTxOut abstractTxIn

elaborateUTxOInput :: Concrete.TxOut -> TxIn Concrete -> Concrete.TxIn
elaborateUTxOInput concreteTxOut (Abstract.TxIn mTxId index) =
  Concrete.TxInUtxo concreteTxId (fromIntegral index)
 where
  concreteTxId = case mTxId of
    Left _     -> coerce . hash $ Concrete.txOutAddress concreteTxOut
    Right txid -> concrete txid

elaborateTxWitsBS
  :: UTxO Concrete -> TxWits Concrete -> Concrete.ATxAux ByteString
elaborateTxWitsBS utxo0 = annotateTxAux . elaborateTxWits utxo0
 where
  annotateTxAux :: Concrete.TxAux -> Concrete.ATxAux ByteString
  annotateTxAux txAux =
    map (LBS.toStrict . CBOR.slice bytes)
      . fromRight (panic "elaborateTxWitsBS: Error decoding TxAux")
      $ CBOR.decodeFullDecoder "ATxAux" Concrete.decodeATxAux bytes
    where bytes = CBOR.serialize txAux

elaborateTxWits :: UTxO Concrete -> TxWits Concrete -> Concrete.TxAux
elaborateTxWits utxo0 (Abstract.TxWits tx witnesses) = Concrete.mkTxAux
  concreteTx
  (elaborateWitnesses concreteTx witnesses)
  where concreteTx = elaborateTx utxo0 tx

elaborateTx :: UTxO Concrete -> Tx Concrete -> Concrete.Tx
elaborateTx utxo0 (Abstract.Tx _ inputs outputs) = Concrete.UnsafeTx
  { Concrete._txInputs     = elaborateTxIns utxo0 inputs
  , Concrete._txOutputs    = elaborateTxOuts outputs
  , Concrete._txAttributes = Concrete.mkAttributes ()
  }

elaborateWitnesses :: Concrete.Tx -> [Wit Concrete] -> Concrete.TxWitness
elaborateWitnesses concreteTx = V.fromList . fmap (elaborateWitness concreteTx)

elaborateWitness :: Concrete.Tx -> Wit Concrete -> Concrete.TxInWitness
elaborateWitness concreteTx (Abstract.Wit key _) = Concrete.PkWitness
  concretePK
  signature
 where
  (concretePK, concreteSK) = elaborateKeyPair $ vKeyPair key
  signature = sign dummyProtocolMagicId SignTx concreteSK sigData
  sigData   = Concrete.TxSigData $ hash concreteTx

elaborateTxIns :: UTxO Concrete -> [TxIn Concrete] -> NonEmpty Concrete.TxIn
elaborateTxIns utxo0 =
  fromMaybe (panic "elaborateTxIns: Empty list of TxIns") . NE.nonEmpty . fmap
    (elaborateTxIn utxo0)

elaborateTxIn :: UTxO Concrete -> TxIn Concrete -> Concrete.TxIn
elaborateTxIn (Abstract.UTxO utxo) abstractTxIn = fst
  $ elaborateUTxOEntry (abstractTxIn, abstractTxOut)
 where
  abstractTxOut =
    fromMaybe (panic "elaborateTxIn: Missing output for input")
      $ M.lookup abstractTxIn utxo

elaborateTxOuts :: [Abstract.TxOut] -> NonEmpty Concrete.TxOut
elaborateTxOuts =
  fromMaybe
      (panic
        "elaborateTxOuts: Tried to elaborate an empty list of Abstract.TxOuts"
      )
    . NE.nonEmpty
    . fmap elaborateTxOut

elaborateTxOut :: Abstract.TxOut -> Concrete.TxOut
elaborateTxOut abstractTxOut = Concrete.TxOut
  { Concrete.txOutAddress = Concrete.makePubKeyAddress
    (elaborateVKey abstractPK)
  , Concrete.txOutValue   = lovelaceValue
  }
 where
  Abstract.TxOut (Abstract.Addr abstractPK) (Abstract.Value value) =
    abstractTxOut

  lovelaceValue = case Concrete.mkLovelace (fromIntegral value) of
    Left  err -> panic $ sformat build err
    Right l   -> l
