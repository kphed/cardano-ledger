{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Txp.Validation
  ( validateTx
  , updateUTxO
  , updateUTxOWitness
  , TxValidationError
  , Environment(..)
  , UTxOValidationError
  )
where


import Cardano.Prelude


import Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V


import Cardano.Binary.Class (Annotated(..), biSize)
import Cardano.Chain.Common
  ( Address
  , Lovelace
  , LovelaceError
  , TxFeePolicy(..)
  , calculateTxSizeLinear
  , checkPubKeyAddress
  , checkRedeemAddress
  , mkKnownLovelace
  , subLovelace
  )
import Cardano.Chain.Txp.Tx (Tx(..), TxIn)
import Cardano.Chain.Txp.TxAux (ATxAux, aTaTx, taTx, taWitness)
import Cardano.Chain.Txp.TxWitness
  (TxInWitness(..), TxSigData(..), recoverSigData)
import Cardano.Chain.Txp.UTxO
  (UTxO, UTxOError, balance, isRedeemUTxO, txOutputUTxO, (</|), (<|))
import qualified Cardano.Chain.Txp.UTxO as UTxO
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters(..))
import Cardano.Crypto
  (ProtocolMagicId, SignTag(..), verifySignatureDecoded, verifyRedeemSigDecoded)


-- | A representation of all the ways a transaction might be invalid
data TxValidationError
  = TxValidationLovelaceError Text LovelaceError
  | TxValidationFeeTooSmall Tx Lovelace Lovelace
  | TxValidationInvalidWitness TxInWitness
  | TxValidationMissingInput TxIn
  deriving (Eq, Show)


-- | Validate that:
--
--   1. All @TxIn@s are in domain of @Utxo@
--   2. The fee is not less than the minimum fee
--   3. Output balance + fee = input balance
--
--   These are the conditions of the UTxO inference rule in the spec. We
--   actually assume 3 by calculating the fee as output balance - input balance.
validateTx
  :: MonadError TxValidationError m => TxFeePolicy -> UTxO -> Tx -> m ()
validateTx feePolicy utxo tx = do

  -- Check that every input is in the domain of 'utxo'
  _txInputs tx `forM_` validateTxIn utxo

  -- Calculate the minimum fee from the 'TxFeePolicy'
  minFee <- if isRedeemUTxO inputUTxO
    then pure $ mkKnownLovelace @0
    else calculateMinimumFee feePolicy

  -- Calculate the balance of the output 'UTxO'
  balanceOut <- balance (txOutputUTxO tx)
    `wrapError` TxValidationLovelaceError "Output Balance"

  -- Calculate the balance of the restricted input 'UTxO'
  balanceIn <- balance inputUTxO
    `wrapError` TxValidationLovelaceError "Input Balance"

  -- Calculate the 'fee' as the difference of the balances
  fee <- subLovelace balanceIn balanceOut
    `wrapError` TxValidationLovelaceError "Fee"

  -- Check that the fee is greater than the minimum
  (minFee <= fee) `orThrowError` TxValidationFeeTooSmall tx minFee fee
 where

  txSize    = biSize tx

  inputUTxO = S.fromList (NE.toList (_txInputs tx)) <| utxo

  calculateMinimumFee
    :: MonadError TxValidationError m => TxFeePolicy -> m Lovelace
  calculateMinimumFee = \case

    TxFeePolicyTxSizeLinear txSizeLinear ->
      calculateTxSizeLinear txSizeLinear txSize
        `wrapError` TxValidationLovelaceError "Minimum Fee"


-- | Validate that 'TxIn' is in the domain of 'UTxO'
validateTxIn :: MonadError TxValidationError m => UTxO -> TxIn -> m ()
validateTxIn utxo txIn
  | txIn `UTxO.member` utxo = pure ()
  | otherwise = throwError $ TxValidationMissingInput txIn


-- | Verify that a 'TxInWitness' is a valid witness for the supplied 'TxSigData'
validateWitness
  :: MonadError TxValidationError m
  => ProtocolMagicId
  -> Annotated TxSigData ByteString
  -> Address
  -> TxInWitness
  -> m ()
validateWitness pm sigData addr witness = case witness of

  PkWitness pk sig ->
    (  verifySignatureDecoded pm SignTx pk sigData sig
      && checkPubKeyAddress pk addr
      )
      `orThrowError` TxValidationInvalidWitness witness

  RedeemWitness pk sig ->
    (  verifyRedeemSigDecoded pm SignRedeemTx pk sigData sig
      && checkRedeemAddress pk addr
      )
      `orThrowError` TxValidationInvalidWitness witness


data Environment = Environment
  { envProtocolParameters :: ProtocolParameters
  } deriving (Eq, Show)

data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)


-- | Validate a transaction and use it to update the 'UTxO'
updateUTxO
  :: MonadError UTxOValidationError m => Environment -> UTxO -> Tx -> m UTxO
updateUTxO env utxo tx = do

  validateTx (ppTxFeePolicy envProtocolParameters) utxo tx
    `wrapError` UTxOValidationTxValidationError

  UTxO.union (S.fromList (NE.toList (_txInputs tx)) </| utxo) (txOutputUTxO tx)
    `wrapError` UTxOValidationUTxOError
 where
   Environment {envProtocolParameters} = env


-- | Validate a transaction with a witness and use it to update the 'UTxO'
updateUTxOWitness
  :: MonadError UTxOValidationError m
  => ProtocolMagicId
  -> Environment
  -> UTxO
  -> ATxAux ByteString
  -> m UTxO
updateUTxOWitness pm env utxo ta = do
  let
    tx      = taTx ta
    witness = taWitness ta
    --TODO:  after rebase: hashAnnotated fix
    sigData = recoverSigData $ aTaTx ta

  -- Get the signing addresses for each transaction input from the 'UTxO'
  addresses <-
    mapM (`UTxO.lookupAddress` utxo) (NE.toList $ _txInputs tx)
      `wrapError` UTxOValidationUTxOError

  -- Validate witnesses and their signing addresses
  mapM_
      (uncurry $ validateWitness pm sigData)
      (zip addresses (V.toList witness))
    `wrapError` UTxOValidationTxValidationError

  -- Update 'UTxO' ignoring witnesses
  updateUTxO env utxo tx
