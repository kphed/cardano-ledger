{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Txp.UTxO
  ( UTxO
  , UTxOError
  , empty
  , fromList
  , fromBalances
  , member
  , lookupAddress
  , union
  , balance
  , (<|)
  , (</|)
  , txOutputUTxO
  , isRedeemUTxO
  )
where

import Cardano.Prelude hiding (empty)

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Cardano.Chain.Common
  (Address, Lovelace, LovelaceError, isRedeemAddress, sumLovelace)
import Cardano.Chain.Txp.Tx (Tx(..), TxId, TxIn(..), TxOut(..))
import Cardano.Crypto (hash)


newtype UTxO = UTxO
  { unUTxO :: Map TxIn TxOut
  } deriving (Eq, Show, HeapWords)

data UTxOError
  = UTxOMissingInput TxIn
  | UTxOOverlappingUnion
  deriving (Eq, Show)

empty :: UTxO
empty = UTxO mempty

fromList :: [(TxIn, TxOut)] -> UTxO
fromList = UTxO . M.fromList

-- | Create a 'UTxO' from a list of initial balances
fromBalances :: [(Address, Lovelace)] -> UTxO
fromBalances = fromList . fmap utxoEntry
 where
  utxoEntry :: (Address, Lovelace) -> (TxIn, TxOut)
  utxoEntry (addr, lovelace) =
    (TxInUtxo (coerce $ hash addr) 0, TxOut addr lovelace)

member :: TxIn -> UTxO -> Bool
member txIn = M.member txIn . unUTxO

lookupAddress :: TxIn -> UTxO -> Either UTxOError Address
lookupAddress txIn =
  maybe (Left $ UTxOMissingInput txIn) (Right . txOutAddress)
    . M.lookup txIn
    . unUTxO

union :: MonadError UTxOError m => UTxO -> UTxO -> m UTxO
union (UTxO m) (UTxO m') = do
  let m'' = M.union m m'
  (M.size m'' == M.size m + M.size m') `orThrowError` UTxOOverlappingUnion
  pure $ UTxO m''

balance :: UTxO -> Either LovelaceError Lovelace
balance = sumLovelace . fmap txOutValue . M.elems . unUTxO

(<|) :: Set TxIn -> UTxO -> UTxO
(<|) inputs = UTxO . flip M.restrictKeys inputs . unUTxO

(</|) :: Set TxIn -> UTxO -> UTxO
(</|) inputs = UTxO . flip M.withoutKeys inputs . unUTxO

txOutputUTxO :: Tx -> UTxO
txOutputUTxO tx = UTxO $ M.fromList
  [ (TxInUtxo (txId tx) ix, txOut) | (ix, txOut) <- indexedOutputs ]
 where
  indexedOutputs :: [(Word32, TxOut)]
  indexedOutputs = zip [0 ..] (NE.toList $ _txOutputs tx)

  txId :: Tx -> TxId
  txId = hash

isRedeemUTxO :: UTxO -> Bool
isRedeemUTxO = all (isRedeemAddress . txOutAddress) . M.elems . unUTxO
