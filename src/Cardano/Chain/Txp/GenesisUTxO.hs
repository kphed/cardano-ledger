module Cardano.Chain.Txp.GenesisUTxO
  ( genesisUtxo
  )
where

import Cardano.Prelude

import qualified Data.Map.Strict as M

import Cardano.Chain.Common (Address, Lovelace, makeRedeemAddress)
import Cardano.Chain.Genesis
  (GenesisData(..), getGenesisAvvmBalances, getGenesisNonAvvmBalances)
import Cardano.Chain.Txp.UTxO (UTxO)
import qualified Cardano.Chain.Txp.UTxO as UTxO


-- | Create initial 'UTxO' from balances defined in the genesis block
genesisUtxo :: GenesisData -> UTxO
genesisUtxo genesisData = UTxO.fromBalances balances
 where
  balances :: [(Address, Lovelace)]
  balances = avvmBalances <> nonAvvmBalances

  avvmBalances :: [(Address, Lovelace)]
  avvmBalances = first makeRedeemAddress
    <$> M.toList (getGenesisAvvmBalances $ gdAvvmDistr genesisData)

  nonAvvmBalances :: [(Address, Lovelace)]
  nonAvvmBalances =
    M.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances genesisData
