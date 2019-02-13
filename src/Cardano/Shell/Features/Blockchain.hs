{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.Shell.Features.Blockchain
  ( BlockchainLayer(..)
  , createBlockchainFeature
  )
where


import Cardano.Prelude
import Cardano.Shell.Types
  ( CardanoConfiguration
  , CardanoEnvironment
  , CardanoFeature(..)
  , CardanoFeatureInit(..)
  )

import Cardano.Chain.Block (ABlock)
import Control.Monad.Conc.Class (MonadConc)

todo :: forall a . a
todo = let x = x in x

data BlockchainLayer = BlockchainLayer {
  runBlockchainLayer :: forall m . MonadIO m => m ()
}

init :: forall m . (MonadIO m, MonadConc m) => m ()
init = todo

cleanup :: forall m . (MonadIO m, MonadConc m) => m ()
cleanup = todo

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature cardanoEnvironment cardanoConfiguration = do
  let
    feature = CardanoFeature
      { featureName     = "Blockchain"
      , featureStart    = init
      , featureShutdown = cleanup
      }
    layer = BlockchainLayer {runBlockchainLayer = todo}
  pure (layer, feature)
