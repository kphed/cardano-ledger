{-# LANGUAGE RankNTypes #-}

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

todo :: forall a . a
todo = let x = x in x

data BlockchainLayer = BlockchainLayer {
  runBlockchainLayer :: forall m . MonadIO m => m ()
}

doUpdate :: State -> ABlock ByteString -> Either SomeError State
doUpdate state block = _

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
    layer = BlockchainLayer {update = doUpdate}
  pure (layer, feature)
