module Cardano.Shell.Features.Blockchain where
  ( BlockchainLayer (..)
  , createBlockchainFeature
  )


import Cardano.Prelude
import           Cardano.Shell.Types (CardanoConfiguration, CardanoEnvironment,
                                      CardanoFeature (..),
                                      CardanoFeatureInit (..)
                                     )

todo :: forall a . a
todo = let x = x in x

data BlockchainLayer = BlockchainLayer {
  update :: State -> ABlock ByteString -> Either SomeError State
}

doUpdate :: State -> ABlock ByteString -> Either SomeError State
doUpdate state block = 


{-
  (\c b ->
    withExceptT (ErrorChainValidationError (blockOrBoundarySlot b)) $ case b of
      ABOBBoundary bvd   -> updateChainBoundary config c bvd
      ABOBBlock    block -> updateChain config c block
  )
-}

init :: forall m. (MonadIO m, MonadConc m) => m ()
init = todo

cleanup :: forall m. (MonadIO m, MonadConc m) => m ()
cleanup = todo

createBlockchainFeature
  :: CardanoEnvironment -> CardanoConfiguration -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature = do
  let feature = CardanoFeature { featureName = "Blockchain"
                               , featureStart = init
                               , featureShutdown = cleanup
                               }
      layer = BlockchainLayer { update = doUpdate }
  pure (layer, feature)

