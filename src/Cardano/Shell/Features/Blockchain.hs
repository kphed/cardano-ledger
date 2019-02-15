{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.Shell.Features.Blockchain
  ( BlockchainLayer(..)
  , createBlockchainFeature
  )
where


import Cardano.Prelude
import Cardano.Shell.Types
  (CardanoConfiguration, CardanoEnvironment, CardanoFeature(..))

import Cardano.Chain.Block (ABlock, ChainValidationState)
import Control.Monad.Conc.Class (MonadConc)
import qualified Cardano.Chain.Genesis.Config as Genesis

todo :: forall a . a
todo = let x = x in x

data BlockchainLayer = BlockchainLayer {
  chainValidationStatus :: (MonadIO m, MonadError EpochError m) => m ()
}

data BlockchainConfiguration = BlockchainConiguration
  { epochFileDir :: Text
  , genesisConfig :: Genesis.Config
  }

init
  :: forall m
   . (MonadIO m, MonadConc m)
  => BlockchainConfiguration
  -> ChainvalidationState
  -> MVar (Either EpochError ())
  -> m ()
init config csv resultVar = do
  epochFiles <-
    sort
    .   fmap (dataDir </>)
    .   filter ("epoch" `isExtensionOf`)
    <$> getDirectoryContents (epochFileDir conf)
  result <- eunExceptT
    $ foldM (ExceptT $ validateEpochFile (genesisConfig conf)) csv epochFiles
  putMVar resultVar result

{-
 - validateEpochFile
  :: Genesis.Config
  -> ChainValidationState
  -> FilePath
  -> IO (Either EpochError ChainValidationState)
-}


cleanup :: forall m . (MonadIO m, MonadConc m) => m ()
cleanup = pure ()

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature cardanoEnvironment cardanoConfiguration = do
  -- Get the configuration from somewhere
  -- Get the initial chain validation state
  -- Fold
  let
    resultVar = _ :: MVar (Either EpochError ())
    cvs       = _ :: ChainValidationState
    feature   = CardanoFeature
      { featureName     = "Blockchain"
      , featureStart    = init blockchainConf cvs resultVar
      , featureShutdown = cleanup
      }
    layer = BlockchainLayer {blockchainState = readMVar resultVar}
  pure (layer, feature)
