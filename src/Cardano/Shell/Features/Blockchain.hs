{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Shell.Features.Blockchain
  ( BlockchainLayer(..)
  , createBlockchainFeature
  )
where

import Cardano.Prelude

import System.FilePath (isExtensionOf, (</>))
import System.Directory (getDirectoryContents)

import Cardano.Shell.Constants.Types (CardanoConfiguration)
import Cardano.Shell.Types
  (CardanoEnvironment, CardanoFeature(..))
import Cardano.Chain.Block (initialChainValidationState, ChainValidationState)
import Cardano.Chain.Epoch.Validation (validateEpochFile, EpochError)
import qualified Cardano.Chain.Genesis.Config as Genesis


data BlockchainLayer = BlockchainLayer
  { chainValidationStatus :: forall m. (MonadIO m, MonadError EpochError m) => m ()
  }

data BlockchainConfiguration = BlockchainConfiguration
  { configEpochFileDir :: FilePath
  , genesisConfig      :: Genesis.Config
  }

init
  :: forall m
   . MonadIO m
  => BlockchainConfiguration
  -> ChainValidationState
  -> MVar (Either EpochError ())
  -> m ()
init config cvs resultVar = do
  let epochFileDir = configEpochFileDir config
  epochFiles <- liftIO $
    sort
    .   fmap (epochFileDir </>)
    .   filter ("epoch" `isExtensionOf`)
    <$> getDirectoryContents epochFileDir
  result <- liftIO . runExceptT . void $ foldM
    (fmap ExceptT <$> validateEpochFile (genesisConfig config))
    cvs
    epochFiles
  liftIO $ putMVar resultVar result


cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

-- TODO: We also ignore `CardanoEnvironment` for now.

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ _ = do
  -- Construct `Config` using mainnet-genesis.json
  -- TODO: Need to populate `CardanoConfiguration` with data from `mainnet-genesis.json`.
  -- We ignore this parameter for now.
  config <-
    either
        (panic "TODO: Add buildable instance for Genesis.ConfigurationError")
        identity
      <$> runExceptT (Genesis.mkConfigFromFile "test/mainnet-genesis.json" Nothing)

  resultVar <- newEmptyMVar

  let blockchainConf = BlockchainConfiguration "cardano-mainnet-mirror/epochs" config

  -- Create initial `ChainValidationState`
  initCVS <- either (panic . show) identity <$> runExceptT (initialChainValidationState config)

  let bcFeature   = CardanoFeature
        { featureName     = "Blockchain"
        -- `featureStart` is the logic to be executed of a given feature.
        , featureStart    = init blockchainConf initCVS resultVar
        , featureShutdown = cleanup
        }
  -- Initialize `BlockchainLayer` with (). This will be updated
  -- after the feature's logic is executed.
  let bcLayer = BlockchainLayer {chainValidationStatus = return ()}
  pure (bcLayer, bcFeature)

