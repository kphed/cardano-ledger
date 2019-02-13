{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Features.Blockchain
  ( BlockchainLayer(..)
  , genesisConfig
  , createBlockchainFeature
  , configEpochFileDir
  )
where

import Cardano.Prelude

import Formatting (build, sformat)

import Cardano.Chain.Block (initialChainValidationState, ChainValidationState)
import Cardano.Chain.Epoch.Validation (EpochError, validateEpochFiles)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Mirror (mainnetEpochFiles)
import Cardano.Shell.Constants.Types (CardanoConfiguration(..), Core(..), Genesis(..))
import Cardano.Shell.Types
  (ApplicationEnvironment(..), CardanoEnvironment, CardanoFeature(..))

--  | `BlockchainLayer` enables us to get the value from an MVar
--  | which holds the result of the bulk chain validation.
data BlockchainLayer = BlockchainLayer
  { chainValidationStatus
      :: forall m
       . MonadIO m
      => m (Maybe (Either EpochError ChainValidationState))
  }

data BlockchainConfiguration = BlockchainConfiguration
  { configEpochFileDir :: FilePath
  , genesisConfig      :: Genesis.Config
  }

cleanup :: forall m . MonadIO m => m ()
cleanup = pure ()

-- TODO: I believe this should get the epoch files locally
-- however for now I use `cardano-mainnet-mirror`.
init
  :: forall m
   . MonadIO m
  => BlockchainConfiguration
  -> ApplicationEnvironment
  -> ChainValidationState
  -> MVar (Either EpochError ChainValidationState)
  -> m ()
init config appEnv initialCVS resultVar = do
  -- Validate epoch files
  files <- case appEnv of
    Development -> take 10 <$> liftIO mainnetEpochFiles
    Production  -> liftIO mainnetEpochFiles

  result <- liftIO $ validateEpochFiles (genesisConfig config) initialCVS files
  liftIO $ putMVar resultVar result

createBlockchainFeature
  :: CardanoEnvironment
  -> CardanoConfiguration
  -> ApplicationEnvironment
  -> IO (BlockchainLayer, CardanoFeature)
createBlockchainFeature _ cc appEnv = do
  -- Construct `Config` using mainnet-genesis.json
  let mainnetGenFilepath = geSrc . coGenesis $ ccCore cc
  config <- either (panic . sformat build) identity
    <$> runExceptT
          (Genesis.mkConfigFromFile mainnetGenFilepath Nothing)

  let
    blockchainConf =
      BlockchainConfiguration "cardano-mainnet-mirror/epochs" config

  -- Create initial `ChainValidationState`.
  initCVS <- either (panic . show) identity
    <$> runExceptT (initialChainValidationState config)

  -- Create MVar that will hold the result of the bulk chain validation.
  resultVar <- newEmptyMVar

  -- Create Blockchain feature
  let
    bcFeature = CardanoFeature
      { featureName     = "Blockchain"
      -- `featureStart` is the logic to be executed of a given feature.
      , featureStart    = init blockchainConf appEnv initCVS resultVar
      , featureShutdown = cleanup
      }

  -- Create `BlockchainLayer`
  let
    bcLayer =
      BlockchainLayer {chainValidationStatus = liftIO $ tryReadMVar resultVar}
  pure (bcLayer, bcFeature)

