{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.Shell.Features.Blockchain
  ( BlockchainLayer(..)
  , createBlockchainFeature
  )
where

import Cardano.Prelude

import Control.Monad.Conc.Class (MonadConc)
import System.FilePath (isExtensionOf, (</>))
import System.Directory (getDirectoryContents)

import Cardano.Shell.Types
  (CardanoConfiguration, CardanoEnvironment, CardanoFeature(..))
import Cardano.Chain.Block (ABlock, ChainValidationState)
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
   . (MonadIO m, MonadConc m)
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
  resultVar <- newEmptyMVar
  let
    resultVar = _ :: MVar (Either EpochError ())
    cvs       = _ :: ChainValidationState
    feature   = CardanoFeature
      { featureName     = "Blockchain"
      , featureStart    = init blockchainConf cvs resultVar
      , featureShutdown = cleanup
      }
    layer = BlockchainLayer {chainValidationStatus = readMVar resultVar}
  pure (layer, feature)
