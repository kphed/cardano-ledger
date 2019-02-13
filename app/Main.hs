{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Cardano.Prelude
import System.Exit (exitSuccess)

import Control.Concurrent (threadDelay)
import Features.Blockchain (BlockchainLayer(..), createBlockchainFeature)
import Cardano.Shell.Features.Logging (LoggingLayer(..), createLoggingFeature)
import Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import Cardano.Shell.Presets (mainnetConfiguration)
import Cardano.Shell.Types
  ( ApplicationEnvironment(..)
  , CardanoApplication(..)
  , initializeCardanoEnvironment
  )

main :: IO ()
main = do
  -- This is where the configuration and environment should come from;
  -- these values are currently thrown away in `createLoggingFeature`
  -- and `createBlockchainFeature`.
  cardanoConfiguration           <- pure mainnetConfiguration
  cardanoEnvironment             <- initializeCardanoEnvironment

    -- Features `chainValidationApp` will use.
  (loggingLayer, loggingFeature) <- createLoggingFeature
    cardanoEnvironment
    cardanoConfiguration
  (blockchainLayer, blockchainFeature) <- createBlockchainFeature
    cardanoEnvironment
    cardanoConfiguration
    Development

  -- Run application.
  runCardanoApplicationWithFeatures
      Development
      [blockchainFeature, loggingFeature]
    . CardanoApplication
    $ chainValidationApp loggingLayer blockchainLayer

chainValidationApp :: LoggingLayer -> BlockchainLayer -> IO ()
chainValidationApp ll bcl = do
  logTrace' <- appendName "cardano-chain" logTrace
  let
    loop = chainValidationStatus bcl >>= \case
      Nothing -> do
        logNotice logTrace' "No results yet!"
        threadDelay 1e7
        loop
      Just results -> case results of
        Left epochError -> logNotice logTrace' $ show epochError
        Right _ ->
          logNotice logTrace' "Epoch validation successful!" >> exitSuccess
  logNotice logTrace' "Begin validating epoch files..."
  loop
  logNotice logTrace' "Finished bulk chain validation."
 where
  logTrace   = llBasicTrace ll
  logNotice  = llLogNotice ll
  appendName = llAppendName ll
