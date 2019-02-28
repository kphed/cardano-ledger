{-# LANGUAGE OverloadedStrings   #-}

module Main
  ( main
  )
where

import Cardano.Prelude

import Cardano.Shell.Constants.Types (CardanoConfiguration)
import Cardano.Shell.Features.Blockchain (createBlockchainFeature)
import Cardano.Shell.Features.Logging (LoggingLayer(..), createLoggingFeature)
import Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import Cardano.Shell.Presets (mainnetConfiguration)
import Cardano.Shell.Types (ApplicationEnvironment(..), CardanoApplication(..), CardanoEnvironment, CardanoFeature(..), initializeCardanoEnvironment)

main :: IO ()
main = do
  -- This is where the configuration and environment should come from;
  -- these values are currently thrown away in `createLoggingFeature`
  -- and `createBlockchainFeature`.
  cardanoConfiguration            <-  pure mainnetConfiguration
  cardanoEnvironment              <-  initializeCardanoEnvironment

    -- Application we are going to execute.
  let blockchainApplication :: LoggingLayer -> CardanoFeature -> IO ()
      blockchainApplication ll bf = do
        let
          logTrace   = llBasicTrace ll
          logNotice  = llLogNotice ll
          appendName = llAppendName ll
        logTrace' <- appendName "cardano-chain" logTrace
        logNotice logTrace' "Begin validating epoch files"
        featureStart bf
        logNotice logTrace' "Almost finished"
        pure ()

    -- Features `blockchainApplication will use.
  (cardanoFeature:rest, loggingLayer) <- initializeAllFeatures cardanoConfiguration cardanoEnvironment

  runCardanoApplicationWithFeatures Development (cardanoFeature:rest) . CardanoApplication $ blockchainApplication loggingLayer cardanoFeature
  pure ()


initializeAllFeatures :: CardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], LoggingLayer)
initializeAllFeatures cardanoConfiguration cardanoEnvironment = do
    (loggingLayer, loggingFeature)  <- createLoggingFeature cardanoEnvironment cardanoConfiguration
    -- We can throw away the `BlockchainLayer` because this is an MVar.
    (_, blockchainFeature)  <- createBlockchainFeature cardanoEnvironment cardanoConfiguration

    let allCardanoFeatures :: [CardanoFeature]
        allCardanoFeatures =
            [ blockchainFeature
            , loggingFeature
            ]

    pure (allCardanoFeatures, loggingLayer)







