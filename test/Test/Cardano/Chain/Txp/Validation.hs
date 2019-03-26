{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Txp.Validation
  ( tests
  )
where

import Cardano.Prelude

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String (fromString)
import Formatting (build, sformat)

import Hedgehog
  ( Group(..)
  , Property
  , PropertyName
  , checkSequential
  , evalEither
  , property
  , withTests
  )
import Cardano.Chain.Block (ABlund, blockSlot, blockTxPayload, foldUTxO, foldUTxOBlund)
import Cardano.Chain.Epoch.File (ParseError, parseEpochFile)
import Cardano.Chain.Genesis (GenesisData(..), readGenesisData)
import Cardano.Chain.Txp
  (UTxO, aUnTxPayload, genesisUtxo, updateUTxOWitness)
import Cardano.Crypto (ProtocolMagicId, getProtocolMagicId)
import Cardano.Mirror (mainnetEpochFiles)

import Test.Options (TestScenario(..))


-- | These tests perform transaction validation over mainnet epoch files
--
--   We have chosen to split each epoch file into its own 'Property', because
--   this leads to a clearer log of progress during testing. This requires an
--   'IORef' to synchronise the 'UTxO' between epochs, as 'Property's do not
--   return values.
tests :: TestScenario -> IO Bool
tests scenario = do

  -- Get 'GenesisData' from the mainnet JSON configuration
  genesisData <- either (panic . sformat build) fst
    <$> runExceptT (readGenesisData "mainnet-genesis.json")

  -- Extract mainnet 'ProtocolMagic'
  let pm = getProtocolMagicId $ gdProtocolMagic genesisData

  -- Create an 'IORef' containing the genesis 'UTxO'
  utxoRef <- newIORef $ genesisUtxo genesisData

  let
    takeFiles :: [FilePath] -> [FilePath]
    takeFiles = case scenario of
      ContinuousIntegration -> take 10
      Development           -> take 15
      QualityAssurance      -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> mainnetEpochFiles

  -- Validate the transactions of each epoch file in a single 'Property' and
  -- check them all sequentially
  let
    properties :: [(PropertyName, Property)]
    properties = zip (fromString <$> files) (epochValid pm utxoRef <$> files)
  checkSequential $ Group "Test.Cardano.Chain.Txp.Validation" properties

-- | Check that a single epoch's transactions are valid by folding over 'Blund's
epochValid :: ProtocolMagicId -> IORef UTxO -> FilePath -> Property
epochValid pm utxoRef fp = withTests 1 . property $ do
  utxo <- liftIO $ readIORef utxoRef
  let stream = parseEpochFile fp
  result  <- (liftIO . runResourceT . runExceptT) (foldUTxO pm utxo stream)
  newUtxo <- evalEither result
  liftIO $ writeIORef utxoRef newUtxo

