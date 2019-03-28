{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Genesis.Config
  ( StaticConfig(..)
  , Config(..)
  , configGenesisHeaderHash
  , configK
  , configSlotSecurityParam
  , configChainQualityThreshold
  , configEpochSlots
  , configProtocolMagic
  , configProtocolMagicId
  , configGeneratedSecretsThrow
  , configBootStakeholders
  , configHeavyDelegation
  , configStartTime
  , configNonAvvmBalances
  , configProtocolParameters
  , configAvvmDistr
  , mkConfig
  , mkConfigFromFile
  , mkConfigFromStaticConfig
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..), liftEither)
import Data.Aeson
  (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Coerce (coerce)
import Data.Time (UTCTime)
import System.FilePath ((</>))
import System.IO.Error (userError)

import Cardano.Binary.Class (Raw)
import Cardano.Chain.Block.Header (HeaderHash, genesisHeaderHash)
import Cardano.Chain.Genesis.Data
  (GenesisData(..), GenesisDataError, readGenesisData)
import Cardano.Chain.Genesis.Hash (GenesisHash(..))
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Initializer (GenesisInitializer(..))
import Cardano.Chain.Genesis.Generate
  (GeneratedSecrets, GenesisDataGenerationError, generateGenesisData)
import Cardano.Chain.Genesis.Spec (GenesisSpec(..))
import Cardano.Chain.Genesis.WStakeholders (GenesisWStakeholders)
import Cardano.Chain.Genesis.Delegation (GenesisDelegation)
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances)
import Cardano.Crypto (Hash, ProtocolMagic(..), ProtocolMagicId(..), hash)
import Cardano.Chain.Common (BlockCount)
import Cardano.Chain.Slotting (EpochSlots, SlotCount)
import Cardano.Chain.Update (ProtocolParameters)
import Cardano.Chain.ProtocolConstants
  (kEpochSlots, kSlotSecurityParam, kChainQualityThreshold)


--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

data StaticConfig
  = GCSpec !GenesisSpec
  -- ^ Genesis from a 'GenesisSpec'
  | GCSrc !FilePath !(Hash Raw)
  -- ^ 'GenesisData' is stored in at 'FilePath' with expected 'Hash Raw'
  deriving (Eq, Show)

instance ToJSON StaticConfig where
  toJSON (GCSrc gcsFile gcsHash) =
    object [ "src"    .= object [ "file" .= gcsFile
                                , "hash" .= gcsHash
                                ]
             ]
  toJSON (GCSpec
           (UnsafeGenesisSpec
             gsAvvmDistr'
             gsHeavyDelegation'
             gsProtocolParameters'
             gsK'
             gsProtocolMagic'
             gsInitializer')) =
    object ["spec" .= object
             [ "protocolParameters" .= gsProtocolParameters'
             , "k" .= gsK'
             , "avvmDistr" .= gsAvvmDistr'
             , "protocolMagic" .=  gsProtocolMagic'
             , "initializer" .= gsInitializer'
             , "heavyDelegation" .= gsHeavyDelegation'
             ]
           ]

instance FromJSON StaticConfig where
  parseJSON = withObject "StaticConfig" $ \o -> do
    src <- o .:? "src"
    case src of
      Just src' -> GCSrc <$> src' .: "file" <*> src' .: "hash"
      Nothing -> do
        specO <- o .: "spec"
        -- GenesisAvvmBalances
        avvmDistrV <- specO .: "avvmDistr"
        -- GenesisDelegation
        heavyDelegationV <- specO .: "heavyDelegation"
        -- ProtocolParameters
        protocolParametersV <- specO .: "protocolParameters"
        -- K
        kV <- specO .: "k"
        -- ProtocolMagic
        protocolMagicV <- specO .: "protocolMagic"
        -- GenesisInitializer
        initializerV <- specO .: "initializer"

        return . GCSpec $
          UnsafeGenesisSpec
            avvmDistrV
            heavyDelegationV
            protocolParametersV
            kV
            protocolMagicV
            initializerV

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config = Config
    { configGenesisData       :: GenesisData
    -- ^ The data needed at genesis
    , configGenesisHash       :: GenesisHash
    -- ^ The hash of the canonical JSON representation of the 'GenesisData'
    , configGeneratedSecrets  :: Maybe GeneratedSecrets
    -- ^ Secrets needed to access 'GenesisData' in testing
    --
    --   TODO: Figure out how to split testing and mainnet needs
    }

configGenesisHeaderHash :: Config -> HeaderHash
configGenesisHeaderHash = genesisHeaderHash . configGenesisHash

configK :: Config -> BlockCount
configK = gdK . configGenesisData

configSlotSecurityParam :: Config -> SlotCount
configSlotSecurityParam = kSlotSecurityParam . configK

configChainQualityThreshold :: Fractional f => Config -> f
configChainQualityThreshold = kChainQualityThreshold . configK

configEpochSlots :: Config -> EpochSlots
configEpochSlots = kEpochSlots . configK

configProtocolMagic :: Config -> ProtocolMagic
configProtocolMagic = gdProtocolMagic . configGenesisData

configProtocolMagicId :: Config -> ProtocolMagicId
configProtocolMagicId =
  getProtocolMagicId . gdProtocolMagic . configGenesisData


configGeneratedSecretsThrow :: MonadIO m => Config -> m GeneratedSecrets
configGeneratedSecretsThrow =
  maybe
      (liftIO $ throwIO $ userError
        "GeneratedSecrets missing from Genesis.Config"
      )
      pure
    . configGeneratedSecrets

configBootStakeholders :: Config -> GenesisWStakeholders
configBootStakeholders = gdBootStakeholders . configGenesisData

configHeavyDelegation :: Config -> GenesisDelegation
configHeavyDelegation = gdHeavyDelegation . configGenesisData

configStartTime :: Config -> UTCTime
configStartTime = gdStartTime . configGenesisData

configNonAvvmBalances :: Config -> GenesisNonAvvmBalances
configNonAvvmBalances = gdNonAvvmBalances . configGenesisData

configProtocolParameters :: Config -> ProtocolParameters
configProtocolParameters = gdProtocolParameters . configGenesisData

configAvvmDistr :: Config -> GenesisAvvmBalances
configAvvmDistr = gdAvvmDistr . configGenesisData

-- | Construct a 'Config' from a 'StaticConfig'
--
--   If the 'StaticConfig' refers to a canonical JSON file, then it will be
--   hashed and checked against the expected hash.
--
--   If the 'StaticConfig' contains a 'GenesisSpec', then a full 'GenesisData'
--   will be generated. In this case a start time must be provided.
mkConfigFromStaticConfig
  :: (MonadError ConfigurationError m, MonadIO m)
  => FilePath
  -- ^ Directory where 'configuration.yaml' is stored
  -> Maybe UTCTime
  -- ^ Optional system start time.
  --   It must be given when the genesis spec uses a testnet initializer.
  -> Maybe Integer
  -- ^ Optional seed which overrides one from testnet initializer if provided
  -> StaticConfig
  -> m Config
mkConfigFromStaticConfig confDir mSystemStart mSeed = \case
  -- If a 'GenesisData' source file is given, we check its hash against the
  -- given expected hash, parse it, and use the GenesisData to fill in all of
  -- the obligations.
  GCSrc fp expectedHash -> do

    isNothing mSystemStart `orThrowError` UnnecessarySystemStartTime

    isNothing mSeed `orThrowError` MeaninglessSeed

    mkConfigFromFile (confDir </> fp) (Just expectedHash)


  -- If a 'GenesisSpec' is given, we ensure we have a start time (needed if it's
  -- a testnet initializer) and then make a 'GenesisData' from it.
  GCSpec spec -> do

    systemStart <- maybe (throwError MissingSystemStartTime) pure mSystemStart

    -- Override seed if necessary
    let
      overrideSeed :: Integer -> GenesisInitializer -> GenesisInitializer
      overrideSeed newSeed gi = gi { giSeed = newSeed }

    let
      spec' = case mSeed of
        Nothing -> spec
        Just newSeed ->
          spec { gsInitializer = overrideSeed newSeed (gsInitializer spec) }

    mkConfig systemStart spec'

mkConfigFromFile
  :: (MonadError ConfigurationError m, MonadIO m)
  => FilePath
  -> Maybe (Hash Raw)
  -> m Config
mkConfigFromFile fp mGenesisHash = do
  (genesisData, genesisHash) <-
    liftEither . first ConfigurationGenesisDataError =<< runExceptT
      (readGenesisData fp)

  case mGenesisHash of
    Nothing -> pure ()
    Just expectedHash ->
      (unGenesisHash genesisHash == expectedHash)
        `orThrowError` GenesisHashMismatch genesisHash expectedHash

  pure $ Config
    { configGenesisData      = genesisData
    , configGenesisHash      = genesisHash
    , configGeneratedSecrets = Nothing
    }

mkConfig
  :: MonadError ConfigurationError m => UTCTime -> GenesisSpec -> m Config
mkConfig startTime genesisSpec = do
  (genesisData, generatedSecrets) <-
    liftEither . first ConfigurationGenerationError $ generateGenesisData
      startTime
      genesisSpec

  pure $ Config
    { configGenesisData      = genesisData
    , configGenesisHash      = genesisHash
    , configGeneratedSecrets = Just generatedSecrets
    }
  where
    -- Anything will do for the genesis hash. A hash of "patak" was used before,
    -- and so it remains.
        genesisHash = GenesisHash $ coerce $ hash @Text "patak"

data ConfigurationError
  = MissingSystemStartTime
  -- ^ A system start time must be given when a testnet genesis is used
  | UnnecessarySystemStartTime
  -- ^ Must not give a custom system start time when using a mainnet genesis
  | ConfigurationGenesisDataError GenesisDataError
  -- ^ An error in constructing 'GenesisData'
  | GenesisHashMismatch GenesisHash (Hash Raw)
  -- ^ The GenesisData canonical JSON hash is different than expected
  | MeaninglessSeed
  -- ^ Custom seed was provided, but it doesn't make sense
  | ConfigurationGenerationError GenesisDataGenerationError
  deriving (Show)
