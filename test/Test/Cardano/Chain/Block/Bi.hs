{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Block.Bi
  ( tests
  , exampleBlockSignature
  , exampleBlockPSignatureHeavy
  , exampleBody
  , exampleConsensusData
  , exampleHeader
  , exampleProof
  , exampleToSign
  , exampleUndo
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Coerce (coerce)
import Data.Maybe (fromJust)

import Hedgehog (Property, (===), withTests, property, tripping, Gen)
import qualified Hedgehog as H

import Cardano.Binary.Class
  ( Decoder
  , DecoderError
  , Encoding
  , decodeFullDecoder
  , dropBytes
  , label
  , serializeEncoding
  )
import Cardano.Chain.Block
  ( Block
  , BlockSignature(..)
  , Body
  , ConsensusData
  , ExtraBodyData(..)
  , ExtraHeaderData(..)
  , Header
  , HeaderHash
  , Proof(..)
  , SlogUndo(..)
  , ToSign(..)
  , Undo(..)
  , body
  , consensusData
  , decodeBlockOrBoundary
  , decodeConcensusData
  , decodeHeader
  , decodeHeader'
  , dropBoundaryBody
  , dropBoundaryConsensusData
  , dropBoundaryHeader
  , encodeBlock
  , encodeConcensusData
  , encodeHeader
  , encodeHeader'
  , mkHeaderExplicit
  )
import Cardano.Chain.Common (mkAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlots(EpochSlots)
  , WithEpochSlots(WithEpochSlots)
  , unWithEpochSlots
  )
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagicId(..)
  , SignTag(..)
  , abstractHash
  , createPsk
  , hash
  , noPassSafeSigner
  , proxySign
  , sign
  , toPublic
  )

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode
  , compareHexDump
  , goldenTestBi
  , roundTripsBiBuildable
  , roundTripsBiShow
  )
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import qualified Test.Cardano.Chain.Delegation.Example as Delegation
import Test.Cardano.Chain.Slotting.Example (exampleSlotId, exampleFlatSlotId)
import Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots, genEpochSlots)
import Test.Cardano.Chain.Txp.Example
  (exampleTxPayload, exampleTxProof, exampleTxpUndo)
import qualified Test.Cardano.Chain.Update.Example as Update
import Test.Cardano.Crypto.Example
  (examplePublicKey, exampleSecretKey, exampleSecretKeys)
import Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Number of slots-per-epoch to be used throughout the examples in this
-- module.
exampleEs :: EpochSlots
exampleEs = EpochSlots 50

goldenHeader :: Property
goldenHeader =
  _goldenTestCBOR
    (encodeHeader' exampleEs)
    (decodeHeader' exampleEs)
    exampleHeader
    "test/golden/bi/block/Header"

-- | Re-implementation of 'goldenTestBi' using custom encode and decode functions.
--
-- This is required for the encode/decode golden-tests for types that do no
-- have a 'Bi' instance, such as 'ConcensusData', 'Header', and 'Block'.
--
-- TODO: move to @Test.Cardano.Binary.Helpers.GoldenRoundTrip@, in @binary@
-- package. Also define @goldenTestBi@ in terms of @goldenTestCBOR@
_goldenTestCBOR
  :: forall a
  . (Eq a, Show a, HasCallStack)
  => (a -> Encoding)
  -> (forall s . Decoder s a)
  -> a
  -> FilePath
  -> Property
_goldenTestCBOR enc dec x path = withFrozenCallStack $ do
  let bs' = encodeWithIndex . serializeEncoding . enc $ x
  withTests 1 . property $ do
    bs <- liftIO $ BS.readFile path
    let target = decodeBase16 bs
    compareHexDump bs bs'
    let
      fullDecoder :: BS.ByteString -> Either DecoderError a
      fullDecoder = decodeFullDecoder "" dec -- TODO: do we want to pass the label as parameter?
    fmap fullDecoder target === Just (Right x)

-- TODO: discuss with Ru whether we want to keep this, as it seems to be the
-- same as 'roundTripHeaderBi'.
-- roundTripHeaderBi :: Property
-- roundTripHeaderBi =
--   eachOf 10 (feedPM genHeaderWithEpochSlots) roundTripsBiBuildable

-- | Round-trip test the backwards compatible header encoding/decoding functions
roundTripHeaderCompat :: Property
roundTripHeaderCompat =
  eachOf 10 (feedPM genHeaderWithEpochSlots) roundTripsHeaderCompat
  where
    roundTripsHeaderCompat :: WithEpochSlots Header -> H.PropertyT IO ()
    roundTripsHeaderCompat esh@(WithEpochSlots es h) =
      trippingBuildable
        esh
        (serializeEncoding . encodeHeader es . unWithEpochSlots)
        (fmap (WithEpochSlots es . fromJust) . decodeFullDecoder "Header" (decodeHeader es))


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- TODO: see comment for 'roundTripHeaderBi'
-- roundTripBlock :: Property
-- roundTripBlock =
--   eachOf 10 (feedPM genBlockWithEpochSlots) roundTripsBiBuildable

-- | Round-trip test the backwards compatible block encoding/decoding functions
roundTripBlockCompat :: Property
roundTripBlockCompat =
  eachOf 10 (feedPM genBlockWithEpochSlots) roundTripsBlockCompat
  where
    roundTripsBlockCompat :: WithEpochSlots Block -> H.PropertyT IO ()
    roundTripsBlockCompat esb@(WithEpochSlots es b) =
      trippingBuildable
        esb
        (serializeEncoding . encodeBlock es . unWithEpochSlots)
        (fmap (WithEpochSlots es . fromJust) . decodeFullDecoder "Block" (decodeBlockOrBoundary es))


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

goldenBlockSignature :: Property
goldenBlockSignature =
  goldenTestBi exampleBlockSignature "test/golden/bi/block/BlockSignature"

goldenBlockSignature_Heavy :: Property
goldenBlockSignature_Heavy = goldenTestBi
  exampleBlockPSignatureHeavy
  "test/golden/bi/block/BlockSignature_Heavy"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi =
  eachOf 10 (feedPMEpochSlots genBlockSignature) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBlockHeader :: Property
goldenDeprecatedBoundaryBlockHeader = deprecatedGoldenDecode
  "BoundaryBlockHeader"
  (void dropBoundaryHeader)
  "test/golden/bi/block/BoundaryBlockHeader"


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBody :: Property
goldenDeprecatedBoundaryBody = deprecatedGoldenDecode
  "BoundaryBody"
  dropBoundaryBody
  "test/golden/bi/block/BoundaryBody"


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryConsensusData :: Property
goldenDeprecatedBoundaryConsensusData = deprecatedGoldenDecode
  "BoundaryConsensusData"
  dropBoundaryConsensusData
  "test/golden/bi/block/BoundaryConsensusData"


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

goldenHeaderHash :: Property
goldenHeaderHash =
  goldenTestBi exampleHeaderHash "test/golden/bi/block/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryProof
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryProof :: Property
goldenDeprecatedBoundaryProof = deprecatedGoldenDecode
  "BoundaryProof"
  dropBytes
  "test/golden/bi/block/BoundaryProof"


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

goldenBody :: Property
goldenBody = goldenTestBi exampleBody "test/golden/bi/block/Body"

roundTripBodyBi :: Property
roundTripBodyBi = eachOf 20 (feedPM genBody) roundTripsBiShow


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

goldenConsensusData :: Property
goldenConsensusData =
  _goldenTestCBOR
    (encodeConcensusData exampleEs)
    (decodeConcensusData exampleEs)
    mcd
    "test/golden/bi/block/ConsensusData"
 where
  mcd =
    consensusData
      (exampleFlatSlotId exampleEs)
      examplePublicKey
      exampleChainDifficulty
      exampleBlockSignature

roundTripConsensusData :: Property
roundTripConsensusData =
  eachOf 20 (feedPMEpochSlots $ genWithEpochSlots genConsensusData) roundTripConsensusData -- roundTripsBiShow
  where
    roundTripConsensusData :: WithEpochSlots ConsensusData -> H.PropertyT IO ()
    roundTripConsensusData (WithEpochSlots es cd) =
      tripping
        cd
        (serializeEncoding . encodeConcensusData es)
        (decodeFullDecoder "ConsensusData" $ decodeConcensusData es)

-- TODO: put this in the appropriate place, and think whether we can give a better name.
genWithEpochSlots
  :: (ProtocolMagicId -> EpochSlots -> Gen a)
  -> ProtocolMagicId
  -> EpochSlots
  -> Gen (WithEpochSlots a)
genWithEpochSlots gen pm es = WithEpochSlots es <$> gen pm es

--------------------------------------------------------------------------------
-- ExtraBodyData
--------------------------------------------------------------------------------

goldenExtraBodyData :: Property
goldenExtraBodyData = goldenTestBi mebd "test/golden/bi/block/ExtraBodyData"
  where mebd = ExtraBodyData (mkAttributes ())

roundTripExtraBodyDataBi :: Property
roundTripExtraBodyDataBi = eachOf 1000 genExtraBodyData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ExtraHeaderData
--------------------------------------------------------------------------------

goldenExtraHeaderData :: Property
goldenExtraHeaderData =
  goldenTestBi exampleExtraHeaderData "test/golden/bi/block/ExtraHeaderData"

roundTripExtraHeaderDataBi :: Property
roundTripExtraHeaderDataBi =
  eachOf 1000 genExtraHeaderData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Proof
--------------------------------------------------------------------------------

goldenProof :: Property
goldenProof = goldenTestBi exampleProof "test/golden/bi/block/Proof"

roundTripProofBi :: Property
roundTripProofBi = eachOf 20 (feedPM genProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

goldenToSign :: Property
goldenToSign = goldenTestBi exampleToSign "test/golden/bi/block/ToSign"

roundTripToSignBi :: Property
roundTripToSignBi = eachOf 20 (feedPMEpochSlots genToSign) roundTripsBiShow


--------------------------------------------------------------------------------
-- Undo
--------------------------------------------------------------------------------

goldenUndo :: Property
goldenUndo = goldenTestBi exampleUndo "test/golden/bi/block/Undo"

roundTripUndo :: Property
roundTripUndo = eachOf 20 (feedPMEpochSlots genUndo) roundTripsBiShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleHeader :: Header
exampleHeader = mkHeaderExplicit
  (ProtocolMagicId 7)
  exampleHeaderHash
  exampleChainDifficulty
  exampleEs
  (exampleFlatSlotId exampleEs)
  exampleSecretKey
  Nothing
  exampleBody
  exampleExtraHeaderData

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature
  (sign (ProtocolMagicId 7) SignMainBlock exampleSecretKey exampleToSign)

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
 where
  sig = proxySign pm SignProxyVK delegateSk psk exampleToSign
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  psk = createPsk
    pm
    (noPassSafeSigner issuerSk)
    (toPublic delegateSk)
    (EpochIndex 5)
  pm = ProtocolMagicId 7

exampleConsensusData :: ConsensusData
exampleConsensusData = consensusData
  (exampleFlatSlotId exampleEs)
  examplePublicKey
  exampleChainDifficulty
  exampleBlockSignature

exampleExtraHeaderData :: ExtraHeaderData
exampleExtraHeaderData = ExtraHeaderData
  Update.exampleProtocolVersion
  Update.exampleSoftwareVersion
  (mkAttributes ())
  (abstractHash (ExtraBodyData (mkAttributes ())))

exampleProof :: Proof
exampleProof = Proof
  exampleTxProof
  SscProof
  (abstractHash dp)
  Update.exampleProof
  where dp = Delegation.unsafePayload (take 4 exampleCertificates)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleBody :: Body
exampleBody = body exampleTxPayload SscPayload dp Update.examplePayload
  where dp = Delegation.unsafePayload (take 4 exampleCertificates)

exampleToSign :: ToSign
exampleToSign = ToSign
  exampleHeaderHash
  exampleProof
  (exampleSlotId exampleEs)
  exampleChainDifficulty
  exampleExtraHeaderData

exampleSlogUndo :: SlogUndo
exampleSlogUndo = SlogUndo $ Just 999

exampleUndo :: Undo
exampleUndo = Undo
  { undoTx   = exampleTxpUndo
  , undoDlg  = Delegation.exampleUndo
  , undoUS   = Update.exampleUndo
  , undoSlog = exampleSlogUndo
  }


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
