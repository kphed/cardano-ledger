module Test.Cardano.Chain.Block.Gen
  ( genBlockSignature
  , genHeaderHash
  , genHeader
  , genHeaderWithEpochSlots
  , genBody
  , genConsensusData
  , genExtraBodyData
  , genExtraHeaderData
  , genProof
  , genToSign
  , genBlock
  , genBlockWithEpochSlots
  , genSlogUndo
  , genUndo
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import Cardano.Chain.Block
  ( Block
  , BlockSignature(..)
  , Body
  , ConsensusData
  , hashHeader
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
  , mkBlockExplicit
  , mkHeaderExplicit
  )
import Cardano.Chain.Common (mkAttributes)
import Cardano.Chain.Slotting (EpochSlots, WithEpochSlots(WithEpochSlots))
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto (ProtocolMagicId, abstractHash)

import Test.Cardano.Chain.Common.Gen (genChainDifficulty)
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import Test.Cardano.Chain.Slotting.Gen
  ( genEpochIndex
  , genEpochSlots
  , genFlatSlotId
  , genSlotId
  )
import Test.Cardano.Chain.Txp.Gen (genTxPayload, genTxProof, genTxpUndo)
import qualified Test.Cardano.Chain.Update.Gen as Update
import Test.Cardano.Crypto.Gen
  ( genAbstractHash
  , genProxySignature
  , genPublicKey
  , genSecretKey
  , genSignature
  , genTextHash
  )


genBlockSignature :: ProtocolMagicId -> EpochSlots -> Gen BlockSignature
genBlockSignature pm epochSlots = Gen.choice
  [ BlockSignature <$> genSignature pm mts
  , BlockPSignatureHeavy <$> genProxySignature pm mts genEpochIndex
  ]
  where mts = genToSign pm epochSlots

genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash

genBody :: ProtocolMagicId -> Gen Body
genBody pm =
  body
    <$> genTxPayload pm
    <*> pure SscPayload
    <*> Delegation.genPayload pm
    <*> Update.genPayload pm

genHeaderWithEpochSlots :: ProtocolMagicId -> Gen (WithEpochSlots Header)
genHeaderWithEpochSlots pm = do
  epochSlots <- genEpochSlots
  h <- genHeader pm epochSlots
  pure $! WithEpochSlots epochSlots h

-- We use `Nothing` as the ProxyVKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
genHeader :: ProtocolMagicId -> EpochSlots -> Gen Header
genHeader pm epochSlots =
  mkHeaderExplicit pm
    <$> genHeaderHash
    <*> genChainDifficulty
    <*> pure epochSlots
    <*> genFlatSlotId
    <*> genSecretKey
    <*> pure Nothing
    <*> genBody pm
    <*> genExtraHeaderData

genConsensusData :: ProtocolMagicId -> EpochSlots -> Gen ConsensusData
genConsensusData pm epochSlots =
  consensusData
    <$> genFlatSlotId
    <*> genPublicKey
    <*> genChainDifficulty
    <*> genBlockSignature pm epochSlots

genExtraBodyData :: Gen ExtraBodyData
genExtraBodyData = pure . ExtraBodyData $ mkAttributes ()

genExtraHeaderData :: Gen ExtraHeaderData
genExtraHeaderData =
  ExtraHeaderData
    <$> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
    <*> pure (mkAttributes ())
    <*> genAbstractHash genExtraBodyData

genProof :: ProtocolMagicId -> Gen Proof
genProof pm =
  Proof
    <$> genTxProof pm
    <*> pure SscProof
    <*> genAbstractHash (Delegation.genPayload pm)
    <*> Update.genProof pm

genToSign :: ProtocolMagicId -> EpochSlots -> Gen ToSign
genToSign pm epochSlots =
  ToSign
    <$> (mkAbstractHash <$> genHeader pm epochSlots)
    <*> genProof pm
    <*> genSlotId epochSlots
    <*> genChainDifficulty
    <*> genExtraHeaderData
  where
    mkAbstractHash :: Header -> HeaderHash
    mkAbstractHash = hashHeader epochSlots

genBlockWithEpochSlots :: ProtocolMagicId -> Gen (WithEpochSlots Block)
genBlockWithEpochSlots pm = do
  epochSlots <- genEpochSlots
  b  <- genBlock pm epochSlots
  pure $! WithEpochSlots epochSlots b

genBlock :: ProtocolMagicId -> EpochSlots -> Gen Block
genBlock pm epochSlots =
  mkBlockExplicit pm
    <$> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
    <*> genHeaderHash
    <*> genChainDifficulty
    <*> pure epochSlots
    <*> genFlatSlotId
    <*> genSecretKey
    <*> pure Nothing
    <*> genBody pm

genSlogUndo :: Gen SlogUndo
genSlogUndo = SlogUndo <$> Gen.maybe genFlatSlotId

genUndo :: ProtocolMagicId -> EpochSlots -> Gen Undo
genUndo pm epochSlots =
  Undo
    <$> genTxpUndo
    <*> Delegation.genUndo pm
    <*> Update.genUndo pm epochSlots
    <*> genSlogUndo
