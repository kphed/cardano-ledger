{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

-- | Validation of delegation certificates and updating of delegation state
--
--   Delegation is split into two phases, Scheduling and Activation. During the
--   Scheduling phase new delegation certificates are validated and added to a
--   queue of 'ScheduledDelegation's. During the Activation phase
--   'ScheduledDelgation's are added to the active delegation state.
module Cardano.Chain.Delegation.Validation
  (
  -- * Scheduling
    SchedulingEnvironment(..)
  , SchedulingState(..)
  , SchedulingError(..)
  , ScheduledDelegation(..)
  , scheduleCertificate

  -- * Activation
  , ActivationState(..)
  , activateDelegation

  -- * Blockchain Interface
  , InterfaceState(..)
  , initialInterfaceState
  , delegates
  , updateDelegation

  -- * Misc utility functions
  , delegatorOf
  )
where

import Cardano.Prelude

import qualified Data.Map as M
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Binary.Class (Annotated(..), serialize')
import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import Cardano.Chain.Genesis as Genesis
  ( Config
  , GenesisDelegation(..)
  , GenesisWStakeholders(..)
  , configBootStakeholders
  , configEpochSlots
  , configHeavyDelegation
  , configProtocolMagicId
  )
import Cardano.Chain.Slotting
  ( EpochIndex
  , FlatSlotId(..)
  , SlotCount(..)
  , addSlotNumber
  , slotNumberEpoch
  , subSlotNumber
  )
import Cardano.Crypto
  ( AProxyVerificationKey(..)
  , ProtocolMagicId
  , PublicKey
  , pskOmega
  , validateProxyVerificationKey
  )


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data SchedulingEnvironment = SchedulingEnvironment
  { seGenesisKeys  :: Set StakeholderId
  , seCurrentEpoch :: EpochIndex
  , seCurrentSlot  :: FlatSlotId
  , seStableAfter     :: SlotCount
  }

data SchedulingState = SchedulingState
  { ssScheduledDelegations :: !(Seq ScheduledDelegation)
  , ssKeyEpochDelegations  :: !(Set (EpochIndex, StakeholderId))
  } deriving (Eq, Show, Generic, NFData)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot      :: !FlatSlotId
  , sdDelegator :: !StakeholderId
  , sdDelegate  :: !StakeholderId
  } deriving (Eq, Show, Generic, NFData)

data SchedulingError

  = SchedulingInvalidCertificate Text
  -- ^ The delegation certificate has an invalid signature

  | SchedulingMultipleDelegationsForEpoch EpochIndex StakeholderId
  -- ^ This delegator has already delegated for the given epoch

  | SchedulingMultipleDelegationsForSlot FlatSlotId StakeholderId
  -- ^ This delegator has already delgated in this slot

  | SchedulingNonGenesisDelegator StakeholderId
  -- ^ This delegator is not one of the allowed genesis keys

  | SchedulingPastEpoch EpochIndex EpochIndex
  -- ^ This delegation is for a past epoch

  deriving (Eq, Show)


-- | Update the delegation 'SchedulingState' with a 'Certificate' if it passes
--   all the validation rules. This is an implementation of the delegation
--   scheduling inference rule from the ledger specification.
scheduleCertificate
  :: MonadError SchedulingError m
  => ProtocolMagicId
  -> SchedulingEnvironment
  -> SchedulingState
  -> ACertificate ByteString
  -> m SchedulingState
scheduleCertificate pm env ss cert = do

  -- Check that the delegator is a genesis key
  (delegator `Set.member` seGenesisKeys)
    `orThrowError` SchedulingNonGenesisDelegator delegator

  -- Check that the delegation epoch is greater than or equal to the current one
  (seCurrentEpoch <= delegationEpoch)
    `orThrowError` SchedulingPastEpoch seCurrentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  ((delegationEpoch, delegator) `Set.notMember` ssKeyEpochDelegations ss)
    `orThrowError` SchedulingMultipleDelegationsForEpoch
                    delegationEpoch
                    delegator

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot (ssScheduledDelegations ss))
    `orThrowError` SchedulingMultipleDelegationsForSlot seCurrentSlot delegator

  -- Check that the delegation certificate is valid
  validateProxyVerificationKey pm cert `wrapError` SchedulingInvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ SchedulingState
    { ssScheduledDelegations = delegation <| ssScheduledDelegations ss
    , ssKeyEpochDelegations  = Set.insert
      (delegationEpoch, delegator)
      (ssKeyEpochDelegations ss)
    }
 where
  delegator = mkStakeholderId $ pskIssuerPk cert
  delegate  = mkStakeholderId $ pskDelegatePk cert

  SchedulingEnvironment { seGenesisKeys, seCurrentEpoch, seCurrentSlot, seStableAfter }
    = env

  delegationEpoch = pskOmega cert

  activationSlot  = addSlotNumber seStableAfter seCurrentSlot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegator

  delegation = ScheduledDelegation activationSlot delegator delegate


--------------------------------------------------------------------------------
-- Activation
--------------------------------------------------------------------------------

-- | Maps containing, for each delegator, the active delegation and the slot it
--   became active in.
data ActivationState = ActivationState
  { asDelegationMap   :: !(Map StakeholderId StakeholderId)
  , asDelegationSlots :: !(Map StakeholderId FlatSlotId)
  } deriving (Eq, Show, Generic, NFData)


-- | Find the delegator of the given stakeholder-id.
--
-- The function returns nothing if no delegator is found. This function does
-- not check injectivity of the delegation map.
delegatorOf
  :: StakeholderId -> Map StakeholderId StakeholderId -> Maybe StakeholderId
delegatorOf vk dms = case M.keys $ M.filter (== vk) dms of
  vkS : _ -> Just vkS
  _       -> Nothing

-- | Activate a 'ScheduledDelegation' if its activation slot is less than the
--   previous delegation slot for this delegate, otherwise discard it. This is
--   an implementation of the delegation activation rule in the ledger
--   specification.
activateDelegation :: ActivationState -> ScheduledDelegation -> ActivationState
activateDelegation as delegation
  | prevDelegationSlot < slot || unFlatSlotId slot == 0 = ActivationState
    { asDelegationMap   = M.insert delegator delegate delegationMap
    , asDelegationSlots = M.insert delegator slot delegationSlots
    }
  | otherwise = as
 where
  ActivationState delegationMap delegationSlots = as
  ScheduledDelegation slot delegator delegate   = delegation

  prevDelegationSlot =
    fromMaybe (FlatSlotId 0) $ M.lookup delegator (asDelegationSlots as)


--------------------------------------------------------------------------------
-- Blockchain Interface
--------------------------------------------------------------------------------

-- | State shared between the blockchain and the ledger
data InterfaceState = InterfaceState
  { isSchedulingState :: !SchedulingState
  , isActivationState :: !ActivationState
  } deriving (Eq, Show, Generic, NFData)


-- | The initial state maps each genesis key to itself and overrides this using
--   certificates from the genesis block.
initialInterfaceState
  :: MonadError SchedulingError m => Genesis.Config -> m InterfaceState
initialInterfaceState config = updateDelegation
  config
  (FlatSlotId 0)
  (SlotCount 0)
  is
  certificates
 where
  is = InterfaceState
    { isSchedulingState = SchedulingState
      { ssScheduledDelegations = mempty
      , ssKeyEpochDelegations  = mempty
      }
    , isActivationState = ActivationState
      { asDelegationMap   = M.fromList $ zip genesisKeys genesisKeys
      , asDelegationSlots = M.fromList $ (, FlatSlotId 0) <$> genesisKeys
      }
    }

  genesisKeys =
    M.keys . getGenesisWStakeholders $ configBootStakeholders config

  certificates =
    fmap annotateCertificate
      . M.elems
      . unGenesisDelegation
      $ configHeavyDelegation config

  annotateCertificate :: Certificate -> ACertificate ByteString
  annotateCertificate c = UnsafeAProxyVerificationKey
    { aPskOmega     = Annotated (pskOmega c) (serialize' $ pskOmega c)
    , pskIssuerPk   = pskIssuerPk c
    , pskDelegatePk = pskDelegatePk c
    , pskCert       = pskCert c
    }


-- | Check whether a delegation is valid in the 'InterfaceState'
delegates :: InterfaceState -> PublicKey -> PublicKey -> Bool
delegates is delegator delegate =
  case M.lookup (mkStakeholderId delegator) delegationMap of
    Nothing -> False
    Just pk -> pk == mkStakeholderId delegate
  where delegationMap = asDelegationMap $ isActivationState is


-- | Update the 'InterfaceState' with a list of new 'Certificate's. This is an
--   implementation of the delegation interface rule from the ledger
--   specification.
updateDelegation
  :: MonadError SchedulingError m
  => Genesis.Config
  -> FlatSlotId
  -> SlotCount
  -> InterfaceState
  -> [ACertificate ByteString]
  -> m InterfaceState
updateDelegation config slot d is certificates = do

  -- Schedule new certificates
  SchedulingState delegations keyEpochs <- foldM
    (scheduleCertificate (configProtocolMagicId config) env)
    (isSchedulingState is)
    certificates

  -- Activate certificates up to this slot
  let
    as = foldl
      activateDelegation
      (isActivationState is)
      (Seq.filter ((<= slot) . sdSlot) delegations)

  -- Remove stale values from 'SchedulingState'
  let
    inWindow s = subSlotNumber d slot <= s && s <= addSlotNumber d slot
    epoch = slotNumberEpoch (configEpochSlots config) slot

    ss'   = SchedulingState
      { ssScheduledDelegations = Seq.filter (inWindow . sdSlot) delegations
      , ssKeyEpochDelegations  = Set.filter ((>= epoch) . fst) keyEpochs
      }

  pure $ InterfaceState {isSchedulingState = ss', isActivationState = as}
 where
  env = SchedulingEnvironment
    { seGenesisKeys  = Set.fromList
      . M.keys
      . getGenesisWStakeholders
      $ configBootStakeholders config
    , seCurrentEpoch = slotNumberEpoch (configEpochSlots config) slot
    , seCurrentSlot  = slot
    , seStableAfter  = d
    }
