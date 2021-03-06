{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Common.StakeholderId
  ( StakeholderId
  , mkStakeholderId
  , shortStakeholderF
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSONKey, ToJSONKey)
import Formatting (Format, formatToString, mapf)
import Formatting.Buildable (Buildable)
import Text.JSON.Canonical (FromObjectKey(..), JSValue(..), ToObjectKey(..))

import Cardano.Binary.Class (Bi)
import Cardano.Chain.Common.AddressHash
import Cardano.Crypto (decodeAbstractHash, hashHexF, shortHashF)
import Cardano.Crypto.Signing (PublicKey)


-- | Stakeholder identifier (stakeholders are identified by their public keys)
newtype StakeholderId = StakeholderId
  { getStakeholderId :: AddressHash PublicKey
  } deriving ( Eq
             , Ord
             , Show
             , NFData
             , Buildable
             , FromJSONKey
             , ToJSONKey
             , Bi
             , HeapWords
             )

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . formatToString hashHexF . getStakeholderId

instance MonadError SchemaError m => FromObjectKey m StakeholderId where
    fromObjectKey = fmap (Just . StakeholderId)
        . parseJSString decodeAbstractHash
        . JSString

mkStakeholderId :: PublicKey -> StakeholderId
mkStakeholderId = StakeholderId . addressHash

shortStakeholderF :: Format r (StakeholderId -> r)
shortStakeholderF = mapf getStakeholderId shortHashF
