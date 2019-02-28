{-# LANGUAGE LambdaCase #-}

module Cardano.Chain.Epoch.Validation
  ( EpochError(..)
  , validateEpochFile
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Cardano.Chain.Block
  ( ABlockOrBoundary(..)
  , ChainValidationError
  , ChainValidationState
  , blockSlot
  , updateChain
  )
import Cardano.Chain.Epoch.File (ParseError, parseEpochFileWithBoundary)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (SlotId)


data EpochError
  = EpochParseError ParseError
  | EpochChainValidationError (Maybe SlotId) ChainValidationError
  | Initial
  deriving (Eq, Show)


-- | Check that a single epoch's 'Block's are valid by folding over them
validateEpochFile
  :: Genesis.Config
  -> ChainValidationState
  -> FilePath
  -> IO (Either EpochError ChainValidationState)
validateEpochFile config cvs fp =
  runResourceT . runExceptT . foldChainValidationState config cvs $ S.map
    fst
    stream
  where stream = parseEpochFileWithBoundary fp


-- | Fold chain validation over a 'Stream' of 'Blund's
foldChainValidationState
  :: Genesis.Config
  -> ChainValidationState
  -> Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT EpochError ResIO ChainValidationState
foldChainValidationState config cvs blocks = S.foldM_
  (\c b ->
    withExceptT (EpochChainValidationError (blockOrBoundarySlot b))
      $ updateChain config c b
  )
  (pure cvs)
  pure
  (hoist (withExceptT EpochParseError) blocks)
 where
  blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe SlotId
  blockOrBoundarySlot = \case
    ABOBBoundary _     -> Nothing
    ABOBBlock    block -> Just $ blockSlot block
