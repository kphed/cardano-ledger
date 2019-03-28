{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Epoch.File
  ( tests
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Hedgehog (Property, (===))
import qualified Hedgehog as H
import Streaming (Of((:>)))
import qualified Streaming as S

import Cardano.Chain.Epoch.File (ParseError, parseEpochFiles)
import Cardano.Chain.Slotting (EpochSlots(EpochSlots))
import Cardano.Mirror (mainnetEpochFiles)


tests :: IO Bool
tests = H.checkSequential $$(H.discoverPrefix "prop")

propDeserializeEpochs :: Property
propDeserializeEpochs = H.withTests 1 $ H.property $ do
  files <- take 10 <$> liftIO mainnetEpochFiles
  H.assert $ not (null files)
  -- TODO: the property cannot take any parameters (if we want discoverPrefix
  -- to work). Now the question is whether it is OK to use an hardcoded value
  -- for the number of slots per epoch, and if so in which module should we
  -- store this constant?
  let stream = parseEpochFiles (EpochSlots 21600) files
  result <- (liftIO . runResourceT . runExceptT . S.run) (S.maps discard stream)
  result === Right ()
 where
  discard :: Of a m -> ExceptT ParseError ResIO m
  discard (_ :> rest) = pure rest
