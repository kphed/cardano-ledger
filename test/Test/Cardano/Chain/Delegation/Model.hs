{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Cardano.Chain.Delegation.Model
  ( prop_commandSDELEG
  )
where

import Cardano.Prelude

import Data.Coerce
import Data.IORef

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Cardano.Chain.Delegation as Concrete
import qualified Control.State.Transition as STS
import qualified Control.State.Transition.Generator as STS
import Ledger.Delegation (DELEG, SDELEG)
import qualified Ledger.Delegation as Abstract

import Test.Cardano.Chain.Elaboration.Delegation
import Test.Cardano.Crypto.Dummy (dummyProtocolMagicId)


--------------------------------------------------------------------------------
-- CHAIN
--------------------------------------------------------------------------------

prop_commandSDELEG :: Property
prop_commandSDELEG = withTests 25 . property $ do
  concreteRef <- liftIO $ newIORef initialConcreteState

  abstractEnv <- forAll $ STS.initEnvGen @DELEG

  actions     <- forAll $ Gen.sequential
    (Range.linear 1 20)
    initialState
    [commandSDELEG concreteRef abstractEnv]

  cleanup concreteRef >> executeSequential initialState actions
 where
  initialConcreteState = Concrete.SchedulingState mempty mempty

  cleanup :: IORef Concrete.SchedulingState -> PropertyT IO ()
  cleanup = liftIO . flip writeIORef initialConcreteState


data StateSDELEG (v :: Type -> Type) = StateSDELEG
  { abstractState      :: Abstract.DSState
  , lastAbstractResult :: Either [STS.PredicateFailure SDELEG] (STS.State SDELEG)
  }

initialState :: StateSDELEG v
initialState = StateSDELEG initialAbstractState (Right initialAbstractState)
  where initialAbstractState = Abstract.DSState [] mempty


newtype SignalSDELEG (v :: Type -> Type)
  = SignalSDELEG (STS.Signal SDELEG)
  deriving Show

instance HTraversable SignalSDELEG where
  htraverse _ s = pure (coerce s)


-- TODO: Change dcertGen to use 'MonadGen'
commandSDELEG
  :: forall m
   . MonadIO m
  => IORef Concrete.SchedulingState
  -> STS.Environment SDELEG
  -> Command Gen m StateSDELEG
commandSDELEG concreteRef abstractEnv = Command gen execute callbacks
 where
  gen :: StateSDELEG v -> Maybe (Gen (SignalSDELEG v))
  gen _ = Just $ SignalSDELEG <$> Abstract.dcertGen abstractEnv

  execute
    :: SignalSDELEG v
    -> m (Either Concrete.SchedulingError Concrete.SchedulingState)
  execute (SignalSDELEG cert) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      result :: Either Concrete.SchedulingError Concrete.SchedulingState
      result = Concrete.scheduleCertificate
        dummyProtocolMagicId
        (elaborateDSEnv abstractEnv)
        concreteState
        (elaborateDCertAnnotated dummyProtocolMagicId cert)

    liftIO . writeIORef concreteRef $ fromRight concreteState result

    pure result

  callbacks
    :: [ Callback
           SignalSDELEG
           (Either Concrete.SchedulingError Concrete.SchedulingState)
           StateSDELEG
       ]
  callbacks =
    [ Update $ \StateSDELEG { abstractState } (SignalSDELEG cert) _ ->
      let
        result =
          STS.applySTS @SDELEG (STS.TRC (abstractEnv, abstractState, cert))
      in StateSDELEG (fromRight abstractState result) result
    , Ensure $ \_ StateSDELEG { lastAbstractResult } _ result -> do
      annotateShow lastAbstractResult
      annotateShow result
      isRight lastAbstractResult === isRight result
    ]
