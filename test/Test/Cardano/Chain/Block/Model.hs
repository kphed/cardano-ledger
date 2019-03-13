{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Cardano.Chain.Block.Model
  ( prop_commandCHAIN
  )
where

import Cardano.Prelude

import Control.Lens
import Data.Coerce
import Data.IORef

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Cardano.Chain.Block as Concrete
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Control.State.Transition as STS
import qualified Control.State.Transition.Generator as STS
import qualified Ledger.Delegation as Abstract

import Test.Cardano.Chain.Elaboration.Block


--------------------------------------------------------------------------------
-- CHAIN
--------------------------------------------------------------------------------

prop_commandCHAIN :: Property
prop_commandCHAIN = withTests 25 . property $ do

  env <- forAll $ STS.initEnvGen @CHAIN

  initialAbstractState <- either (panic . show) pure
    $ STS.applySTS @CHAIN (STS.IRC env)

  let
    initialStateCHAIN :: StateCHAIN v
    initialStateCHAIN =
      StateCHAIN initialAbstractState [] (Right initialAbstractState)

  let
    initialConcreteState = either (panic . show) identity
      $ Concrete.initialChainValidationState (abEnvToCfg env)

  concreteRef <- liftIO $ newIORef initialConcreteState

  actions     <- forAll $ Gen.sequential
    (Range.linear 1 30)
    initialStateCHAIN
    [commandCHAIN concreteRef env]

  liftIO $ writeIORef concreteRef initialConcreteState

  executeSequential initialStateCHAIN actions


data StateCHAIN (v :: Type -> Type) = StateCHAIN
  { abstractState :: STS.State CHAIN
  , certs :: [Abstract.DCert]
  , lastAbstractResult :: Either [STS.PredicateFailure CHAIN] (STS.State CHAIN)
  }


data SignalCHAIN (v :: Type -> Type)
  = SignalCHAIN (Maybe Abstract.DCert) (STS.Signal CHAIN)
  deriving Show

instance HTraversable SignalCHAIN where
  htraverse _ s = pure (coerce s)


commandCHAIN
  :: forall m
   . MonadIO m
  => IORef Concrete.ChainValidationState
  -> STS.Environment CHAIN
  -> Command Gen m StateCHAIN
commandCHAIN concreteRef env = Command gen execute callbacks
 where
  gen :: StateCHAIN v -> Maybe (Gen (SignalCHAIN v))
  gen StateCHAIN { abstractState, certs } =
    Just $ SignalCHAIN <$> genCert <*> STS.sigGen @CHAIN env abstractState
   where
    genCert :: Gen (Maybe Abstract.DCert)
    genCert = case certs of
      []     -> pure Nothing
      certs' -> Just <$> Gen.element certs'

  execute
    :: SignalCHAIN Concrete
    -> m
         (Either Concrete.ChainValidationError Concrete.ChainValidationState)
  execute (SignalCHAIN cert block) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      concreteBlock =
        elaborateBS (abEnvToCfg env) env cert concreteState block

      result
        :: Either Concrete.ChainValidationError Concrete.ChainValidationState
      result =
        Concrete.updateChain (abEnvToCfg env) concreteState concreteBlock

    liftIO . writeIORef concreteRef $ fromRight concreteState result

    pure result

  callbacks
    :: [ Callback
           SignalCHAIN
           (Either Concrete.ChainValidationError Concrete.ChainValidationState)
           StateCHAIN
       ]
  callbacks =
    [ Update
      $ \StateCHAIN { abstractState, certs } (SignalCHAIN _ block) _ ->
          let
            result =
              STS.applySTS @CHAIN (STS.TRC (env, abstractState, block))
            certs' = if isRight result
              then block ^. Abstract.bBody . Abstract.bDCerts ++ certs
              else certs
          in StateCHAIN (fromRight abstractState result) certs' result
    , Ensure $ \_ StateCHAIN { lastAbstractResult } _ result -> do
      annotateShow lastAbstractResult
      annotateShow result
      isRight lastAbstractResult === isRight result
    ]
