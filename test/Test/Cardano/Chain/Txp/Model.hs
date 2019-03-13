{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Chain.Txp.Model
  ( tests
  )
where

import Cardano.Prelude

import Text.Show
import Data.IORef
import qualified Data.Map.Strict as M
import Unsafe.Coerce

import qualified Cardano.Chain.Txp as Concrete
import Cardano.Crypto
import qualified Control.State.Transition as STS
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.State

import qualified Test.Cardano.Chain.Elaboration.UTxO as E
import Test.Cardano.Crypto.Dummy


--------------------------------------------------------------------------------
-- UTXOW
--------------------------------------------------------------------------------

prop_commandUTXOW :: Property
prop_commandUTXOW = withDiscards 1000 . withTests 1000 . property $ do
  let
    genAbstractValue :: MonadGen m => m Abstract.Value
    genAbstractValue =
      Abstract.Value . fromIntegral <$> Gen.word32 (Range.linear 0 1000000)

    -- Each address should only appear once in the starting balances
    genAbstractTxOuts :: MonadGen m => m [Abstract.TxOut]
    genAbstractTxOuts = do
      addrs <- Gen.subsequence modelAddresses
      traverse (\a -> Abstract.TxOut a <$> genAbstractValue) addrs

    -- genAbstractTxOut :: MonadGen m => m Abstract.TxOut
    -- genAbstractTxOut = Abstract.TxOut <$> genModelAddr <*> genAbstractValue

  initialBalances <- forAll genAbstractTxOuts -- Gen.list (Range.linear 0 30) genAbstractTxOut

  let
    initialConcreteState =
      E.elaborateUTxO $ mkInitialAbstractUTxO initialBalances

  concreteRef <- liftIO $ newIORef initialConcreteState

  let
    env :: forall v . STS.Environment (UTXOW v)
    env = Abstract.UTxOEnv
      { Abstract.utxo0 = mkInitialAbstractUTxO initialBalances
      , Abstract.pps   = Abstract.ProtocolConstants (\_ -> Abstract.Value 0)
      }

  actions <- forAll $ Gen.sequential
    (Range.linear 1 100)
    (initialStateUTXOW env)
    [commandUTXOW concreteRef env]

  liftIO $ writeIORef concreteRef initialConcreteState

  executeSequential (initialStateUTXOW env) actions
 where
  mkInitialAbstractUTxO :: [Abstract.TxOut] -> E.UTxO v
  mkInitialAbstractUTxO = unsafeCoerce . mkInitialAbstractUTxO'

  mkInitialAbstractUTxO' :: [Abstract.TxOut] -> E.UTxO Symbolic
  mkInitialAbstractUTxO' =
    Abstract.UTxO . M.fromList . fmap mkUTxOEntry

  mkUTxOEntry :: Abstract.TxOut -> (E.TxIn Symbolic, Abstract.TxOut)
  mkUTxOEntry o@(Abstract.TxOut a _) = (Abstract.TxIn (Left a) 0, o)


type UTXOW v = Abstract.UTXOW (E.TxId v)

data StateUTXOW (v :: Type -> Type) = StateUTXOW
  { abstractState :: E.UTxO v
  , lastResult    :: Either [STS.PredicateFailure (UTXOW v)] (E.UTxO v)
  } deriving (Eq, Show)

initialStateUTXOW :: STS.Environment (UTXOW v) -> StateUTXOW v
initialStateUTXOW (Abstract.UTxOEnv initialUTxO _) = StateUTXOW
  { abstractState = initialUTxO
  , lastResult    = Right initialUTxO
  }


newtype SignalUTXOW (v :: Type -> Type)
  = SignalUTXOW (STS.Signal (UTXOW v))
  deriving (Show)

instance HTraversable SignalUTXOW where
  htraverse
    :: forall f g h
    . Applicative f
    => (forall a . Var a g -> f (Var a h))
    -> SignalUTXOW g
    -> f (SignalUTXOW h)
  htraverse f (SignalUTXOW t) = SignalUTXOW <$> htraverseTxWits t
   where
    htraverseTxWits :: E.TxWits g -> f (E.TxWits h)
    htraverseTxWits (Abstract.TxWits body witnesses) =
      Abstract.TxWits
        <$> htraverseTx body
        <*> traverse htraverseWitness witnesses

    htraverseTx :: E.Tx g -> f (E.Tx h)
    htraverseTx (Abstract.Tx id inputs outputs) =
      Abstract.Tx
        <$> htraverseTxId id
        <*> traverse htraverseInput inputs
        <*> pure outputs

    htraverseInput :: E.TxIn g -> f (E.TxIn h)
    htraverseInput (Abstract.TxIn txId index) =
      Abstract.TxIn <$> htraverseTxId txId <*> pure index

    htraverseTxId :: E.TxId g -> f (E.TxId h)
    htraverseTxId (Left a)     = pure $ Left a
    htraverseTxId (Right txId) = Right <$> htraverse f txId

    htraverseWitness :: E.Wit g -> f (E.Wit h)
    htraverseWitness (Abstract.Wit key (Abstract.Sig tx owner)) =
      Abstract.Wit key <$> (Abstract.Sig <$> htraverseTx tx <*> pure owner)


data ConcreteResult = ConcreteResult
  { crResult :: Either Concrete.UTxOValidationError Concrete.UTxO
  , crAppliedTx :: Concrete.Tx
  } deriving (Eq, Show)


commandUTXOW
  :: forall m
   . MonadIO m
  => IORef Concrete.UTxO
  -> (forall v . STS.Environment (UTXOW v))
  -> Command Gen m StateUTXOW
commandUTXOW concreteRef env = Command gen execute callbacks
 where
  gen :: StateUTXOW Symbolic -> Maybe (Gen (SignalUTXOW Symbolic))
  gen StateUTXOW { abstractState } =
    Just
    .   fmap SignalUTXOW
    $   Abstract.makeTxWits abstractState
    <$> genAbstractTx
   where
    genAbstractTx :: Gen (E.Tx Symbolic)
    genAbstractTx = do
      ins  <- Gen.filter (not . null) (Gen.subsequence (M.keys m))
      outs <- genEqualTxOuts abstractState ins
      let placeholderId = Abstract.Addr (Abstract.VKey (Abstract.Owner 0))
      pure $ Abstract.Tx (Left placeholderId) ins outs
    Abstract.UTxO m = abstractState

  execute :: SignalUTXOW Concrete -> m ConcreteResult
  execute (SignalUTXOW txWits) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      concreteTxAux = E.elaborateTxWitsBS (Abstract.utxo0 env) txWits

      result :: Either Concrete.UTxOValidationError Concrete.UTxO
      result = Concrete.updateUTxOWitness
        dummyProtocolMagicId
        (E.elaborateUTxOEnv env)
        concreteState
        concreteTxAux

    liftIO . writeIORef concreteRef $ fromRight concreteState result

    pure $ ConcreteResult
      { crResult    = result
      , crAppliedTx = Concrete.taTx concreteTxAux
      }

  callbacks :: [Callback SignalUTXOW ConcreteResult StateUTXOW]
  callbacks =
    [ Update update
    , Ensure $ \_ StateUTXOW { lastResult } _ concreteResult ->
      case (fmap E.elaborateUTxO lastResult, crResult concreteResult) of
        (Left e, Left e') -> do
          annotateShow e
          annotateShow e'
          failure
        (Left e, Right _) -> do
          annotateShow e
          failure
        (Right _, Left e) -> do
          annotateShow e
          failure
        (Right u, Right u') -> u === u'
    ]

  update
    :: forall v
    .  StateUTXOW v
    -> SignalUTXOW v
    -> Var ConcreteResult v
    -> StateUTXOW v
  update StateUTXOW { abstractState } (SignalUTXOW txWits) concreteResult =
    let
      varTxId = case concreteResult of
        VarSymbolic (Symbolic s) -> VarSymbolic (Symbolic s)
        VarConcrete (Concrete cr) ->
          VarConcrete (Concrete (hash $ crAppliedTx cr))

      Abstract.TxWits { Abstract.body } = txWits

      txWits' =
        txWits { Abstract.body = body { Abstract.txid = Right varTxId } }

      result =
        STS.applySTS @(UTXOW v) (STS.TRC (env, abstractState, txWits'))
    in StateUTXOW (fromRight abstractState result) result

instance Show (Abstract.ProtocolConstants id) where
  show _ = "ProtocolConstants { pcMinFee = <function> }"

genEqualTxOuts
  :: MonadGen m => E.UTxO Symbolic -> [E.TxIn Symbolic] -> m [Abstract.TxOut]
genEqualTxOuts utxo ins = do
  -- Choose a number of outputs
  noOuts <- Gen.int (Range.linear 1 10)

  -- Split the input balance equally between the outputs
  let
    Abstract.Value inputBalance = Abstract.balance (ins Abstract.<| utxo)
    outputBalance     = Abstract.Value $ inputBalance `div` fromIntegral noOuts
    Abstract.Value ob = outputBalance
    remainder = Abstract.Value $ inputBalance - (ob * fromIntegral noOuts)

  let
    balances =
      (outputBalance <> remainder) : replicate (noOuts - 1) outputBalance

  -- Generate the right number of addresses
  addrs <- replicateM noOuts genModelAddr

  -- Convert to TxOuts
  pure $ uncurry Abstract.TxOut <$> zip addrs balances

genModelAddr :: MonadGen m => m Abstract.Addr
genModelAddr = Gen.element modelAddresses

modelAddresses :: [Abstract.Addr]
modelAddresses = Abstract.Addr . Abstract.VKey . Abstract.Owner <$> [0 .. 15]


-----------------------------------------------------------------------
-- Main Test Export
-----------------------------------------------------------------------

tests :: IO Bool
tests = checkSequential $$discover
