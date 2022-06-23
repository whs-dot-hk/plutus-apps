{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V1.Typed.Scripts
  ( module Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies,
    module Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators,
    module Plutus.Script.Utils.V1.Typed.Scripts.Validators,
    Validator,
    MintingPolicy,
    StakeValidator,
    makeTypedScriptTxIn,
    TypedScriptTxIn (..),
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    txInValue,
    makePubKeyTxIn,
    typePubKeyTxIn,
    typePubKeyTxOut,
    makeTypedScriptTxOut,
    typeScriptTxOut,
    typeScriptTxOutRef,
    typeScriptTxIn,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, (.:))
import Data.Aeson qualified
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum (Datum), FromData, MintingPolicy,
                             Redeemer (Redeemer), StakeValidator, ToData (..),
                             TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue), TxOutRef, Validator, Value,
                             addressCredential)
import Plutus.V1.Ledger.Tx (TxIn (TxIn, txInRef, txInType),
                            TxInType (ConsumePublicKeyAddress, ConsumeScriptAddress, ConsumeSimpleScriptAddress))

{- Note [Scripts returning Bool]
It used to be that the signal for validation failure was a script being `error`. This is nice for
the validator, since you can determine whether the script evaluation is error-or-not without having
to look at what the result actually *is* if there is one.

However, from the script author's point of view, it would be nicer to return a Bool, since
otherwise you end up doing a lot of `if realCondition then () else error ()` which is rubbish.

So we changed the result type to be Bool. But now we have to answer the question of how the
validator knows what the result value is. All *sorts* of terms can be True or False in disguise.
The easiest way to tell is by reducing it to the previous problem: apply a function which does a
pattern match and returns error in the case of False and () otherwise. Then, as before, we just
check for error in the overall evaluation.
-}

-- | A 'TxIn' tagged by two phantom types: a list of the types of the data scripts in the transaction; and the connection type of the input.
data TypedScriptTxIn a = TypedScriptTxIn
  { tyTxInTxIn   :: TxIn,
    tyTxInOutRef :: TypedScriptTxOutRef a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxIn a) where
  l == r =
    tyTxInTxIn l == tyTxInTxIn r
      && tyTxInOutRef l == tyTxInOutRef r

instance (FromJSON (DatumType a), FromData (DatumType a), ToData (DatumType a)) => FromJSON (TypedScriptTxIn a) where
  parseJSON (Data.Aeson.Object v) =
    TypedScriptTxIn <$> v .: "tyTxInTxIn" <*> v .: "tyTxInOutRef"
  parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxIn a) where
  toJSON TypedScriptTxIn {tyTxInTxIn, tyTxInOutRef} =
    object ["tyTxInTxIn" .= tyTxInTxIn, "tyTxInOutRef" .= tyTxInOutRef]

-- | Create a 'TypedScriptTxIn' from a correctly-typed validator, redeemer, and output ref.
makeTypedScriptTxIn ::
  forall inn.
  (ToData (RedeemerType inn), ToData (DatumType inn)) =>
  TypedValidator inn ->
  RedeemerType inn ->
  TypedScriptTxOutRef inn ->
  TypedScriptTxIn inn
makeTypedScriptTxIn si r tyRef@(TypedScriptTxOutRef ref TypedScriptTxOut {tyTxOutData = d}) =
  let vs = validatorScript si
      rs = Redeemer (toBuiltinData r)
      ds = Datum (toBuiltinData d)
      txInType = ConsumeScriptAddress vs rs ds
   in TypedScriptTxIn @inn (TxIn ref (Just txInType)) tyRef

txInValue :: TypedScriptTxIn a -> Value
txInValue = txOutValue . tyTxOutTxOut . tyTxOutRefOut . tyTxInOutRef

-- | A public-key 'TxIn'. We need this to be sure that it is not a script input.
newtype PubKeyTxIn = PubKeyTxIn {unPubKeyTxIn :: TxIn}
  deriving stock (Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Create a 'PubKeyTxIn'.
makePubKeyTxIn :: TxOutRef -> PubKeyTxIn
makePubKeyTxIn ref = PubKeyTxIn . TxIn ref . Just $ ConsumePublicKeyAddress

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (FromData (DatumType a), ToData (DatumType a)) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData  :: DatumType a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxOut a) where
  l == r =
    tyTxOutTxOut l == tyTxOutTxOut r
      && tyTxOutData l == tyTxOutData r

instance (FromJSON (DatumType a), FromData (DatumType a), ToData (DatumType a)) => FromJSON (TypedScriptTxOut a) where
  parseJSON (Data.Aeson.Object v) =
    TypedScriptTxOut <$> v .: "tyTxOutTxOut" <*> v .: "tyTxOutData"
  parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxOut a) where
  toJSON TypedScriptTxOut {tyTxOutTxOut, tyTxOutData} =
    object ["tyTxOutTxOut" .= tyTxOutTxOut, "tyTxOutData" .= tyTxOutData]

-- | Create a 'TypedScriptTxOut' from a correctly-typed data script, an address, and a value.
makeTypedScriptTxOut ::
  forall out.
  (ToData (DatumType out), FromData (DatumType out)) =>
  TypedValidator out ->
  DatumType out ->
  Value ->
  TypedScriptTxOut out
makeTypedScriptTxOut ct d value =
  TypedScriptTxOut @out
    TxOut
      { txOutAddress = validatorAddress ct,
        txOutValue = value,
        txOutDatumHash = Just (datumHash $ Datum $ toBuiltinData d)
      }
    d

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxOutRef a) where
  l == r =
    tyTxOutRefRef l == tyTxOutRefRef r
      && tyTxOutRefOut l == tyTxOutRefOut r

instance (FromJSON (DatumType a), FromData (DatumType a), ToData (DatumType a)) => FromJSON (TypedScriptTxOutRef a) where
  parseJSON (Data.Aeson.Object v) =
    TypedScriptTxOutRef <$> v .: "tyTxOutRefRef" <*> v .: "tyTxOutRefOut"
  parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxOutRef a) where
  toJSON TypedScriptTxOutRef {tyTxOutRefRef, tyTxOutRefOut} =
    object ["tyTxOutRefRef" .= tyTxOutRefRef, "tyTxOutRefOut" .= tyTxOutRefOut]

-- | A public-key 'TxOut'. We need this to be sure that it is not a script output.
newtype PubKeyTxOut = PubKeyTxOut {unPubKeyTxOut :: TxOut}
  deriving stock (Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Create a 'PubKeyTxIn' from an existing 'TxIn' by checking that it has the right payment type.
typePubKeyTxIn ::
  forall m.
  (MonadError ConnectionError m) =>
  TxIn ->
  m PubKeyTxIn
typePubKeyTxIn inn@TxIn {txInType} =
  case txInType of
    Just ConsumePublicKeyAddress -> pure $ PubKeyTxIn inn
    Just x                       -> throwError $ WrongInType x
    Nothing                      -> throwError MissingInType

-- | Create a 'PubKeyTxOUt' from an existing 'TxOut' by checking that it has the right payment type.
typePubKeyTxOut ::
  forall m.
  (MonadError ConnectionError m) =>
  TxOut ->
  m PubKeyTxOut
typePubKeyTxOut out@TxOut {txOutDatumHash} =
  case txOutDatumHash of
    Nothing -> pure $ PubKeyTxOut out
    Just _  -> throwError $ WrongOutType ExpectedPubkeyGotScript

-- | Create a 'TypedScriptTxIn' from an existing 'TxIn' by checking the types of its parts.
typeScriptTxIn ::
  forall inn m.
  ( FromData (RedeemerType inn),
    ToData (RedeemerType inn),
    FromData (DatumType inn),
    ToData (DatumType inn),
    MonadError ConnectionError m
  ) =>
  (TxOutRef -> Maybe (TxOut, Datum)) ->
  TypedValidator inn ->
  TxIn ->
  m (TypedScriptTxIn inn)
typeScriptTxIn _lookupRef _typedValidator (TxIn _tor Nothing) =
  throwError MissingInType
typeScriptTxIn lookupRef typedValidator (TxIn tor (Just tit)) =
  case tit of
    ConsumeScriptAddress _val re da -> do
      rsVal <- checkRedeemer typedValidator re
      _ <- checkDatum typedValidator da
      typedOut <- typeScriptTxOutRef @inn lookupRef typedValidator tor
      pure $ makeTypedScriptTxIn typedValidator rsVal typedOut
    _ -> throwError $ WrongInType tit

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  TypedValidator out ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOut@TxOut {txOutAddress, txOutDatumHash} datum = do
  case addressCredential txOutAddress of
    PubKeyCredential _ ->
      throwError $ WrongOutType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case txOutDatumHash of
        Just dh -> do
          unless (datumHash datum == dh) $
            error "wrong datum hash" -- FIXME
          checkValidatorAddress tv txOutAddress
          dsVal <- checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        Nothing -> error "no datum hash" -- FIXME

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts. To do this we
-- need to cross-reference against the validator script and be able to look up the 'TxOut' to which this
-- reference points.
typeScriptTxOutRef ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  (TxOutRef -> Maybe (TxOut, Datum)) ->
  TypedValidator out ->
  TxOutRef ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef lookupRef ct ref = do
  -- FIXME, this function feels silly to me
  case lookupRef ref of
    Just (txOut, datum) -> do
      tyOut <- typeScriptTxOut @out ct txOut datum
      pure $ TypedScriptTxOutRef ref tyOut
    Nothing ->
      throwError UnknownRef
