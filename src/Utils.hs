{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Control.Lens (review)
import Data.Text qualified as T
import Ledger
  ( ChainIndexTxOut
      ( PublicKeyChainIndexTxOut,
        ScriptChainIndexTxOut,
        _ciTxOutDatum
      ),
    CurrencySymbol,
    Datum (..),
    DatumHash,
    Redeemer (Redeemer),
    ScriptContext,
    TokenName,
    TxInInfo (txInInfoResolved),
    TxOut (txOutAddress, txOutDatumHash, txOutValue),
    ValidatorHash,
    Value,
    findOwnInput,
    toValidatorHash,
  )
import Ledger.Constraints
import Ledger.Value qualified as Value
import Plutus.Contract (AsContractError, Contract, datumFromHash, throwError)
import Plutus.Contract.Types (AsContractError (_ConstraintResolutionError))
import PlutusTx qualified
import PlutusTx.Prelude
  ( Applicative (pure),
    Either,
    Maybe (..),
    const,
    either,
    error,
    fromMaybe,
    maybe,
    ($),
    (.),
    (>>=),
  )

-- onchain

-- the standard Maybe.fromJust don't work onchain
{-# INLINEABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error ()

{-# INLINEABLE datumToData #-}
datumToData :: (PlutusTx.FromData a) => Datum -> Maybe a
datumToData datum = PlutusTx.fromBuiltinData $ getDatum datum

{-# INLINEABLE validatorHashOf #-}
validatorHashOf :: TxInInfo -> Maybe ValidatorHash
validatorHashOf = toValidatorHash . txOutAddress . txInInfoResolved

{-# INLINEABLE datumHashOf #-}
datumHashOf :: TxInInfo -> DatumHash
datumHashOf = fromJust . txOutDatumHash . txInInfoResolved

{-# INLINEABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINEABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

-- offchain

getDatumOrHash :: ChainIndexTxOut -> Either DatumHash Datum
getDatumOrHash PublicKeyChainIndexTxOut {} = throwError "no datum for a txout of a public key address"
getDatumOrHash ScriptChainIndexTxOut {_ciTxOutDatum} = _ciTxOutDatum

emptyRedeemer :: Redeemer
emptyRedeemer = Redeemer $ PlutusTx.toBuiltinData ()

getDatum' :: DatumHash -> Contract w s T.Text Datum
getDatum' dh =
  datumFromHash dh >>= \case
    Nothing -> throwError "datum not found"
    Just d -> pure d

extractData :: (PlutusTx.FromData a) => ChainIndexTxOut -> Contract w s T.Text a
extractData o = do
  (Datum e) <- either getDatum' pure $ getDatumOrHash o
  maybe
    (throwError "datum hash wrong type")
    pure
    (PlutusTx.fromBuiltinData e)

unsafeDatum :: (PlutusTx.FromData a) => ChainIndexTxOut -> Maybe a
unsafeDatum ciTxOut = either (const Nothing) datumToData $ getDatumOrHash ciTxOut

extractComponent :: Value -> CurrencySymbol -> TokenName -> Value
extractComponent value currency token = Value.singleton currency token $ Value.valueOf value currency token

mkMultiValidatorTx :: forall w s e. (AsContractError e) => [SomeLookupsAndConstraints] -> Contract w s e UnbalancedTx
mkMultiValidatorTx lookupsAndConstraints = either (throwError . review _ConstraintResolutionError) pure $ mkSomeTx lookupsAndConstraints
