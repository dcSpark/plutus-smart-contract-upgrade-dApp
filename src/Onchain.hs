{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Onchain where

import Data.Aeson (FromJSON, ToJSON)
import Ledger
  ( Address (Address),
    AssetClass,
    Datum (Datum),
    DatumHash (DatumHash),
    PubKey (PubKey),
    ScriptContext (ScriptContext, scriptContextTxInfo),
    Signature (Signature),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoInputs),
    TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
    ValidatorHash (..),
    Value,
    findDatum,
    getContinuingOutputs,
    ownHashes,
    scriptAddress,
    scriptOutputsAt,
    validatorHash,
  )
import Ledger.Contexts qualified as Validation
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (..), mkTypedValidator, validatorScript, wrapValidator)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (Credential (ScriptCredential))
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import PlutusPrelude (Generic)
import PlutusTx qualified
import PlutusTx.Prelude
  ( Bool,
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe (Just, Nothing),
    MultiplicativeSemigroup ((*)),
    Ord ((>), (>=)),
    filter,
    fold,
    fst,
    head,
    length,
    map,
    mapMaybe,
    nub,
    sha2_256,
    traceError,
    traceIfFalse,
    verifySignature,
    ($),
    (&&),
    (.),
  )
import Prelude (Show)
import Prelude qualified

-- UTILITY FUNCTIONS

-- | Verify the signature on a signed hash
{-# INLINEABLE checkSignature #-}
checkSignature ::
  -- | The hash of the message
  BuiltinByteString ->
  -- | The public key of the signatory
  PubKey ->
  -- | The signed message
  Signature ->
  Bool
checkSignature bsHash (PubKey (LedgerBytes pk)) (Signature sig) =
  verifySignature pk bsHash sig

{-# INLINEABLE validateSignatures #-}
validateSignatures :: [PubKey] -> Integer -> [Signature] -> BuiltinByteString -> Bool
validateSignatures pubKeys minSignatures sigs msgHash =
  let uniquePubKeys = nub pubKeys
      uniqueSigs = nub sigs
      allComb = [(pk, sig) | pk <- uniquePubKeys, sig <- uniqueSigs]
      isValidSig (pk, sig) = checkSignature msgHash pk sig
      matches = length $ filter isValidSig allComb
   in matches >= minSignatures

data UpgradeConfig = UpgradeConfig
  { controlAsset :: !AssetClass,
    treasury :: !ValidatorHash
  }
  deriving (Generic, Show)

PlutusTx.makeLift ''UpgradeConfig
PlutusTx.makeIsDataIndexed ''UpgradeConfig [('UpgradeConfig, 0)]

data MigrationDatum = MigrationDatum
  { originalScriptHash :: ValidatorHash,
    newScriptHash :: ValidatorHash,
    newValue :: Value,
    newDatumHash :: DatumHash
  }
  deriving (Generic, Show, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''MigrationDatum
PlutusTx.makeIsDataIndexed ''MigrationDatum [('MigrationDatum, 0)]

-- ONCHAIN VALIDATORS

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Prelude.Eq, Prelude.Show, FromJSON, ToJSON)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Prelude.Eq)

PlutusTx.makeLift ''ClearString

data Game

instance ValidatorTypes Game where
  type RedeemerType Game = ClearString
  type DatumType Game = HashedString

gameInstance :: TypedValidator Game
gameInstance =
  mkTypedValidator @Game
    $$(PlutusTx.compile [||validateGuess||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @HashedString @ClearString

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

gameScriptHash :: ValidatorHash
gameScriptHash = validatorHash $ validatorScript gameInstance

gameScriptAddress :: Address
gameScriptAddress = Ledger.scriptAddress (validatorScript gameInstance)

data MathReward

instance ValidatorTypes MathReward where
  type RedeemerType MathReward = Integer
  type DatumType MathReward = Integer

mathRewardValidatorScript :: Integer -> Integer -> ScriptContext -> Bool
mathRewardValidatorScript datum redeemer _ = redeemer * redeemer == datum

-- Upgrade script

{-# INLINEABLE upgradeMathScript #-}
upgradeMathScript :: UpgradeConfig -> DatumType MathReward -> RedeemerType MathReward -> ScriptContext -> Bool
upgradeMathScript UpgradeConfig {controlAsset, treasury} datum redeemer ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  let controlTokenPayToTreasury = Value.assetClassValueOf (Validation.valueLockedBy txInfo treasury) controlAsset > 0
   in if controlTokenPayToTreasury
        then
          let --migration spending path
              noValueCurrentContract = Value.isZero $ Validation.valueLockedBy txInfo $ Validation.ownHash ctx
           in noValueCurrentContract
        else -- regular spending path, delegate to script
          mathRewardValidatorScript datum redeemer ctx

upgradeMathScriptInstance :: UpgradeConfig -> TypedValidator MathReward
upgradeMathScriptInstance config =
  mkTypedValidator
    ($$(PlutusTx.compile [||upgradeMathScript||]) `PlutusTx.applyCode` PlutusTx.liftCode config)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator

upgradeMathScriptHash :: UpgradeConfig -> ValidatorHash
upgradeMathScriptHash config = validatorHash $ validatorScript $ upgradeMathScriptInstance config

upgradeMathScriptAddress :: UpgradeConfig -> Address
upgradeMathScriptAddress config = Ledger.scriptAddress (validatorScript $ upgradeMathScriptInstance config)

-- Treasury script
data Treasury

instance ValidatorTypes Treasury where
  type RedeemerType Treasury = [Signature]
  type DatumType Treasury = MigrationDatum

data TreasuryConfig = TreasuryConfig
  { treasuryAsset :: !AssetClass,
    authorizedPubKeys :: ![PubKey],
    minSigRequired :: !Integer
  }
  deriving (Generic, Show)

PlutusTx.makeLift ''TreasuryConfig
PlutusTx.makeIsDataIndexed ''TreasuryConfig [('TreasuryConfig, 0)]

{-# INLINEABLE ownNextDatumHash #-}
ownNextDatumHash :: ScriptContext -> DatumHash
ownNextDatumHash ctx = case getContinuingOutputs ctx of
  [o] -> case txOutDatumHash o of
    Nothing -> traceError "wrong output type"
    Just h -> h
  _ -> traceError "expected exactly one continuing output"

-- {-# INLINEABLE valueSpentBy #-}
-- -- | Get the total value of inputs spent by this transaction.
-- valueSpentBy :: TxInfo -> ValidatorHash -> Value
-- valueSpentBy = foldMap (txOutValue . txInInfoResolved) . txInfoInputs

{-# INLINEABLE valueSpentBy #-}

-- | Get the list of 'TxOut' outputs of the pending transaction at
--   a given script address.
valueSpentBy :: TxInfo -> ValidatorHash -> Value
valueSpentBy p h =
  let flt TxOut {txOutAddress = Address (ScriptCredential s) _, txOutValue = v} | s == h = Just v
      flt _ = Nothing
   in fold $ mapMaybe (flt . txInInfoResolved) (txInfoInputs p)

{-# INLINEABLE treasuryScript #-}
treasuryScript :: TreasuryConfig -> MigrationDatum -> [Signature] -> ScriptContext -> Bool
treasuryScript TreasuryConfig {treasuryAsset, authorizedPubKeys, minSigRequired} _ sigs ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  let outTreasuryDatumHash = ownNextDatumHash ctx
      outputDatum = case findDatum outTreasuryDatumHash txInfo of
        Nothing -> traceError "datum not found"
        Just (Datum d) -> case PlutusTx.fromBuiltinData d of
          Just ad' -> ad'
          Nothing -> traceError "error decoding data"

      DatumHash msg = outTreasuryDatumHash
      requiredSignatures = validateSignatures authorizedPubKeys minSigRequired sigs msg
      MigrationDatum {originalScriptHash, newScriptHash, newValue, newDatumHash} = outputDatum
      ownValidatorHash = fst $ ownHashes ctx
      tokenPreserved = Value.assetClassValueOf (Validation.valueLockedBy txInfo ownValidatorHash) treasuryAsset > 0
      valueSigned = Validation.valueLockedBy txInfo newScriptHash == newValue
      valuePreserved = valueSpentBy txInfo originalScriptHash == Validation.valueLockedBy txInfo newScriptHash
      outDatumHash' = head $ nub $ map fst $ scriptOutputsAt newScriptHash txInfo
      datumSigned = newDatumHash == outDatumHash'
   in traceIfFalse "not enough signatures for minting" requiredSignatures
        && traceIfFalse "token not preserved" tokenPreserved
        && traceIfFalse "value in target migration script not signed" valueSigned
        && traceIfFalse "value not migrated" valuePreserved
        && traceIfFalse "Datum in new script not signed" datumSigned

treasuryScriptInstance :: TreasuryConfig -> TypedValidator Treasury
treasuryScriptInstance treasuryConfig =
  mkTypedValidator @Treasury
    ($$(PlutusTx.compile [||treasuryScript||]) `PlutusTx.applyCode` PlutusTx.liftCode treasuryConfig)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator

treasuryScriptHash :: TreasuryConfig -> ValidatorHash
treasuryScriptHash treasuryConfig = validatorHash $ validatorScript $ treasuryScriptInstance treasuryConfig

treasuryScriptAddress :: TreasuryConfig -> Address
treasuryScriptAddress treasuryConfig = Ledger.scriptAddress (validatorScript $ treasuryScriptInstance treasuryConfig)
