{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MaliciousEndpoints where

import Control.Monad (forever, void)
import Data.Text qualified as T
import Ledger (Datum (Datum), datumHash)
import Ledger.Constraints
import Ledger.Typed.Scripts
import Ledger.Value qualified as Value
import Offchain (MigrateContractParams (MigrateContractParams))
import Onchain
import Plutus.Contract
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx (toBuiltinData)
import PlutusTx.Monoid
import Utils
import Prelude

-- | Doesn't check the signatures
maliciousMigrateNoSigs :: TreasuryConfig -> UpgradeConfig -> MigrateContractParams -> Contract () MigrateMaliciousSchema T.Text ()
maliciousMigrateNoSigs treasuryCfg upgradeCfg (MigrateContractParams newDatum' newValue' signatures) = do
  let TreasuryConfig {treasuryAsset} = treasuryCfg
      treasuryTokenValue = Value.assetClassValue treasuryAsset 1
      currentScriptHash = upgradeMathScriptHash upgradeCfg
      newScriptHash = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = MigrationDatum currentScriptHash newScriptHash newValue' newDatumHash
  treasuryUtxo <- utxosAt (treasuryScriptAddress treasuryCfg)
  contractUtxo <- utxosAt (upgradeMathScriptAddress upgradeCfg)
  let currentValueInContract = collectFromScript contractUtxo 0

      collectTreasuryToken = collectFromScript treasuryUtxo signatures
      returnTreasuryToken = mustPayToTheScript (Just newMigrationDatum) treasuryTokenValue
      migrateContractReq = mustPayToOtherScript gameScriptHash (Datum $ toBuiltinData newDatum') newValue'

      treasuryLookups = typedValidatorLookups (treasuryScriptInstance treasuryCfg) <> unspentOutputs treasuryUtxo
      migrationLookups = otherScript (validatorScript gameInstance) <> unspentOutputs contractUtxo <> typedValidatorLookups (upgradeMathScriptInstance upgradeCfg)
      treasuryConstraint = collectTreasuryToken <> returnTreasuryToken
      migrationConstraint = migrateContractReq <> currentValueInContract
      treasurySpend = SomeLookupsAndConstraints treasuryLookups treasuryConstraint
      migrationSpend = SomeLookupsAndConstraints migrationLookups migrationConstraint
  void $ mkMultiValidatorTx [treasurySpend, migrationSpend] >>= submitTxConfirmed . adjustUnbalancedTx

-- | doesn't return the treasury token
maliciousMigrateNoToken :: TreasuryConfig -> UpgradeConfig -> MigrateContractParams -> Contract () MigrateMaliciousSchema T.Text ()
maliciousMigrateNoToken treasuryCfg upgradeCfg (MigrateContractParams newDatum' newValue' signatures) =
  let TreasuryConfig {treasuryAsset, authorizedPubKeys, minSigRequired} = treasuryCfg
      treasuryTokenValue = Value.assetClassValue treasuryAsset 0
      currentScriptHash = upgradeMathScriptHash upgradeCfg
      newScriptHash = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = Just $ MigrationDatum currentScriptHash newScriptHash newValue' newDatumHash
      migrationDatumHash = datumHash $ Datum $ toBuiltinData newMigrationDatum
      migrationSigned = validateSignatures authorizedPubKeys minSigRequired signatures migrationDatumHash
   in if not migrationSigned
        then logError @Prelude.String "Migration not signed"
        else do
          treasuryUtxo <- utxosAt (treasuryScriptAddress treasuryCfg)
          contractUtxo <- utxosAt (upgradeMathScriptAddress upgradeCfg)
          let currentValueInContract = collectFromScript contractUtxo 0

              collectTreasuryToken = collectFromScript treasuryUtxo signatures
              returnTreasuryToken = mustPayToTheScript newMigrationDatum treasuryTokenValue
              migrateContractReq = mustPayToOtherScript gameScriptHash (Datum $ toBuiltinData newDatum') newValue'

              treasuryLookups = typedValidatorLookups (treasuryScriptInstance treasuryCfg) <> unspentOutputs treasuryUtxo
              migrationLookups = otherScript (validatorScript gameInstance) <> unspentOutputs contractUtxo <> typedValidatorLookups (upgradeMathScriptInstance upgradeCfg)
              treasuryConstraint = collectTreasuryToken <> returnTreasuryToken
              migrationConstraint = migrateContractReq <> currentValueInContract
              treasurySpend = SomeLookupsAndConstraints treasuryLookups treasuryConstraint
              migrationSpend = SomeLookupsAndConstraints migrationLookups migrationConstraint
          void $ mkMultiValidatorTx [treasurySpend, migrationSpend] >>= submitTxConfirmed . adjustUnbalancedTx

-- | move a value not signed
maliciousMigrateDifferentValue :: TreasuryConfig -> UpgradeConfig -> MigrateContractParams -> Contract () MigrateMaliciousSchema T.Text ()
maliciousMigrateDifferentValue treasuryCfg upgradeCfg (MigrateContractParams newDatum' newValue' signatures) = do
  let TreasuryConfig {treasuryAsset} = treasuryCfg
      treasuryTokenValue = Value.assetClassValue treasuryAsset 1
      currentScriptHash = upgradeMathScriptHash upgradeCfg
      newScriptHash = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = MigrationDatum currentScriptHash newScriptHash newValue' newDatumHash
  treasuryUtxo <- utxosAt (treasuryScriptAddress treasuryCfg)
  contractUtxo <- utxosAt (upgradeMathScriptAddress upgradeCfg)
  let currentValueInContract = collectFromScript contractUtxo 0

      collectTreasuryToken = collectFromScript treasuryUtxo signatures
      returnTreasuryToken = mustPayToTheScript (Just newMigrationDatum) treasuryTokenValue
      migrateContractReq = mustPayToOtherScript gameScriptHash (Datum $ toBuiltinData newDatum') (newValue' <> inv (Ada.lovelaceValueOf 11110))

      treasuryLookups = typedValidatorLookups (treasuryScriptInstance treasuryCfg) <> unspentOutputs treasuryUtxo
      migrationLookups = otherScript (validatorScript gameInstance) <> unspentOutputs contractUtxo <> typedValidatorLookups (upgradeMathScriptInstance upgradeCfg)
      treasuryConstraint = collectTreasuryToken <> returnTreasuryToken
      migrationConstraint = migrateContractReq <> currentValueInContract
      treasurySpend = SomeLookupsAndConstraints treasuryLookups treasuryConstraint
      migrationSpend = SomeLookupsAndConstraints migrationLookups migrationConstraint
  void $ mkMultiValidatorTx [treasurySpend, migrationSpend] >>= submitTxConfirmed . adjustUnbalancedTx

type MigrateMaliciousSchema =
  Endpoint "migrate no sigs" MigrateContractParams
    .\/ Endpoint "migrate no token" MigrateContractParams
    .\/ Endpoint "migrate diff value" MigrateContractParams

endpoints :: (TreasuryConfig, UpgradeConfig) -> Contract () MigrateMaliciousSchema T.Text ()
endpoints (treasuryCfg, ugpradeCfg) =
  forever $
    handleError logError $ awaitPromise $ maliciousMigrateNoSigs' `select` maliciousMigrateNoToken' `select` maliciousMigrateDifferentValue'
  where
    maliciousMigrateNoSigs' = endpoint @"migrate no sigs" $ maliciousMigrateNoSigs treasuryCfg ugpradeCfg
    maliciousMigrateNoToken' = endpoint @"migrate no token" $ maliciousMigrateNoToken treasuryCfg ugpradeCfg
    maliciousMigrateDifferentValue' = endpoint @"migrate diff value" $ maliciousMigrateDifferentValue treasuryCfg ugpradeCfg