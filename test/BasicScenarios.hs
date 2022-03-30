{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BasicScenarios where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Endpoints
  ( aTokenValue,
    allSignDatumHash,
    contract,
    contractMalicious,
    emCfg,
    privKeys,
    signDatumHash,
    upgradeConfig,
  )
import Ledger (Datum (..), datumHash)
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import Offchain
  ( LockValueParams (LockValueParams),
    MigrateContractParams (MigrateContractParams),
    hashString,
  )
import Onchain
  ( MigrationDatum (MigrationDatum),
    gameScriptAddress,
    gameScriptHash,
    upgradeMathScriptHash,
  )
import Plutus.Contract.Test
  ( assertFailedTransaction,
    assertNoFailedTransactions,
    checkPredicateOptions,
    defaultCheckOptions,
    emulatorConfig,
    valueAtAddress,
    w1,
    w2,
    walletFundsChange,
    (.&&.),
  )
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Prelude
import Test.Tasty (TestTree, testGroup)
import Prelude (head)

normalTests :: TestTree
normalTests =
  let options = defaultCheckOptions & emulatorConfig .~ emCfg
   in testGroup
        "Normal contract use"
        [ checkPredicateOptions
            options
            "Collect prize"
            (assertNoFailedTransactions .&&. walletFundsChange w2 (Ada.lovelaceValueOf 1_000_000_000))
            collectPrize,
          checkPredicateOptions
            options
            "Try prize with invalid response"
            (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["PT5"] _) -> True; _ -> False))
            collectPrizeInvalid,
          checkPredicateOptions
            options
            "Migrate contract"
            ( assertNoFailedTransactions
                .&&. valueAtAddress
                  gameScriptAddress
                  (Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue ==)
            )
            migrateCurrentContract,
          checkPredicateOptions
            options
            "Migrate contract without all signatures"
            (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["not enough valid signatures", "PT5"] _) -> True; _ -> False))
            migrateCurrentContractNotEnoughSigs,
          checkPredicateOptions
            options
            "Migrate contract token not preserved"
            (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["treasury token not preserved", "PT5"] _) -> True; _ -> False))
            migrateCurrentContractTokenNotPreserved          
        ]

collectPrize :: Trace.EmulatorTrace ()
collectPrize = do
  h1 <- Trace.activateContractWallet w1 contract
  h2 <- Trace.activateContractWallet w2 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"submit solution" h2 4
  void $ Trace.waitNSlots 1

collectPrizeInvalid :: Trace.EmulatorTrace ()
collectPrizeInvalid = do
  h1 <- Trace.activateContractWallet w1 contract
  h2 <- Trace.activateContractWallet w2 contract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"submit solution" h2 5
  void $ Trace.waitNSlots 1

migrateCurrentContract :: Trace.EmulatorTrace ()
migrateCurrentContract = do
  h1 <- Trace.activateContractWallet w1 contract
  Trace.callEndpoint @"init treasury" h1 ()
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
  void $ Trace.waitNSlots 1
  let newValue' = Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
      newDatum' = hashString "secret"
      currentScriptHash = upgradeMathScriptHash upgradeConfig
      newScriptHash' = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = Just $ MigrationDatum currentScriptHash newScriptHash' newValue' newDatumHash
      signatures = allSignDatumHash newMigrationDatum
      migrationData = MigrateContractParams newDatum' newValue' signatures
  Trace.callEndpoint @"migrate contract" h1 migrationData

--
migrateCurrentContractNotEnoughSigs :: Trace.EmulatorTrace ()
migrateCurrentContractNotEnoughSigs = do
  h1 <- Trace.activateContractWallet w1 contract
  h1e <- Trace.activateContractWallet w1 contractMalicious
  Trace.callEndpoint @"init treasury" h1 ()
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
  void $ Trace.waitNSlots 1
  let newValue' = Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
      newDatum' = hashString "secret"
      currentScriptHash = upgradeMathScriptHash upgradeConfig
      newScriptHash' = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      oneSig = Prelude.head privKeys
      newMigrationDatum = Just $ MigrationDatum currentScriptHash newScriptHash' newValue' newDatumHash
      migrationData = MigrateContractParams newDatum' newValue' [signDatumHash newMigrationDatum oneSig]
  Trace.callEndpoint @"migrate no sig check" h1e migrationData

migrateCurrentContractTokenNotPreserved :: Trace.EmulatorTrace ()
migrateCurrentContractTokenNotPreserved = do
  h1 <- Trace.activateContractWallet w1 contract
  h1e <- Trace.activateContractWallet w1 contractMalicious
  Trace.callEndpoint @"init treasury" h1 ()
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
  void $ Trace.waitNSlots 1
  let newValue' = Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
      newDatum' = hashString "secret"
      currentScriptHash = upgradeMathScriptHash upgradeConfig
      newScriptHash' = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = Just $ MigrationDatum currentScriptHash newScriptHash' newValue' newDatumHash
      migrationData = MigrateContractParams newDatum' newValue' $ allSignDatumHash newMigrationDatum
  Trace.callEndpoint @"migrate no token" h1e migrationData