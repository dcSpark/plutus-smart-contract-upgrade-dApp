{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BasicScenarios where

import Control.Lens
import Control.Monad hiding (fmap)
import Endpoints
import Ledger (Datum (..), datumHash)
import Ledger.Ada as Ada
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import Offchain
import Onchain
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusTx.Prelude ((<>))
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Test.Tasty

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
            migrateCurrentContract
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
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
  void $ Trace.waitNSlots 1
  let newValue' = Ada.lovelaceValueOf 1_000_000_000 <> aTokenValue
      newDatum' = hashString "secret"
      currentScriptHash = upgradeMathScriptHash upgradeConfig
      newScriptHash' = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = MigrationDatum currentScriptHash newScriptHash' newValue' newDatumHash
      migrationData = MigrateContractParams newDatum' newValue' $ signDatumHash newMigrationDatum
  Trace.callEndpoint @"migrate contract" h1 migrationData

--
