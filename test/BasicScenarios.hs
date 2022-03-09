{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BasicScenarios where

--useCaseTests,

import Control.Lens
import Control.Monad hiding (fmap)
import Endpoints
import Ledger.Ada as Ada
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
-- import MaliciousEndpoints as ME
import Offchain
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Test.Tasty

-- import Wallet.Emulator.Wallet

-- import Prelude ((<>))

normalTests :: TestTree
normalTests =
  let options = defaultCheckOptions & emulatorConfig .~ emCfg
   in testGroup
        "Normal contract use"
        [ checkPredicateOptions
            options
            "Collect prize"
            (assertNoFailedTransactions .&&. walletFundsChange w7 (Ada.lovelaceValueOf 1_000_000_000))
            collectPrize,
          checkPredicateOptions
            options
            "Try prize with invalid response"
            (assertFailedTransaction (\_ err _ -> case err of ScriptFailure (EvaluationError ["", "PT5"] _) -> True; _ -> False))
            collectPrizeInvalid,
          checkPredicateOptions
            options
            "Migrate contract"
            (assertNoFailedTransactions .&&. walletFundsChange w7 (Ada.lovelaceValueOf 1_000_000_000))
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
  Trace.callEndpoint @"lock funds" h1 $ LockValueParams 16 $ Ada.lovelaceValueOf 1_000_000_000
  void $ Trace.waitNSlots 1

-- let
--   newValue' = Ada.lovelaceValueOf 1_000_000_000
--   newDatum' = ""
--   newScriptHash' =
--   migrationData = MigrateContractParams newScriptHash' newDatum' newValue' signatures
-- Trace.callEndpoint @"migrate contract" h2
-- TODO
-- build new token
-- sign it
--
