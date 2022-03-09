{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Endpoints
  ( endpointTests,
    emCfg,
    contract,
  )
where

-- import Control.Lens
import Control.Monad hiding (fmap)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Ledger (AssetClass, CurrencySymbol, PaymentPubKey (unPaymentPubKey), PubKey, TokenName, Value)
import Ledger.Ada as Ada
import Ledger.Value qualified as Value
import Offchain as OC
import Onchain
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import Test.Tasty

-- import Wallet.Emulator.Wallet (mockWalletAddress)

currency :: CurrencySymbol
currency = "Treasury"

token :: TokenName
token = "MigrationControlToken"

migrationControlAsset :: AssetClass
migrationControlAsset = Value.assetClass currency token

migrationControlValue :: Value
migrationControlValue = Value.assetClassValue migrationControlAsset 1

aTokenValue :: Value
aTokenValue = Value.singleton "SomeCurrency" "SomeToken" 1

pubKeys :: [PubKey]
pubKeys = map (unPaymentPubKey . mockWalletPaymentPubKey) [w1, w2, w3]

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left $ Map.fromList ((w1, v <> migrationControlValue) : [(w, v) | w <- [w2, w3, w4, w5]])) def def
  where
    v = Ada.lovelaceValueOf 1_000_000_000_000 <> aTokenValue

treasuryConfig :: TreasuryConfig
treasuryConfig = TreasuryConfig migrationControlAsset pubKeys 2

upgradeConfig :: UpgradeConfig
upgradeConfig = UpgradeConfig migrationControlAsset (treasuryScriptHash treasuryConfig)

contract :: Contract () MigrateSchema T.Text ()
contract = OC.endpoints (treasuryConfig, upgradeConfig)

endpointTests :: TestTree
endpointTests =
  let -- options = defaultCheckOptions & emulatorConfig .~ emCfg
      tag :: Trace.ContractInstanceTag
      tag = "instance 1"
   in testGroup
        "Simple endpoint tests"
        [ checkPredicate
            "Expose endpoints"
            ( endpointAvailable @"lock funds" contract tag
                .&&. endpointAvailable @"submit solution" contract tag
                .&&. endpointAvailable @"migrate contract" contract tag
            )
            $ void $ Trace.activateContractWallet w1 contract --,
            -- checkPredicateOptions options "Build treasury" assertNoFailedTransactions buildTreasuryTrace,
            -- checkPredicateOptions options "Single vote" assertNoFailedTransactions singleVoteTrace,
            -- checkPredicateOptions options "Return vote" assertNoFailedTransactions returnVoteTrace,
            -- checkPredicateOptions options "Tally votes" assertNoFailedTransactions tallyVotesTrace
        ]

-- buildTreasuryTrace :: Trace.EmulatorTrace ()
-- buildTreasuryTrace = do
--   h1 <- Trace.activateContractWallet w1 $ contract
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
--   void $ Trace.waitNSlots 1

-- singleVoteTrace :: Trace.EmulatorTrace ()
-- singleVoteTrace = do
--   h1 <- Trace.activateContractWallet w1 $ contract
--   h2 <- Trace.activateContractWallet w2 $ contract
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
--   void $ Trace.waitNSlots 1
--   let voteParam = VoteAddressParams (mockWalletAddress w5) 1
--   Trace.callEndpoint @"2-vote address" h2 voteParam

-- returnVoteTrace :: Trace.EmulatorTrace ()
-- returnVoteTrace = do
--   h1 <- Trace.activateContractWallet w1 $ contract
--   h2 <- Trace.activateContractWallet w2 $ contract
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
--   void $ Trace.waitNSlots 1
--   let voteParam = VoteAddressParams (mockWalletAddress w5) 1
--   Trace.callEndpoint @"2-vote address" h2 voteParam
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"4-return vote" h2 ()
--   void $ Trace.waitNSlots 1

-- tallyVotesTrace :: Trace.EmulatorTrace ()
-- tallyVotesTrace = do
--   h1 <- Trace.activateContractWallet w1 $ contract
--   h2 <- Trace.activateContractWallet w2 $ contract
--   h3 <- Trace.activateContractWallet w3 $ contract
--   h4 <- Trace.activateContractWallet w4 $ contract
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"1-setup treasury" h1 $ Ada.lovelaceValueOf 1_000_000_000 <> specialTokenValue
--   void $ Trace.waitNSlots 1
--   let voteParam = VoteAddressParams (mockWalletAddress w5) 1
--   Trace.callEndpoint @"2-vote address" h2 voteParam
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"2-vote address" h3 voteParam
--   void $ Trace.waitNSlots 1
--   Trace.callEndpoint @"3-collect" h4 ()
--   void $ Trace.waitNSlots 1
