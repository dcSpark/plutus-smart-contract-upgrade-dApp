{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Endpoints where

import Control.Monad (void)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Ledger (AssetClass, CurrencySymbol, Datum (..), DatumHash (..), Passphrase, PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (unPaymentPubKey), PubKey, Signature, TokenName, Value, datumHash)
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Crypto qualified as Crypto
import Ledger.Value qualified as Value
import MaliciousEndpoints as MC
import Offchain as OC (MigrateSchema, endpoints)
import Onchain
  ( MigrationDatum,
    TreasuryConfig (TreasuryConfig),
    UpgradeConfig (UpgradeConfig),
    treasuryScriptHash,
  )
import Plutus.Contract (Contract)
import Plutus.Contract.Test
  ( Wallet,
    checkPredicate,
    endpointAvailable,
    mockWalletPaymentPubKey,
    w1,
    w2,
    w3,
    w4,
    w5,
    (.&&.),
  )
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx (ToData (toBuiltinData))
import Test.Tasty (TestTree, testGroup)
import Utils (fromJust)
import Wallet.Emulator.Wallet (walletToMockWallet)

currency :: CurrencySymbol
currency = Value.currencySymbol "Treasury"

token :: TokenName
token = Value.tokenName "MigrationControlToken"

migrationControlAsset :: AssetClass
migrationControlAsset = Value.assetClass currency token

migrationControlValue :: Value
migrationControlValue = Value.assetClassValue migrationControlAsset 1

aCurrency :: CurrencySymbol
aCurrency = Value.currencySymbol "SomeCurrency"

aToken :: TokenName
aToken = Value.tokenName "SomeToken"

anAsset :: AssetClass
anAsset = Value.assetClass aCurrency aToken

aTokenValue :: Value
aTokenValue = Value.assetClassValue anAsset 1

wallets :: [Wallet]
wallets = [w1, w2, w3]

pubKeys :: [PubKey]
pubKeys = map (unPaymentPubKey . mockWalletPaymentPubKey) wallets

privKeys :: [PaymentPrivateKey]
privKeys = map (paymentPrivateKey . fromJust . walletToMockWallet) wallets

passphrase :: Passphrase
passphrase = ""

signDatumHash :: Maybe MigrationDatum -> PaymentPrivateKey -> Signature
signDatumHash d privKey = Crypto.sign msgHash (unPaymentPrivateKey privKey) passphrase
  where
    datum' = Datum $ toBuiltinData d
    DatumHash msgHash = datumHash datum'

allSignDatumHash :: Maybe MigrationDatum -> [Signature]
allSignDatumHash d = map (signDatumHash d) privKeys

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

contractMalicious :: Contract () MigrateMaliciousSchema T.Text ()
contractMalicious = MC.endpoints (treasuryConfig, upgradeConfig)

endpointTests :: TestTree
endpointTests =
  let tag :: Trace.ContractInstanceTag
      tag = "instance 1"
   in testGroup
        "Simple endpoint tests"
        [ checkPredicate
            "Expose endpoints"
            ( endpointAvailable @"lock funds" contract tag
                .&&. endpointAvailable @"submit solution" contract tag
                .&&. endpointAvailable @"migrate contract" contract tag
                .&&. endpointAvailable @"init treasury" contract tag
            )
            $ void $ Trace.activateContractWallet w1 contract
        ]