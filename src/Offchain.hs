{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Offchain where

import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
-- import Data.ByteString.Lazy.Char8 (ByteString)

import Data.Semigroup
import Data.Text qualified as T
import GHC.Base (undefined)
import Ledger
  ( Datum (..),
    DatumHash (..),
    Passphrase,
    PaymentPrivateKey,
    -- PrivateKey,
    Signature (..),
    ValidatorHash,
    Value,
    datumHash,
    unPaymentPrivateKey,
  )
import Ledger.Constraints
import Ledger.Crypto qualified as Crypto
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value qualified as Value
import Onchain
import Plutus.Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusPrelude (Generic)
import PlutusTx.Prelude hiding ((<>))
import Utils
import Prelude qualified

---------------------------------------------
-- lock value in the contract
data LockValueParams = LockValueParams
  { target :: Integer,
    prize :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

lockFunds :: UpgradeConfig -> LockValueParams -> Contract () MigrateSchema T.Text ()
lockFunds config LockValueParams {target, prize} = do
  let tx = mustPayToTheScript target prize
      lookup = typedValidatorLookups (upgradeMathScriptInstance config)
  void $ mkTxConstraints @MathReward lookup tx >>= submitTxConfirmed . adjustUnbalancedTx

submitsolution :: UpgradeConfig -> Integer -> Contract () MigrateSchema T.Text ()
submitsolution config solution = do
  contractUtxo <- utxosAt (upgradeMathScriptAddress config)
  let tx = collectFromScript contractUtxo solution
      lookup = typedValidatorLookups (upgradeMathScriptInstance config)
  void $ mkTxConstraints @MathReward lookup tx >>= submitTxConfirmed . adjustUnbalancedTx

data MigrateContractParams = MigrateContractParams
  { newScriptHash :: ValidatorHash,
    newDatum :: DatumType Game,
    newValue :: Value,
    signatures :: [Signature]
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

signDatumHash :: MigrationDatum -> PaymentPrivateKey -> Passphrase -> Signature
signDatumHash migrationDatum privKey = Crypto.sign msgHash (unPaymentPrivateKey privKey)
  where
    datum' = Datum $ toBuiltinData migrationDatum
    DatumHash msgHash = datumHash datum'

migrateContract :: TreasuryConfig -> UpgradeConfig -> MigrateContractParams -> Contract () MigrateSchema T.Text ()
migrateContract treasuryCfg upgradeCfg (MigrateContractParams newScriptHash' newDatum' newValue' signatures) =
  let TreasuryConfig {treasuryAsset, authorizedPubKeys, minSigRequired} = treasuryCfg
      treasuryTokenValue = Value.assetClassValue treasuryAsset 1
      currentScriptHash = upgradeMathScriptHash upgradeCfg
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = MigrationDatum currentScriptHash newScriptHash' newValue' newDatumHash
      DatumHash migrationDatumHash = datumHash $ Datum $ toBuiltinData newMigrationDatum
      migrationSigned = validateSignatures authorizedPubKeys minSigRequired signatures migrationDatumHash
   in if not migrationSigned
        then logError @Prelude.String "Migration not signed"
        else do
          -- ensure new datum is signed
          -- lookup treasury utxo with token
          -- lookup current contract utxo(s) with content
          -- collect treasury and pay token to treasury
          -- collect contract and pay to new address with new datum
          treasuryUtxo <- utxosAt (treasuryScriptAddress treasuryCfg)
          contractUtxo <- utxosAt (upgradeMathScriptAddress upgradeCfg)
          let currentValueInContract = collectFromScript contractUtxo undefined

              collectTreasuryToken = collectFromScript treasuryUtxo signatures
              returnTreasuryToken = mustPayToTheScript newMigrationDatum treasuryTokenValue
              migrateContractReq = mustPayToTheScript newDatum' newValue'

              treasuryLookups = typedValidatorLookups (treasuryScriptInstance treasuryCfg)
              migrationLookups = typedValidatorLookups gameInstance
              treasuryConstraint = collectTreasuryToken <> returnTreasuryToken
              migrationConstraint = migrateContractReq <> currentValueInContract
              treasurySpend = SomeLookupsAndConstraints treasuryLookups treasuryConstraint
              migrationSpend = SomeLookupsAndConstraints migrationLookups migrationConstraint
          void $ mkMultiValidatorTx [treasurySpend, migrationSpend] >>= submitTxConfirmed . adjustUnbalancedTx

type MigrateSchema =
  Endpoint "lock funds" LockValueParams
    .\/ Endpoint "submit solution" Integer
    .\/ Endpoint "migrate contract" MigrateContractParams

endpoints :: (TreasuryConfig, UpgradeConfig) -> Contract () MigrateSchema T.Text ()
endpoints (treasuryCfg, ugpradeCfg) =
  forever $
    handleError logError $
      awaitPromise $ lockFunds' `select` useContract' `select` migrateContract'
  where
    lockFunds' = endpoint @"lock funds" $ lockFunds ugpradeCfg
    useContract' = endpoint @"submit solution" $ submitsolution ugpradeCfg
    migrateContract' = endpoint @"migrate contract" $ migrateContract treasuryCfg ugpradeCfg
