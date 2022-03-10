{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Offchain where

import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
-- import Data.ByteString.Lazy.Char8 (ByteString)

import Data.ByteString.Char8 qualified as C
import Data.Semigroup
import Data.Text qualified as T
import GHC.Base (undefined)
import Ledger
  ( Datum (..),
    DatumHash (..),
    Signature (..),
    Value,
    datumHash,
  )
import Ledger.Constraints
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
  logInfo @Prelude.String "lock value"

submitsolution :: UpgradeConfig -> Integer -> Contract () MigrateSchema T.Text ()
submitsolution config solution = do
  contractUtxo <- utxosAt (upgradeMathScriptAddress config)
  let tx = collectFromScript contractUtxo solution
  void $ submitTxConstraintsSpending (upgradeMathScriptInstance config) contractUtxo tx
  logInfo @Prelude.String "solution sent"

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Prelude.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Prelude.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

data MigrateContractParams = MigrateContractParams
  { newDatum :: DatumType Game,
    newValue :: Value,
    signatures :: [Signature]
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

migrateContract :: TreasuryConfig -> UpgradeConfig -> MigrateContractParams -> Contract () MigrateSchema T.Text ()
migrateContract treasuryCfg upgradeCfg (MigrateContractParams newDatum' newValue' signatures) =
  let TreasuryConfig {treasuryAsset, authorizedPubKeys, minSigRequired} = treasuryCfg
      treasuryTokenValue = Value.assetClassValue treasuryAsset 1
      currentScriptHash = upgradeMathScriptHash upgradeCfg
      newScriptHash = gameScriptHash
      newDatumHash = datumHash $ Datum $ toBuiltinData newDatum'
      newMigrationDatum = MigrationDatum currentScriptHash newScriptHash newValue' newDatumHash
      DatumHash migrationDatumHash = datumHash $ Datum $ toBuiltinData newMigrationDatum
      migrationSigned = validateSignatures authorizedPubKeys minSigRequired signatures migrationDatumHash
   in if not migrationSigned
        then logError @Prelude.String "Migration not signed"
        else do
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
