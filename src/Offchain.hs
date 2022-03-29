{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Offchain where

import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import Data.Semigroup (Semigroup ((<>)))
import Data.Text qualified as T
import Ledger
  ( Datum (..),
    Signature (..),
    Value,
    datumHash,
  )
import Ledger.Constraints
  ( SomeLookupsAndConstraints (SomeLookupsAndConstraints),
    adjustUnbalancedTx,
    mustPayToOtherScript,
    mustPayToTheScript,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Typed.Scripts (ValidatorTypes (..), validatorScript)
import Ledger.Value qualified as Value
import Onchain
  ( ClearString (..),
    Game,
    HashedString (..),
    MathReward,
    MigrationDatum (MigrationDatum),
    Treasury,
    TreasuryConfig (..),
    UpgradeConfig,
    gameInstance,
    gameScriptHash,
    treasuryScriptAddress,
    treasuryScriptInstance,
    upgradeMathScriptAddress,
    upgradeMathScriptHash,
    upgradeMathScriptInstance,
    validateSignatures,
  )
import Plutus.Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    collectFromScript,
    endpoint,
    handleError,
    logError,
    logInfo,
    mkTxConstraints,
    select,
    submitTxConfirmed,
    submitTxConstraintsSpending,
    utxosAt,
    type (.\/),
  )
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import PlutusPrelude (Generic)
import PlutusTx.Prelude
  ( Integer,
    Maybe (Just, Nothing),
    not,
    sha2_256,
    toBuiltin,
    ($),
    (.),
    (>>=),
  )
import Utils (mkMultiValidatorTx)
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

initTreasury :: TreasuryConfig -> Contract () MigrateSchema T.Text ()
initTreasury config@TreasuryConfig {treasuryAsset} = do
  let treasuryValue = Value.assetClassValue treasuryAsset 1
      tx = mustPayToTheScript Nothing treasuryValue
      lookup = typedValidatorLookups (treasuryScriptInstance config)
  void $ mkTxConstraints @Treasury lookup tx >>= submitTxConfirmed . adjustUnbalancedTx
  logInfo @Prelude.String "lock value"

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

type MigrateSchema =
  Endpoint "lock funds" LockValueParams
    .\/ Endpoint "submit solution" Integer
    .\/ Endpoint "migrate contract" MigrateContractParams
    .\/ Endpoint "init treasury" ()

endpoints :: (TreasuryConfig, UpgradeConfig) -> Contract () MigrateSchema T.Text ()
endpoints (treasuryCfg, ugpradeCfg) =
  forever $
    handleError logError $
      awaitPromise $ lockFunds' `select` useContract' `select` migrateContract' `select` initTreasury'
  where
    lockFunds' = endpoint @"lock funds" $ lockFunds ugpradeCfg
    useContract' = endpoint @"submit solution" $ submitsolution ugpradeCfg
    migrateContract' = endpoint @"migrate contract" $ migrateContract treasuryCfg ugpradeCfg
    initTreasury' = endpoint @"init treasury" $ \() -> initTreasury treasuryCfg
