# plutus-smart-contract-upgrade-dApp
Smart contract with controlled upgrade

<h1 align="center">
  Plutus voting dApp
</h1>
<p align="center">Basic quorum treasury control dApp.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.

## Protocol

This contract allows to pay the content of a treasury to the an address that gets enough votes.
A special designated token represents the vote. Depening on the intial voting token distribution, allows different voting power per wallet.

Protocol steps:

0. Prerequisites:
   - The contract to be migrated must include the migration code and be parametrized with the treasury token.
   - The treasury must be parametrized with the authorized public signatures and the required minimum numer of them.

1. Build the new migration datum with:

   - the current contract validator hash
   - the validator hash of the contract to migrate to
   - the new datum hash
   - the value to move to the new contract (should be the value in the current contract)

2. Gather signatures
   - Approved signers must sign the migration datum hash (from step 1)

3. Migrate the contract
   - To allow migration, the origin contract requires the treasury token to be spent and pay to the treasury on the same transaction
   - The treasury will allow spending the treasury token if the origin script, the target script, the new datum at the target script, and the value at the target script matches the signed datum hash.

## Building

To build the project execute `cabal build` at the project root.

To build:

``` bash
$ nix-shell
...
$ cabal build
...
```

## Testing

To run use-case test execute the following commands at the project root.

``` bash
$ cabal test
$ cabal test
Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - contract-upgrade-0.1.0.0 (test:contract-upgrade-test) (first run)
Preprocessing test suite 'contract-upgrade-test' for contract-upgrade-0.1.0.0..
Building test suite 'contract-upgrade-test' for contract-upgrade-0.1.0.0..
Running 1 test suites...
Test suite contract-upgrade-test: RUNNING...
use cases
  Simple endpoint tests
    Expose endpoints:                OK
  Normal contract use
    Collect prize:                   OK (0.08s)
    Try prize with invalid response: OK (0.04s)
    Migrate contract:                OK (0.11s)

All 4 tests passed (0.24s)
Test suite contract-upgrade-test: PASS
1 of 1 test suites (1 of 1 test cases) passed.

```
