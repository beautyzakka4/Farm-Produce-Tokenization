# Farm Produce Tokenization

This smart contract enables farmers to tokenize their future harvests, allowing investors to purchase shares of produce before it's harvested. This creates a marketplace for agricultural futures on the Stacks blockchain.

## Features

- Farmers can register their upcoming produce with details like type, quantity, and expected harvest date
- Investors can buy shares of future harvests
- Farmers can register when a harvest is complete
- Investors can claim their portion of the harvest based on their share ownership
- Shares can be transferred between users

## Contract Functions

### For Farmers

#### Register Produce
```
(contract-call? .farm-produce register-produce "Corn" u1000 u1672531200 u10 "Farm Location")
```
Parameters:
- Produce type (string-ascii, max 20 chars)
- Quantity (uint)
- Expected harvest date (uint, Unix timestamp)
- Price per unit in STX (uint)
- Location (string-ascii, max 50 chars)

Returns the token ID of the registered produce.

#### Register Harvest
```
(contract-call? .farm-produce register-harvest u1)
```
Parameters:
- Token ID (uint)

Marks a produce as harvested, allowing investors to claim their shares. Can only be called by the farmer who registered the produce and only after the expected harvest date.

### For Investors

#### Buy Shares
```
(contract-call? .farm-produce buy-shares u1 u100)
```
Parameters:
- Token ID (uint)
- Number of shares to buy (uint)

Transfers STX to the farmer based on the price per unit and number of shares.

#### Claim Harvest Share
```
(contract-call? .farm-produce claim-harvest-share u1)
```
Parameters:
- Harvest ID (uint)

Claims the investor's share of a harvested produce.

#### Transfer Shares
```
(contract-call? .farm-produce transfer-shares u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u50)
```
Parameters:
- Token ID (uint)
- Recipient (principal)
- Number of shares to transfer (uint)

Transfers ownership of shares from the caller to the recipient.

### Read-Only Functions

- `get-last-token-id`: Returns the ID of the last registered produce
- `get-last-harvest-id`: Returns the ID of the last registered harvest
- `get-produce-details`: Returns details about a specific produce
- `get-harvest-details`: Returns details about a specific harvest
- `get-token-owner`: Returns the owner of a produce token
- `get-investor-shares`: Returns the number of shares an investor owns for a specific produce

## Example Usage Flow

1. Farmer registers a future corn harvest
2. Investors buy shares of the corn harvest
3. When harvest time arrives, the farmer registers the harvest
4. Investors claim their portion of the harvest based on their share ownership

This contract creates a decentralized marketplace for agricultural futures, providing farmers with early capital and investors with access to agricultural investments.