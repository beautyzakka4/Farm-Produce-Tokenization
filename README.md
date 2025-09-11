# Farm Produce Tokenization with Weather Risk Assessment

## Project Overview

This project is a comprehensive Farm Produce Tokenization platform built on Stacks blockchain that enables farmers to tokenize their future harvest, allowing investors to purchase shares and manage weather-related risks. The system includes sophisticated weather risk assessment capabilities that help both farmers and investors make informed decisions about crop protection and insurance.

## Key Features

### Core Farm Produce Tokenization (`farm-produce.clar`)
- **Produce Registration**: Farmers can tokenize their expected harvest with details like crop type, quantity, harvest date, and location
- **Share Trading**: Investors can buy and sell shares in tokenized farm produce
- **Harvest Management**: Farmers can register actual harvest and investors can claim their proportional shares
- **Rating System**: Investors can rate produce quality after harvest
- **Insurance Pool**: Community-funded insurance system to protect against crop failures
- **Yield Prediction**: Farmers can submit yield predictions and track accuracy
- **Supply Chain Traceability**: Complete tracking from harvest to consumer delivery

### Weather Risk Assessment System (`weather-risk-assessment.clar`) - NEW FEATURE

This new addition provides advanced weather monitoring and risk management capabilities:

#### Weather Data Management
- **Weather Recording**: Authorized sources can record weather data including temperature, humidity, precipitation, and weather events
- **Location Risk Profiling**: Automatic calculation of risk levels based on weather patterns and historical events
- **Risk Score Calculation**: Advanced algorithm that factors in multiple weather variables

#### Alert and Notification System  
- **Alert Subscriptions**: Farmers and investors can subscribe to weather alerts for specific locations
- **Automated Alerts**: System automatically generates alerts when risk thresholds are exceeded
- **Token-Specific Alerts**: Weather alerts can be created for specific tokenized crops

#### Insurance Premium Adjustment
- **Dynamic Premium Calculation**: Insurance premiums are automatically adjusted based on current weather risk levels
- **Risk Multipliers**: Different risk levels (Low: 100%, Medium: 130%, High: 170%, Critical: 250%) affect insurance costs
- **Location-Based Pricing**: Premiums vary by geographic location based on historical weather patterns

#### Weather Risk Categories
- **Low Risk** (100% multiplier): Normal weather conditions
- **Medium Risk** (130% multiplier): Some concerning weather patterns
- **High Risk** (170% multiplier): Dangerous weather conditions likely
- **Critical Risk** (250% multiplier): Extreme weather events occurring

#### Weather Events Tracked
- Drought conditions
- Flooding
- Frost damage
- Hail storms
- Severe storms

## Recent Improvements

The weather risk assessment system was successfully added to enhance the original farm produce tokenization platform with:

1. **Scientific Risk Assessment**: Weather data provides objective, measurable risk metrics
2. **Automated Premium Adjustments**: Insurance costs automatically reflect current risk levels
3. **Early Warning System**: Proactive alerts help farmers and investors protect their interests
4. **Historical Risk Tracking**: Location-based risk profiles improve over time with more data
5. **Transparent Risk Pricing**: All stakeholders can see how weather affects insurance costs

## Technical Implementation

### Smart Contracts
1. **farm-produce.clar**: Main contract handling tokenization, trading, insurance, and supply chain
2. **weather-risk-assessment.clar**: Weather monitoring, risk assessment, and alert system

### Key Data Structures
- Weather records with location, temperature, humidity, precipitation, and risk scores
- Location risk profiles tracking historical weather patterns
- Alert subscriptions for farmers and investors
- Weather-adjusted insurance multipliers

### Testing
Comprehensive test suite covering:
- Weather data recording and authorization
- Risk level calculations and updates
- Alert subscription/unsubscription
- Insurance premium adjustments
- Weather risk summaries

## Use Cases

### For Farmers
- Tokenize future harvests to get upfront funding
- Subscribe to weather alerts to protect crops
- Get insurance coverage with weather-adjusted pricing
- Track yield prediction accuracy for better planning

### For Investors
- Invest in agricultural produce with risk assessment data
- Monitor weather conditions affecting their investments
- Receive alerts about potential crop impacts
- Purchase insurance coverage based on scientific risk models

### For Insurance Providers
- Dynamic premium calculation based on real weather data
- Automated risk assessment reduces manual underwriting
- Historical weather data improves actuarial modeling

##

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