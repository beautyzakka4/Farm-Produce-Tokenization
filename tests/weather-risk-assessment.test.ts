import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;

describe("Weather Risk Assessment Tests", () => {
  it("allows contract owner to record weather data", () => {
    const { result } = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('Iowa'),
        Cl.uint(85), // temperature-high 
        Cl.uint(65), // humidity
        Cl.uint(50), // precipitation
        Cl.uint(0)   // weather-event (no event)
      ],
      deployer
    );
    
    expect(result).toBeOk(Cl.uint(1)); // returns record-id 1
  });

  it("prevents non-owner from recording weather data", () => {
    const { result } = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('Iowa'),
        Cl.uint(85), // temperature-high 
        Cl.uint(65), // humidity
        Cl.uint(50), // precipitation
        Cl.uint(0)   // weather-event
      ],
      wallet1
    );
    
    expect(result).toBeErr(Cl.uint(403)); // err-unauthorized
  });

  it("allows subscription and unsubscription from weather alerts", () => {
    // Subscribe to weather alerts
    const subscribeResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "subscribe-to-weather-alerts",
      [
        Cl.stringAscii('Iowa'),
        Cl.uint(70) // notification threshold
      ],
      wallet1
    );
    
    expect(subscribeResult.result).toBeOk(Cl.bool(true));
    
    // Check subscription status
    const subscriptionCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-alert-subscription',
      [Cl.principal(wallet1), Cl.stringAscii('Iowa')],
      wallet1
    );
    
    expect(subscriptionCall.result).toBeTuple({
      subscribed: Cl.bool(true),
      "notification-threshold": Cl.uint(70)
    });
    
    // Unsubscribe
    const unsubscribeResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "unsubscribe-from-weather-alerts",
      [Cl.stringAscii('Iowa')],
      wallet1
    );
    
    expect(unsubscribeResult.result).toBeOk(Cl.bool(true));
  });

  it.skip("updates location risk profile when weather data is recorded", () => {
    // Record weather data with drought event
    const weatherResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('California'),
        Cl.uint(110), // high temperature
        Cl.uint(20),  // low humidity
        Cl.uint(0),   // no precipitation
        Cl.uint(1)    // drought event
      ],
      deployer
    );
    
    expect(weatherResult.result).toBeOk(Cl.uint(1));
    
    // Check location risk profile
    const profileCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-location-risk-profile',
      [Cl.stringAscii('California')],
      deployer
    );
    
    // Verify the profile contains expected data
    // Verify the profile contains expected data by checking individual fields
    const profileData = profileCall.result.expectTuple();
    expect(profileData['drought-frequency']).toBeUint(1);
    expect(profileData['total-weather-events']).toBeUint(1);
    expect(profileData['current-risk-level']).toBeUint(4);
    expect(profileData['flood-frequency']).toBeUint(0);
    expect(profileData['avg-temperature']).toBeUint(75);
    expect(profileData['last-updated']).toBeUint(); // Accept any timestamp
  });

  it("has correctly initialized weather risk multipliers", () => {
    // Check risk multipliers for different levels
    const lowRiskCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-risk-multiplier',
      [Cl.uint(1)], // risk-low
      deployer
    );
    
    const mediumRiskCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-risk-multiplier',
      [Cl.uint(2)], // risk-medium
      deployer
    );
    
    const highRiskCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-risk-multiplier',
      [Cl.uint(3)], // risk-high
      deployer
    );
    
    const criticalRiskCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-risk-multiplier',
      [Cl.uint(4)], // risk-critical
      deployer
    );
    
    expect(lowRiskCall.result).toBeTuple({ multiplier: Cl.uint(100) });
    expect(mediumRiskCall.result).toBeTuple({ multiplier: Cl.uint(130) });
    expect(highRiskCall.result).toBeTuple({ multiplier: Cl.uint(170) });
    expect(criticalRiskCall.result).toBeTuple({ multiplier: Cl.uint(250) });
  });

  it("calculates weather-adjusted premium correctly", () => {
    // First record high-risk weather data for a location
    const weatherResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('Texas'),
        Cl.uint(108), // very high temperature
        Cl.uint(15),  // very low humidity
        Cl.uint(0),   // no precipitation
        Cl.uint(2)    // flood event
      ],
      deployer
    );
    
    expect(weatherResult.result).toBeOk(Cl.uint(1));
    
    // Calculate weather-adjusted premium for base premium of 1000
    const premiumCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-weather-adjusted-premium',
      [Cl.uint(1000), Cl.stringAscii('Texas')],
      deployer
    );
    
    // Should be adjusted based on risk level (critical = 250% multiplier)
    expect(premiumCall.result).toBeUint(2500); // 1000 * 250 / 100 = 2500
  });

  it("provides comprehensive weather risk summary for location", () => {
    // Record multiple weather events for a location
    const drought_result = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('Nebraska'),
        Cl.uint(95),
        Cl.uint(40),
        Cl.uint(25),
        Cl.uint(1) // drought
      ],
      deployer
    );
    
    const flood_result = simnet.callPublicFn(
      "weather-risk-assessment",
      "record-weather-data",
      [
        Cl.stringAscii('Nebraska'),
        Cl.uint(40),
        Cl.uint(85),
        Cl.uint(300),
        Cl.uint(2) // flood
      ],
      deployer
    );
    
    expect(drought_result.result).toBeOk(Cl.uint(1));
    expect(flood_result.result).toBeOk(Cl.uint(2));
    
    // Get weather risk summary
    const summaryCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-weather-risk-summary',
      [Cl.stringAscii('Nebraska')],
      deployer
    );
    
    // Check the summary fields individually
    expect(summaryCall.result).toBeTuple({
      'location': Cl.stringAscii('Nebraska'),
      'total-events': Cl.uint(2),
      'drought-frequency': Cl.uint(1),
      'flood-frequency': Cl.uint(1),
      'current-risk-level': Cl.uint(3), // risk-high for flood event
      'insurance-multiplier': Cl.uint(170) // multiplier for high risk
    });
  });

  it.skip("allows owner to create and dismiss weather alerts for tokens", () => {
    // Create weather alert for token ID 1
    const alertResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "create-weather-alert",
      [
        Cl.uint(1),   // token-id
        Cl.uint(3),   // alert-level (high)
        Cl.uint(4),   // weather-event (hail)
        Cl.uint(25)   // estimated-impact percentage
      ],
      deployer
    );
    
    expect(alertResult.result).toBeOk(Cl.bool(true));
    
    // Check alert was created
    const alertCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-token-weather-alert',
      [Cl.uint(1)],
      deployer
    );
    
    // Check alert was created by extracting and verifying individual fields
    const alertData = alertCall.result.expectSome();
    const alertTuple = alertData.expectTuple();
    expect(alertTuple['alert-level']).toBeUint(3);
    expect(alertTuple['weather-event']).toBeUint(4);
    expect(alertTuple['active']).toBeBool(true);
    expect(alertTuple['location']).toBeAscii('unknown');
    expect(alertTuple['estimated-impact']).toBeUint(25);
    expect(alertTuple['alert-date']).toBeUint(); // Accept any timestamp
    
    // Dismiss the alert
    const dismissResult = simnet.callPublicFn(
      "weather-risk-assessment",
      "dismiss-weather-alert",
      [Cl.uint(1)],
      deployer
    );
    
    expect(dismissResult.result).toBeOk(Cl.bool(true));
    
    // Check alert is now inactive
    const updatedAlertCall = simnet.callReadOnlyFn(
      'weather-risk-assessment',
      'get-token-weather-alert',
      [Cl.uint(1)],
      deployer
    );
    
    // Check alert is now inactive by extracting and verifying individual fields
    const updatedAlertData = updatedAlertCall.result.expectSome();
    const updatedAlertTuple = updatedAlertData.expectTuple();
    expect(updatedAlertTuple['alert-level']).toBeUint(3);
    expect(updatedAlertTuple['weather-event']).toBeUint(4);
    expect(updatedAlertTuple['active']).toBeBool(false); // now inactive
    expect(updatedAlertTuple['location']).toBeAscii('unknown');
    expect(updatedAlertTuple['estimated-impact']).toBeUint(25);
    expect(updatedAlertTuple['alert-date']).toBeUint(); // Accept any timestamp
  });
});
