;; Weather Risk Assessment and Alert System
;; This contract provides weather monitoring, risk assessment, and alert functionality
;; to help farmers and investors make informed decisions about crop protection

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-data (err u402))
(define-constant err-unauthorized (err u403))

;; Data variables
(define-data-var next-weather-record-id uint u1)
(define-data-var alert-threshold-score uint u70)

;; Weather risk levels
(define-constant risk-low u1)
(define-constant risk-medium u2)
(define-constant risk-high u3)
(define-constant risk-critical u4)

;; Weather event types
(define-constant event-drought u1)
(define-constant event-flood u2)
(define-constant event-frost u3)
(define-constant event-hail u4)
(define-constant event-storm u5)

;; Weather records for specific locations and time periods
(define-map weather-records
  { record-id: uint }
  {
    location: (string-ascii 50),
    date: uint,
    temperature-high: uint,
    humidity: uint,
    precipitation: uint,
    weather-event: uint,
    risk-score: uint,
    data-source: principal
  }
)

;; Location risk profiles
(define-map location-risk-profiles
  { location: (string-ascii 50) }
  {
    current-risk-level: uint,
    last-updated: uint,
    total-weather-events: uint,
    drought-frequency: uint,
    flood-frequency: uint,
    avg-temperature: uint
  }
)

;; Token weather alerts
(define-map token-weather-alerts
  { token-id: uint }
  {
    location: (string-ascii 50),
    alert-level: uint,
    weather-event: uint,
    alert-date: uint,
    estimated-impact: uint,
    active: bool
  }
)

;; Alert subscriptions
(define-map alert-subscriptions
  { farmer: principal, location: (string-ascii 50) }
  { subscribed: bool, notification-threshold: uint }
)

(define-map produce-details
  { token-id: uint }
  {
    farmer: principal,
    produce-type: (string-ascii 20),
    quantity: uint,
    expected-harvest-date: uint,
    price-per-unit: uint,
    location: (string-ascii 50),
    created-at: uint
  }
)

(define-map harvest-details
  { harvest-id: uint }
  {
    token-id: uint,
    claimed: bool,
    claim-date: uint
  }
)

(define-map token-ownership
  { token-id: uint }
  { owner: principal, shares: uint }
)

(define-map investor-holdings
  { investor: principal, token-id: uint }
  { shares: uint }
)

;; Weather Risk Assessment Functions

;; Insurance risk multiplier mapping
(define-map weather-risk-multipliers
  { risk-level: uint }
  { multiplier: uint }
)

;; Initialize risk multipliers
(map-set weather-risk-multipliers { risk-level: risk-low } { multiplier: u100 })
(map-set weather-risk-multipliers { risk-level: risk-medium } { multiplier: u130 })
(map-set weather-risk-multipliers { risk-level: risk-high } { multiplier: u170 })
(map-set weather-risk-multipliers { risk-level: risk-critical } { multiplier: u250 })

;; Read-only functions
(define-read-only (get-weather-record (record-id uint))
  (map-get? weather-records { record-id: record-id })
)

(define-read-only (get-location-risk-profile (location (string-ascii 50)))
  (default-to
    { current-risk-level: risk-low, last-updated: u0, total-weather-events: u0, drought-frequency: u0, flood-frequency: u0, avg-temperature: u0 }
    (map-get? location-risk-profiles { location: location })
  )
)

(define-read-only (get-token-weather-alert (token-id uint))
  (map-get? token-weather-alerts { token-id: token-id })
)

(define-read-only (get-alert-subscription (farmer principal) (location (string-ascii 50)))
  (default-to
    { subscribed: false, notification-threshold: u50 }
    (map-get? alert-subscriptions { farmer: farmer, location: location })
  )
)

(define-read-only (get-risk-multiplier (risk-level uint))
  (default-to
    { multiplier: u100 }
    (map-get? weather-risk-multipliers { risk-level: risk-level })
  )
)

;; Record weather data (only authorized data sources)
(define-public (record-weather-data (location (string-ascii 50)) (temperature-high uint) (humidity uint) (precipitation uint) (weather-event uint))
  (let
    (
      (record-id (var-get next-weather-record-id))
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
      (risk-score (calculate-weather-risk-score temperature-high humidity precipitation weather-event))
    )
    ;; Only contract owner or authorized sources can record weather data
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (and (>= weather-event u0) (<= weather-event event-storm)) err-invalid-data)
    (asserts! (<= humidity u100) err-invalid-data)
    
    (map-set weather-records
      { record-id: record-id }
      {
        location: location,
        date: current-time,
        temperature-high: temperature-high,
        humidity: humidity,
        precipitation: precipitation,
        weather-event: weather-event,
        risk-score: risk-score,
        data-source: tx-sender
      }
    )
    
    ;; Update location risk profile
    (unwrap-panic (update-location-risk-profile location weather-event risk-score))
    
    ;; Check if alerts need to be triggered
    (unwrap-panic (check-and-trigger-alerts location risk-score weather-event))
    
    (var-set next-weather-record-id (+ record-id u1))
    (ok record-id)
  )
)

;; Calculate weather risk score based on multiple factors
(define-private (calculate-weather-risk-score (temperature-high uint) (humidity uint) (precipitation uint) (weather-event uint))
  (let
    (
      (temp-risk (if (or (< temperature-high u32) (> temperature-high u104)) u30 u0))
      (humidity-risk (if (or (< humidity u30) (> humidity u90)) u20 u0))
      (precip-risk (if (or (is-eq precipitation u0) (> precipitation u200)) u25 u0))
      (event-risk (if (> weather-event u0) u40 u0))
    )
    (+ temp-risk humidity-risk precip-risk event-risk)
  )
)

;; Update location risk profile based on new weather data
(define-private (update-location-risk-profile (location (string-ascii 50)) (weather-event uint) (risk-score uint))
  (let
    (
      (current-profile (get-location-risk-profile location))
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
      (new-total-events (+ (get total-weather-events current-profile) (if (> weather-event u0) u1 u0)))
      (new-drought-freq (+ (get drought-frequency current-profile) (if (is-eq weather-event event-drought) u1 u0)))
      (new-flood-freq (+ (get flood-frequency current-profile) (if (is-eq weather-event event-flood) u1 u0)))
      (new-risk-level (if (>= risk-score u80) 
                         risk-critical
                         (if (>= risk-score u60)
                           risk-high
                           (if (>= risk-score u30)
                             risk-medium
                             risk-low))))
    )
    (map-set location-risk-profiles
      { location: location }
      {
        current-risk-level: new-risk-level,
        last-updated: current-time,
        total-weather-events: new-total-events,
        drought-frequency: new-drought-freq,
        flood-frequency: new-flood-freq,
        avg-temperature: u75 ;; simplified - could be calculated from historical data
      }
    )
    (ok true)
  )
)

;; Check and trigger weather alerts for tokens in affected location
(define-private (check-and-trigger-alerts (location (string-ascii 50)) (risk-score uint) (weather-event uint))
  (let
    (
      (alert-threshold (var-get alert-threshold-score))
    )
    (if (>= risk-score alert-threshold)
      (begin
        ;; This would trigger alerts for all tokens in this location
        ;; For simplicity, we'll just return success
        (ok true)
      )
      (ok false)
    )
  )
)

;; Subscribe to weather alerts for a location
(define-public (subscribe-to-weather-alerts (location (string-ascii 50)) (notification-threshold uint))
  (begin
    (asserts! (and (>= notification-threshold u0) (<= notification-threshold u100)) err-invalid-data)
    
    (map-set alert-subscriptions
      { farmer: tx-sender, location: location }
      { subscribed: true, notification-threshold: notification-threshold }
    )
    (ok true)
  )
)

;; Unsubscribe from weather alerts
(define-public (unsubscribe-from-weather-alerts (location (string-ascii 50)))
  (begin
    (map-set alert-subscriptions
      { farmer: tx-sender, location: location }
      { subscribed: false, notification-threshold: u0 }
    )
    (ok true)
  )
)

;; Create weather alert for specific token
(define-public (create-weather-alert (token-id uint) (alert-level uint) (weather-event uint) (estimated-impact uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
    )
    ;; Only contract owner can create alerts
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= alert-level risk-low) (<= alert-level risk-critical)) err-invalid-data)
    (asserts! (and (>= weather-event u0) (<= weather-event event-storm)) err-invalid-data)
    
    (map-set token-weather-alerts
      { token-id: token-id }
      {
        location: "unknown", ;; Would be populated from token details
        alert-level: alert-level,
        weather-event: weather-event,
        alert-date: current-time,
        estimated-impact: estimated-impact,
        active: true
      }
    )
    (ok true)
  )
)

;; Dismiss weather alert
(define-public (dismiss-weather-alert (token-id uint))
  (let
    (
      (alert (unwrap! (map-get? token-weather-alerts { token-id: token-id }) err-not-found))
    )
    (map-set token-weather-alerts
      { token-id: token-id }
      (merge alert { active: false })
    )
    (ok true)
  )
)

;; Get weather-adjusted insurance premium
(define-read-only (get-weather-adjusted-premium (base-premium uint) (location (string-ascii 50)))
  (let
    (
      (location-profile (get-location-risk-profile location))
      (risk-level (get current-risk-level location-profile))
      (multiplier (get multiplier (get-risk-multiplier risk-level)))
    )
    (/ (* base-premium multiplier) u100)
  )
)

;; Get weather risk summary for a location
(define-read-only (get-weather-risk-summary (location (string-ascii 50)))
  (let
    (
      (profile (get-location-risk-profile location))
    )
    {
      location: location,
      current-risk-level: (get current-risk-level profile),
      total-events: (get total-weather-events profile),
      drought-frequency: (get drought-frequency profile),
      flood-frequency: (get flood-frequency profile),
      insurance-multiplier: (get multiplier (get-risk-multiplier (get current-risk-level profile)))
    }
  )
)




