(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-harvest-not-ready (err u105))
(define-constant err-insufficient-tokens (err u106))
(define-constant err-harvest-already-claimed (err u107))

;; (define-non-fungible-token farm-produce-token test 'uint)
;; (define-non-fungible-token harvest-claim test 'uint)

(define-data-var last-token-id uint u0)
(define-data-var last-harvest-id uint u0)

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

(define-read-only (get-last-token-id)
  (var-get last-token-id)
)

(define-read-only (get-last-harvest-id)
  (var-get last-harvest-id)
)

(define-read-only (get-produce-details (token-id uint))
  (map-get? produce-details { token-id: token-id })
)

(define-read-only (get-harvest-details (harvest-id uint))
  (map-get? harvest-details { harvest-id: harvest-id })
)

(define-read-only (get-token-owner (token-id uint))
  (map-get? token-ownership { token-id: token-id })
)

(define-read-only (get-investor-shares (investor principal) (token-id uint))
  (default-to u0 (get shares (map-get? investor-holdings { investor: investor, token-id: token-id })))
)

(define-public (register-produce (produce-type (string-ascii 20)) (quantity uint) (expected-harvest-date uint) (price-per-unit uint) (location (string-ascii 50)))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (block-time (unwrap-panic (get-stacks-block-info? time u0)))
    )
    (asserts! (> quantity u0) err-invalid-amount)
    (asserts! (> expected-harvest-date block-time) err-invalid-amount)
    
    ;; (try! (nft-mint? farm-produce-token token-id tx-sender))
    (map-set produce-details
      { token-id: token-id }
      {
        farmer: tx-sender,
        produce-type: produce-type,
        quantity: quantity,
        expected-harvest-date: expected-harvest-date,
        price-per-unit: price-per-unit,
        location: location,
        created-at: block-time
      }
    )
    (map-set token-ownership
      { token-id: token-id }
      { owner: tx-sender, shares: quantity }
    )
    (map-set investor-holdings
      { investor: tx-sender, token-id: token-id }
      { shares: quantity }
    )
    (var-set last-token-id token-id)
    (ok token-id)
  )
)

(define-public (buy-shares (token-id uint) (shares uint))
  (let
    (
      (produce (unwrap! (map-get? produce-details { token-id: token-id }) err-not-found))
      (ownership (unwrap! (map-get? token-ownership { token-id: token-id }) err-not-found))
      (seller (get owner ownership))
      (seller-shares (get shares ownership))
      (price-per-unit (get price-per-unit produce))
      (total-price (* shares price-per-unit))
      (investor-current-shares (get-investor-shares tx-sender token-id))
    )
    (asserts! (>= seller-shares shares) err-insufficient-tokens)
    ;; (asserts! (is-eq (unwrap! (nft-get-owner? farm-produce-token token-id) err-not-found) seller) err-unauthorized)
    
    (try! (stx-transfer? total-price tx-sender seller))
    
    (map-set token-ownership
      { token-id: token-id }
      { owner: seller, shares: (- seller-shares shares) }
    )
    
    (map-set investor-holdings
      { investor: seller, token-id: token-id }
      { shares: (- seller-shares shares) }
    )
    
    (map-set investor-holdings
      { investor: tx-sender, token-id: token-id }
      { shares: (+ investor-current-shares shares) }
    )
    
    (ok true)
  )
)

(define-public (register-harvest (token-id uint))
  (let
    (
      (produce (unwrap! (map-get? produce-details { token-id: token-id }) err-not-found))
      (farmer (get farmer produce))
      (expected-date (get expected-harvest-date produce))
      (block-time (unwrap-panic (get-stacks-block-info? time u0)))
      (harvest-id (+ (var-get last-harvest-id) u1))
    )
    (asserts! (is-eq tx-sender farmer) err-unauthorized)
    (asserts! (>= block-time expected-date) err-harvest-not-ready)
    
    ;; (try! (nft-mint? harvest-claim harvest-id tx-sender))
    (map-set harvest-details
      { harvest-id: harvest-id }
      {
        token-id: token-id,
        claimed: false,
        claim-date: u0
      }
    )
    (var-set last-harvest-id harvest-id)
    (ok harvest-id)
  )
)

(define-public (claim-harvest-share (harvest-id uint))
  (let
    (
      (harvest (unwrap! (map-get? harvest-details { harvest-id: harvest-id }) err-not-found))
      (token-id (get token-id harvest))
      (produce (unwrap! (map-get? produce-details { token-id: token-id }) err-not-found))
      (investor-shares (get-investor-shares tx-sender token-id))
      (block-time (unwrap-panic (get-stacks-block-info? time u0)))
    )
    (asserts! (> investor-shares u0) err-unauthorized)
    (asserts! (not (get claimed harvest)) err-harvest-already-claimed)
    
    (map-set harvest-details
      { harvest-id: harvest-id }
      {
        token-id: token-id,
        claimed: true,
        claim-date: block-time
      }
    )
    
    (ok { token-id: token-id, shares: investor-shares })
  )
)

(define-public (transfer-shares (token-id uint) (recipient principal) (shares uint))
  (let
    (
      (sender-shares (get-investor-shares tx-sender token-id))
      (recipient-shares (get-investor-shares recipient token-id))
    )
    (asserts! (>= sender-shares shares) err-insufficient-tokens)
    
    (map-set investor-holdings
      { investor: tx-sender, token-id: token-id }
      { shares: (- sender-shares shares) }
    )
    
    (map-set investor-holdings
      { investor: recipient, token-id: token-id }
      { shares: (+ recipient-shares shares) }
    )
    
    (ok true)
  )
)