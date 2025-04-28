;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u101))
(define-constant ERR-BELOW-MIN-COLLATERAL-RATIO (err u102))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-POSITION-NOT-FOUND (err u105))
(define-constant ERR-NO-POSITION-TO-LIQUIDATE (err u106))
(define-constant ERR-LIQUIDATION-THRESHOLD-NOT-REACHED (err u107))
(define-constant ERR-ASSET-NOT-SUPPORTED (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-COLLATERAL-RATIO u150) ;; 150% minimum collateral ratio
(define-constant LIQUIDATION-THRESHOLD u130) ;; 130% liquidation threshold
(define-constant LIQUIDATION-PENALTY u10) ;; 10% liquidation penalty
(define-constant ORIGINATION-FEE u1) ;; 0.1% origination fee (basis points)
(define-constant INTEREST-RATE-BASE u1000) ;; Base interest rate (basis points per year)
(define-constant INTEREST-RATE-SLOPE1 u2000) ;; Interest rate slope 1 (basis points per year)
(define-constant INTEREST-RATE-SLOPE2 u5000) ;; Interest rate slope 2 (basis points per year)
(define-constant OPTIMAL-UTILIZATION u800) ;; 80% optimal utilization rate
(define-constant SECONDS-PER-YEAR u31536000) ;; Seconds in a year for interest calculations

;; Data maps for protocol state
(define-map supported-assets (string-ascii 10) bool)
(define-map oracle-prices (string-ascii 10) uint)
(define-map asset-pools (string-ascii 10) {
    total-deposits: uint,
    total-borrows: uint,
    last-update-time: uint
})

;; Loan positions
(define-map loan-positions 
    { owner: principal, position-id: uint }
    {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }
)

(define-map user-position-ids 
    principal 
    (list 100 uint)
)

(define-data-var next-position-id uint u1)

;; Governance parameters
(define-map asset-params 
    (string-ascii 10)
    {
        collateral-factor: uint,
        liquidation-threshold: uint,
        liquidation-penalty: uint,
        borrow-enabled: bool,
        deposit-enabled: bool
    }
)

;; Helper functions
(define-private (is-asset-supported (asset-symbol (string-ascii 10)))
    (default-to false (map-get? supported-assets asset-symbol))
)

(define-private (get-asset-price (asset-symbol (string-ascii 10)))
    (default-to u0 (map-get? oracle-prices asset-symbol))
)

(define-private (get-position-collateral-value (position {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }))
    (let (
        (collateral-price (get-asset-price (get collateral-asset position)))
        (collateral-amount (get collateral-amount position))
    )
    (/ (* collateral-amount collateral-price) u1000000))
)

(define-private (get-position-borrow-value (position {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }))
    (let (
        (borrow-price (get-asset-price (get borrow-asset position)))
        (borrow-amount (get borrow-amount position))
    )
    (/ (* borrow-amount borrow-price) u1000000))
)

(define-private (get-collateral-ratio (position {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }))
    (let (
        (collateral-value (get-position-collateral-value position))
        (borrow-value (get-position-borrow-value position))
    )
    (if (is-eq borrow-value u0)
        u0
        (/ (* collateral-value u100) borrow-value)))
)

(define-private (is-position-healthy (position {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }))
    (>= (get-collateral-ratio position) MIN-COLLATERAL-RATIO)
)

(define-private (is-position-liquidatable (position {
        collateral-asset: (string-ascii 10),
        collateral-amount: uint,
        borrow-asset: (string-ascii 10),
        borrow-amount: uint,
        interest-index: uint,
        timestamp: uint
    }))
    (let (
        (params (default-to {
            collateral-factor: u750,
            liquidation-threshold: LIQUIDATION-THRESHOLD,
            liquidation-penalty: LIQUIDATION-PENALTY,
            borrow-enabled: true,
            deposit-enabled: true
        } (map-get? asset-params (get collateral-asset position))))
        (liquidation-threshold (get liquidation-threshold params))
    )
    (< (get-collateral-ratio position) liquidation-threshold))
)

;; Read-only functions
(define-read-only (get-collateral-ratio-for-position (user principal) (position-id uint))
    (let (
        (position (default-to {
                collateral-asset: "",
                collateral-amount: u0,
                borrow-asset: "",
                borrow-amount: u0,
                interest-index: u0,
                timestamp: u0
            } (map-get? loan-positions { owner: user, position-id: position-id })))
    )
    (get-collateral-ratio position))
)

(define-read-only (get-max-borrow-amount (user principal) (position-id uint) (borrow-asset (string-ascii 10)))
    (let (
        (position (default-to {
                collateral-asset: "",
                collateral-amount: u0,
                borrow-asset: "",
                borrow-amount: u0,
                interest-index: u0,
                timestamp: u0
            } (map-get? loan-positions { owner: user, position-id: position-id })))
        (collateral-asset (get collateral-asset position))
        (asset-param (default-to {
            collateral-factor: u750,
            liquidation-threshold: LIQUIDATION-THRESHOLD,
            liquidation-penalty: LIQUIDATION-PENALTY,
            borrow-enabled: true,
            deposit-enabled: true
        } (map-get? asset-params collateral-asset)))
        (collateral-value (get-position-collateral-value position))
        (collateral-factor (get collateral-factor asset-param))
        (max-borrow-value (/ (* collateral-value collateral-factor) u1000))
        (current-borrow-value (get-position-borrow-value position))
        (available-borrow-value (- max-borrow-value current-borrow-value))
        (borrow-price (get-asset-price borrow-asset))
    )
    (if (is-eq borrow-price u0)
        u0
        (/ (* available-borrow-value u1000000) borrow-price))
    )
)



(define-read-only (get-user-positions (user principal))
    (default-to (list) (map-get? user-position-ids user))
)

(define-read-only (get-asset-total-deposits (asset-symbol (string-ascii 10)))
    (let (
        (pool (default-to { total-deposits: u0, total-borrows: u0, last-update-time: u0 } 
                          (map-get? asset-pools asset-symbol)))
    )
    (get total-deposits pool))
)

(define-read-only (get-asset-total-borrows (asset-symbol (string-ascii 10)))
    (let (
        (pool (default-to { total-deposits: u0, total-borrows: u0, last-update-time: u0 } 
                          (map-get? asset-pools asset-symbol)))
    )
    (get total-borrows pool))
)

;; Flash loan constants
(define-constant FLASH-LOAN-FEE u10) ;; 0.1% fee (basis points)

;; Flash loan data
(define-map flash-loan-state 
    uint 
    { 
        active: bool,
        borrower: principal,
        asset: (string-ascii 10),
        amount: uint,
        fee: uint
    }
)

(define-data-var flash-loan-nonce uint u0)

;; Flash loans require callbacks to ensure the loan is repaid in the same transaction
(define-trait flash-loan-receiver 
    (
        (execute-operation ((string-ascii 10) uint uint uint) (response bool uint))
    )
)