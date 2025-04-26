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



