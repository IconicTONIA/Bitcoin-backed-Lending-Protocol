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

(define-public (flash-loan 
    (asset-symbol (string-ascii 10)) 
    (amount uint) 
    (receiver <flash-loan-receiver>)
    (params (optional (buff 1024)))
)
    (let (
        (loan-id (var-get flash-loan-nonce))
        (asset-pool (default-to { total-deposits: u0, total-borrows: u0, last-update-time: u0 } 
                               (map-get? asset-pools asset-symbol)))
        (total-liquidity (get total-deposits asset-pool))
        (fee-amount (/ (* amount FLASH-LOAN-FEE) u10000))
    )
        ;; Check asset is supported
        (asserts! (is-asset-supported asset-symbol) ERR-ASSET-NOT-SUPPORTED)
        
        ;; Check there is enough liquidity
        (asserts! (<= amount total-liquidity) ERR-INSUFFICIENT-LIQUIDITY)
        
        ;; Set flash loan state
        (map-set flash-loan-state loan-id 
            { 
                active: true,
                borrower: tx-sender,
                asset: asset-symbol,
                amount: amount,
                fee: fee-amount
            }
        )
        
        ;; Increment nonce
        (var-set flash-loan-nonce (+ loan-id u1))
        
        ;; Transfer funds to receiver

        
        ;; Execute operation on receiver contract
        (match (contract-call? receiver execute-operation asset-symbol amount fee-amount loan-id)
            success 
            (begin
                ;; Verify loan was repaid with fee
                ;; In a real implementation, this would check that the funds were actually returned
                ;; to the protocol plus the fee

                ;; Update pool state to add the fee
                (map-set asset-pools asset-symbol 
                    { 
                        total-deposits: (+ total-liquidity fee-amount), 
                        total-borrows: (get total-borrows asset-pool),
                        last-update-time:stacks-block-height
                    }
                )
                
                ;; Set loan as inactive
                (map-set flash-loan-state loan-id 
                    { 
                        active: false,
                        borrower: tx-sender,
                        asset: asset-symbol,
                        amount: amount,
                        fee: fee-amount
                    }
                )
                
                (ok true)
            )
            error (begin
                ;; Operation failed, but in a real transaction this would revert anyway
                ;; Just for clarity in the code
                (err error)
            )
        )
    )
)

;; Interest bearing token trait
(define-trait interest-token-trait
    (
        (mint (uint principal) (response bool uint))
        (burn (uint principal) (response bool uint))
        (update-index (uint) (response uint uint))
    )
)

;; Interest token contracts by asset
(define-map asset-interest-token (string-ascii 10) principal)

;; Interest indices for accurate interest tracking
(define-map interest-index (string-ascii 10) { 
    borrow-index: uint,
    supply-index: uint,
    last-update-time: uint
})

;; Interest model parameters
(define-map interest-model-params (string-ascii 10) {
    base-rate: uint,
    slope1: uint,
    slope2: uint,
    optimal-utilization: uint
})

;; Calculate the current interest rate based on utilization
(define-read-only (calculate-interest-rate (asset-symbol (string-ascii 10)))
    (let (
        (pool (default-to { total-deposits: u0, total-borrows: u0, last-update-time: u0 } 
                          (map-get? asset-pools asset-symbol)))
        (model (default-to { 
            base-rate: INTEREST-RATE-BASE, 
            slope1: INTEREST-RATE-SLOPE1,
            slope2: INTEREST-RATE-SLOPE2,
            optimal-utilization: OPTIMAL-UTILIZATION
        } (map-get? interest-model-params asset-symbol)))
        (total-deposits (get total-deposits pool))
        (total-borrows (get total-borrows pool))
        (utilization-rate (if (is-eq total-deposits u0) 
                             u0 
                             (/ (* total-borrows u1000) total-deposits)))
        (base-rate (get base-rate model))
        (slope1 (get slope1 model))
        (slope2 (get slope2 model))
        (optimal-utilization (get optimal-utilization model))
    )
    (if (<= utilization-rate optimal-utilization)
        (+ base-rate (/ (* utilization-rate slope1) u1000))
        (+ base-rate (/ (* optimal-utilization slope1) u1000) 
           (/ (* (- utilization-rate optimal-utilization) slope2) u1000))
    ))
)

;; Update interest indices
(define-public (update-interest-indices (asset-symbol (string-ascii 10)))
    (let (
        (pool (default-to { total-deposits: u0, total-borrows: u0, last-update-time: u0 } 
                         (map-get? asset-pools asset-symbol)))
        (indices (default-to { borrow-index: u1000000, supply-index: u1000000, last-update-time: u0 } 
                             (map-get? interest-index asset-symbol)))
        (current-time stacks-block-height)
        (time-elapsed (- current-time (get last-update-time indices)))
        (borrow-rate (calculate-interest-rate asset-symbol))
        (borrow-interest (/ (* borrow-rate time-elapsed) SECONDS-PER-YEAR))
        (supply-rate (if (is-eq (get total-borrows pool) u0)
                       u0
                       (/ (* borrow-rate (get total-borrows pool)) (get total-deposits pool))))
        (supply-interest (/ (* supply-rate time-elapsed) SECONDS-PER-YEAR))
        (new-borrow-index (+ (get borrow-index indices) 
                            (/ (* (get borrow-index indices) borrow-interest) u10000)))
        (new-supply-index (+ (get supply-index indices) 
                            (/ (* (get supply-index indices) supply-interest) u10000)))
    )
        ;; Update indices
        (map-set interest-index asset-symbol {
            borrow-index: new-borrow-index,
            supply-index: new-supply-index,
            last-update-time: current-time
        })
        
        
        (ok { 
            borrow-index: new-borrow-index, 
            supply-index: new-supply-index 
        })
    )
)

;; Governance constants
(define-constant VOTING-DELAY u1440) ;; ~1 day in blocks
(define-constant VOTING-PERIOD u10080) ;; ~7 days in blocks
(define-constant PROPOSAL-THRESHOLD u100000000000) ;; 100,000 governance tokens
(define-constant QUORUM-VOTES u1000000000000) ;; 1,000,000 governance tokens

;; Proposal states
(define-constant PROPOSAL-STATE-PENDING u0)
(define-constant PROPOSAL-STATE-ACTIVE u1)
(define-constant PROPOSAL-STATE-CANCELED u2)
(define-constant PROPOSAL-STATE-DEFEATED u3)
(define-constant PROPOSAL-STATE-SUCCEEDED u4)
(define-constant PROPOSAL-STATE-QUEUED u5)
(define-constant PROPOSAL-STATE-EXECUTED u6)
(define-constant PROPOSAL-STATE-EXPIRED u7)

;; Governance token trait
(define-trait governance-token-trait
    (
        (get-balance (principal) (response uint uint))
        (get-total-supply () (response uint uint))
    )
)

;; Timelock data structures
(define-map timelock-transactions 
    uint
    {
        target: principal,
        function-name: (string-ascii 100),
        parameters: (list 10 (buff 100)),
        eta: uint,
        executed: bool
    }
)

;; Governance data structures
(define-data-var governance-token principal tx-sender) ;; Should be set to governance token contract
(define-data-var proposal-count uint u0)
(define-data-var timelock-delay uint u172800) ;; 2 days (in seconds)

(define-map proposals 
    uint 
    {
        proposer: principal,
        start-block: uint,
        end-block: uint,
        description: (string-utf8 500),
        for-votes: uint,
        against-votes: uint,
        canceled: bool,
        executed: bool,
        transaction-id: uint
    }
)

;; Risk parameters
(define-data-var global-pause bool false)
(define-map asset-pause (string-ascii 10) { 
    deposits-paused: bool, 
    borrows-paused: bool,
    liquidations-paused: bool
})

;; Reserve factors - percentage of interest that goes to protocol reserves
(define-map reserve-factor (string-ascii 10) uint) ;; Basis points

;; Protocol reserves
(define-map protocol-reserves (string-ascii 10) uint)

;; Circuit breaker thresholds
(define-map circuit-breakers (string-ascii 10) {
    price-decrease-threshold: uint, ;; Basis points
    price-increase-threshold: uint, ;; Basis points
    borrow-increase-threshold: uint, ;; Basis points
    deposit-decrease-threshold: uint ;; Basis points
})

;; Price history for volatility tracking
(define-map price-history 
    { asset: (string-ascii 10), timestamp: uint } 
    uint
)

;; Pause/unpause protocol globally
(define-public (set-global-pause (paused bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set global-pause paused)
        (ok paused)
    )
)

;; Set reserve factor for an asset
(define-public (set-reserve-factor (asset-symbol (string-ascii 10)) (factor uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= factor u5000) ERR-INVALID-AMOUNT) ;; Max 50%
        (map-set reserve-factor asset-symbol factor)
        (ok factor)
    )
)

;; Set circuit breaker thresholds
(define-public (set-circuit-breakers 
    (asset-symbol (string-ascii 10))
    (price-decrease-threshold uint)
    (price-increase-threshold uint)
    (borrow-increase-threshold uint)
    (deposit-decrease-threshold uint)
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set circuit-breakers asset-symbol {
            price-decrease-threshold: price-decrease-threshold,
            price-increase-threshold: price-increase-threshold,
            borrow-increase-threshold: borrow-increase-threshold,
            deposit-decrease-threshold: deposit-decrease-threshold
        })
        (ok true)
    )
)

;; Risk assessment score for a position
(define-read-only (get-position-risk-score (user principal) (position-id uint))
    (let (
        (position (default-to {
                collateral-asset: "",
                collateral-amount: u0,
                borrow-asset: "",
                borrow-amount: u0,
                interest-index: u0,
                timestamp: u0
            } (map-get? loan-positions { owner: user, position-id: position-id })))
        (collateral-ratio (get-collateral-ratio position))
        (liquidation-threshold (get liquidation-threshold 
                              (default-to {
                                collateral-factor: u750,
                                liquidation-threshold: LIQUIDATION-THRESHOLD,
                                liquidation-penalty: LIQUIDATION-PENALTY,
                                borrow-enabled: true,
                                deposit-enabled: true
                              } (map-get? asset-params (get collateral-asset position)))))
        ;; Distance to liquidation - higher is safer
        (safety-margin (- collateral-ratio liquidation-threshold))
    )
        ;; Convert to risk score (0-100, lower is safer)
        (if (>= safety-margin u100) 
            u0 ;; Very safe
            (if (<= safety-margin u0)
                u100 ;; Extremely risky or already liquidatable
                (- u100 safety-margin))) ;; Linear risk score based on safety margin
    )
)
