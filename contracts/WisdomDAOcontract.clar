
;; title: WisdomDAO - Collective Intelligence Marketplace
;; version: 1.0.0
;; summary: A stake-to-predict platform where accuracy builds reputation capital
;; description: Enables domain experts to stake on predictions, earn rewards for accuracy,
;;              and mint Knowledge NFTs for proven insights

;; traits
(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; token definitions
(define-fungible-token wisdom-token)
(define-non-fungible-token knowledge-nft uint)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_STAKE (err u103))
(define-constant ERR_PREDICTION_CLOSED (err u104))
(define-constant ERR_UNAUTHORIZED (err u105))
(define-constant ERR_INVALID_OUTCOME (err u106))
(define-constant ERR_ALREADY_RESOLVED (err u107))
(define-constant ERR_INSUFFICIENT_BALANCE (err u108))
(define-constant ERR_DIVISION_BY_ZERO (err u109))
(define-constant ERR_OVERFLOW (err u110))
(define-constant ERR_ALREADY_INITIALIZED (err u111))
(define-constant ERR_PAUSED (err u112))
(define-constant ERR_INVALID_INPUT (err u113))
(define-constant ERR_PREDICTION_CHANGED (err u114))

(define-constant MIN_STAKE u1000000) ;; 1 WISDOM token (6 decimals)
(define-constant PLATFORM_FEE u50) ;; 5% fee (basis points / 100)
(define-constant ACCURACY_THRESHOLD u80) ;; 80% accuracy needed for NFT minting
(define-constant MAX_PREDICTION_DURATION u144) ;; ~1 day in blocks
(define-constant MAX_STAKE u1000000000000) ;; Maximum stake to prevent overflow

;; data vars
(define-data-var next-prediction-id uint u1)
(define-data-var next-nft-id uint u1)
(define-data-var total-wisdom-supply uint u0)
(define-data-var platform-treasury uint u0)
(define-data-var initialized bool false)
(define-data-var paused bool false)

;; data maps
(define-map predictions
  { prediction-id: uint }
  {
    creator: principal,
    title: (string-ascii 256),
    description: (string-ascii 1024),
    domain: (string-ascii 64),
    end-block: uint,
    total-stake-yes: uint,
    total-stake-no: uint,
    outcome: (optional bool), ;; none = unresolved, some(true) = yes, some(false) = no
    quality-score: uint, ;; 0-100
    resolved-block: (optional uint)
  }
)

(define-map user-predictions
  { user: principal, prediction-id: uint }
  {
    stake-amount: uint,
    prediction: bool, ;; true = yes, false = no
    claimed: bool
  }
)

(define-map user-stats
  { user: principal }
  {
    total-predictions: uint,
    correct-predictions: uint,
    total-staked: uint,
    total-earned: uint,
    reputation-score: uint, ;; Bayesian accuracy score (0-10000 basis points)
    domain-expertise: (list 10 (string-ascii 64)) ;; up to 10 domains
  }
)

(define-map domain-experts
  { domain: (string-ascii 64) }
  { experts: (list 50 principal) } ;; top 50 experts per domain
)

(define-map knowledge-nfts
  { nft-id: uint }
  {
    prediction-id: uint,
    creator: principal,
    accuracy-score: uint,
    domain: (string-ascii 64),
    minted-block: uint,
    uri: (string-ascii 256)
  }
)

(define-map question-curators
  { user: principal }
  { 
    total-curated: uint,
    quality-rating: uint,
    is-verified: bool
  }
)

;; public functions

;; Initialize the platform with initial token supply (one-time only)
(define-public (initialize (initial-supply uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (asserts! (not (var-get initialized)) ERR_ALREADY_INITIALIZED)
    (asserts! (> initial-supply u0) ERR_INVALID_INPUT)
    (try! (ft-mint? wisdom-token initial-supply CONTRACT_OWNER))
    (var-set total-wisdom-supply initial-supply)
    (var-set initialized true)
    (ok true)
  )
)

;; Create a new prediction market
(define-public (create-prediction 
  (title (string-ascii 256))
  (description (string-ascii 1024))
  (domain (string-ascii 64))
  (duration uint))
  (let (
    (prediction-id (var-get next-prediction-id))
    (end-block (+ stacks-block-height duration))
  )
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (> duration u0) ERR_INVALID_INPUT)
    (asserts! (<= duration MAX_PREDICTION_DURATION) ERR_INVALID_OUTCOME)
    (asserts! (> (len title) u0) ERR_INVALID_INPUT)
    (asserts! (> (len description) u0) ERR_INVALID_INPUT)
    (asserts! (> (len domain) u0) ERR_INVALID_INPUT)
    
    (map-set predictions
      { prediction-id: prediction-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        domain: domain,
        end-block: end-block,
        total-stake-yes: u0,
        total-stake-no: u0,
        outcome: none,
        quality-score: u50, ;; default quality score
        resolved-block: none
      }
    )
    
    (var-set next-prediction-id (+ prediction-id u1))
    (unwrap-panic (update-user-curation-stats tx-sender))
    (print { event: "prediction-created", prediction-id: prediction-id, creator: tx-sender })
    (ok prediction-id)
  )
)

;; Stake on a prediction
(define-public (stake-prediction (prediction-id uint) (prediction bool) (amount uint))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
    (existing-stake (map-get? user-predictions { user: tx-sender, prediction-id: prediction-id }))
    (current-yes-stake (get total-stake-yes prediction-data))
    (current-no-stake (get total-stake-no prediction-data))
  )
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (> amount u0) ERR_INVALID_INPUT)
    (asserts! (>= amount MIN_STAKE) ERR_INSUFFICIENT_STAKE)
    (asserts! (<= amount MAX_STAKE) ERR_OVERFLOW)
    (asserts! (< stacks-block-height (get end-block prediction-data)) ERR_PREDICTION_CLOSED)
    (asserts! (is-none (get outcome prediction-data)) ERR_ALREADY_RESOLVED)
    
    ;; Prevent changing prediction after initial stake
    (match existing-stake
      stake-data (asserts! (is-eq (get prediction stake-data) prediction) ERR_PREDICTION_CHANGED)
      true
    )
    
    ;; Check for overflow before adding
    (if prediction
      (asserts! (<= (+ current-yes-stake amount) MAX_STAKE) ERR_OVERFLOW)
      (asserts! (<= (+ current-no-stake amount) MAX_STAKE) ERR_OVERFLOW)
    )
    
    ;; Transfer tokens from user
    (try! (ft-transfer? wisdom-token amount tx-sender (as-contract tx-sender)))
    
    ;; Update prediction totals
    (if prediction
      (map-set predictions { prediction-id: prediction-id }
        (merge prediction-data { total-stake-yes: (+ current-yes-stake amount) }))
      (map-set predictions { prediction-id: prediction-id }
        (merge prediction-data { total-stake-no: (+ current-no-stake amount) }))
    )
    
    ;; Update user stake
    (map-set user-predictions 
      { user: tx-sender, prediction-id: prediction-id }
      {
        stake-amount: (+ (default-to u0 (get stake-amount existing-stake)) amount),
        prediction: prediction,
        claimed: false
      }
    )
    
    ;; Update user stats
    (unwrap-panic (update-user-stake-stats tx-sender amount))
    
    (print { event: "stake-placed", user: tx-sender, prediction-id: prediction-id, amount: amount, prediction: prediction })
    (ok true)
  )
)

;; Resolve a prediction (only creator or contract owner)
(define-public (resolve-prediction (prediction-id uint) (outcome bool))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
  )
    (asserts! (or (is-eq tx-sender (get creator prediction-data)) 
                  (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (>= stacks-block-height (get end-block prediction-data)) ERR_PREDICTION_CLOSED)
    (asserts! (is-none (get outcome prediction-data)) ERR_ALREADY_RESOLVED)
    
    (map-set predictions { prediction-id: prediction-id }
      (merge prediction-data { 
        outcome: (some outcome),
        resolved-block: (some stacks-block-height)
      })
    )
    
    (print { event: "prediction-resolved", prediction-id: prediction-id, outcome: outcome })
    (ok true)
  )
)

;; Claim rewards for correct prediction
(define-public (claim-rewards (prediction-id uint))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
    (user-stake (unwrap! (map-get? user-predictions { user: tx-sender, prediction-id: prediction-id }) ERR_NOT_FOUND))
    (outcome (unwrap! (get outcome prediction-data) ERR_NOT_FOUND))
  )
    (asserts! (not (get claimed user-stake)) ERR_ALREADY_RESOLVED)
    (asserts! (is-eq (get prediction user-stake) outcome) ERR_INVALID_OUTCOME)
    
    (let (
      (total-winning-stake (if outcome 
                            (get total-stake-yes prediction-data)
                            (get total-stake-no prediction-data)))
      (total-losing-stake (if outcome 
                           (get total-stake-no prediction-data)
                           (get total-stake-yes prediction-data)))
      (user-stake-amount (get stake-amount user-stake))
      (platform-fee-amount (/ (* total-losing-stake PLATFORM_FEE) u1000))
      (remaining-pool (- total-losing-stake platform-fee-amount))
    )
      ;; Check for division by zero
      (asserts! (> total-winning-stake u0) ERR_DIVISION_BY_ZERO)
      
      (let (
        (user-reward (+ user-stake-amount (/ (* user-stake-amount remaining-pool) total-winning-stake)))
      )
        ;; Mark as claimed BEFORE transfer (checks-effects-interactions)
        (map-set user-predictions
          { user: tx-sender, prediction-id: prediction-id }
          (merge user-stake { claimed: true })
        )
        
        ;; Update platform treasury
        (var-set platform-treasury (+ (var-get platform-treasury) platform-fee-amount))
        
        ;; Update user stats
        (unwrap-panic (update-user-success-stats tx-sender user-reward))
        
        ;; Transfer rewards to user (last step)
        (try! (as-contract (ft-transfer? wisdom-token user-reward tx-sender contract-caller)))
        
        (print { event: "rewards-claimed", user: tx-sender, prediction-id: prediction-id, reward: user-reward })
        (ok user-reward)
      )
    )
  )
)

;; Mint Knowledge NFT for high-accuracy predictions
(define-public (mint-knowledge-nft (prediction-id uint) (uri (string-ascii 256)))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
    (user-stats-data (get-user-stats-or-default tx-sender))
    (user-stake (unwrap! (map-get? user-predictions { user: tx-sender, prediction-id: prediction-id }) ERR_NOT_FOUND))
    (nft-id (var-get next-nft-id))
  )
    (asserts! (not (var-get paused)) ERR_PAUSED)
    (asserts! (> (len uri) u0) ERR_INVALID_INPUT)
    (asserts! (is-some (get outcome prediction-data)) ERR_NOT_FOUND)
    (asserts! (get claimed user-stake) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get prediction user-stake) 
                     (unwrap-panic (get outcome prediction-data))) ERR_INVALID_OUTCOME)
    (asserts! (>= (get reputation-score user-stats-data) (* ACCURACY_THRESHOLD u100)) ERR_INSUFFICIENT_BALANCE)
    
    (try! (nft-mint? knowledge-nft nft-id tx-sender))
    
    (map-set knowledge-nfts
      { nft-id: nft-id }
      {
        prediction-id: prediction-id,
        creator: tx-sender,
        accuracy-score: (get reputation-score user-stats-data),
        domain: (get domain prediction-data),
        minted-block: stacks-block-height,
        uri: uri
      }
    )
    
    (var-set next-nft-id (+ nft-id u1))
    
    (print { event: "knowledge-nft-minted", user: tx-sender, nft-id: nft-id, prediction-id: prediction-id })
    (ok nft-id)
  )
)

;; Update question quality score (verified curators only)
(define-public (update-quality-score (prediction-id uint) (quality-score uint))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
    (curator-data (unwrap! (map-get? question-curators { user: tx-sender }) ERR_UNAUTHORIZED))
  )
    (asserts! (get is-verified curator-data) ERR_UNAUTHORIZED)
    (asserts! (<= quality-score u100) ERR_INVALID_OUTCOME)
    
    (map-set predictions { prediction-id: prediction-id }
      (merge prediction-data { quality-score: quality-score })
    )
    
    (ok true)
  )
)

;; Verify curator (only contract owner)
(define-public (verify-curator (curator principal))
  (let (
    (curator-data (default-to { total-curated: u0, quality-rating: u50, is-verified: false }
                   (map-get? question-curators { user: curator })))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (map-set question-curators
      { user: curator }
      (merge curator-data { is-verified: true })
    )
    (ok true)
  )
)

;; Pause/unpause contract (only contract owner)
(define-public (set-paused (paused-state bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (var-set paused paused-state)
    (ok true)
  )
)

;; read only functions

;; Get prediction details
(define-read-only (get-prediction (prediction-id uint))
  (map-get? predictions { prediction-id: prediction-id })
)

;; Get user's stake in a prediction
(define-read-only (get-user-stake (user principal) (prediction-id uint))
  (map-get? user-predictions { user: user, prediction-id: prediction-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

;; Get user statistics with defaults
(define-read-only (get-user-stats-or-default (user principal))
  (default-to 
    {
      total-predictions: u0,
      correct-predictions: u0,
      total-staked: u0,
      total-earned: u0,
      reputation-score: u5000, ;; 50% starting reputation
      domain-expertise: (list)
    }
    (map-get? user-stats { user: user })
  )
)

;; Calculate potential rewards
(define-read-only (calculate-potential-reward (prediction-id uint) (user principal))
  (let (
    (prediction-data (unwrap! (map-get? predictions { prediction-id: prediction-id }) ERR_NOT_FOUND))
    (user-stake (unwrap! (map-get? user-predictions { user: user, prediction-id: prediction-id }) ERR_NOT_FOUND))
  )
    (if (get claimed user-stake)
      (ok u0)
      (let (
        (user-prediction (get prediction user-stake))
        (total-winning-stake (if user-prediction 
                              (get total-stake-yes prediction-data)
                              (get total-stake-no prediction-data)))
        (total-losing-stake (if user-prediction 
                             (get total-stake-no prediction-data)
                             (get total-stake-yes prediction-data)))
        (user-stake-amount (get stake-amount user-stake))
        (platform-fee-amount (/ (* total-losing-stake PLATFORM_FEE) u1000))
        (remaining-pool (- total-losing-stake platform-fee-amount))
      )
        ;; Check for division by zero
        (if (is-eq total-winning-stake u0)
          (ok user-stake-amount)
          (ok (+ user-stake-amount (/ (* user-stake-amount remaining-pool) total-winning-stake)))
        )
      )
    )
  )
)

;; Get domain experts
(define-read-only (get-domain-experts (domain (string-ascii 64)))
  (map-get? domain-experts { domain: domain })
)

;; Get Knowledge NFT details
(define-read-only (get-knowledge-nft (nft-id uint))
  (map-get? knowledge-nfts { nft-id: nft-id })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-predictions: (- (var-get next-prediction-id) u1),
    total-nfts: (- (var-get next-nft-id) u1),
    platform-treasury: (var-get platform-treasury),
    total-wisdom-supply: (var-get total-wisdom-supply)
  }
)

;; NFT trait implementations
(define-read-only (get-last-token-id)
  (ok (- (var-get next-nft-id) u1))
)

(define-read-only (get-token-uri (nft-id uint))
  (ok (get uri (map-get? knowledge-nfts { nft-id: nft-id })))
)

(define-public (transfer (nft-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (nft-transfer? knowledge-nft nft-id sender recipient)
  )
)

;; private functions

;; Update user staking statistics
(define-private (update-user-stake-stats (user principal) (amount uint))
  (let (
    (current-stats (get-user-stats-or-default user))
  )
    (map-set user-stats { user: user }
      (merge current-stats {
        total-predictions: (+ (get total-predictions current-stats) u1),
        total-staked: (+ (get total-staked current-stats) amount)
      })
    )
    (ok true)
  )
)

;; Update user success statistics and reputation
(define-private (update-user-success-stats (user principal) (reward uint))
  (let (
    (current-stats (get-user-stats-or-default user))
    (new-correct (+ (get correct-predictions current-stats) u1))
    (total-preds (get total-predictions current-stats))
    ;; Bayesian reputation: (correct + 1) / (total + 2) * 10000
    (new-reputation (/ (* (+ new-correct u1) u10000) (+ total-preds u2)))
  )
    (map-set user-stats { user: user }
      (merge current-stats {
        correct-predictions: new-correct,
        total-earned: (+ (get total-earned current-stats) reward),
        reputation-score: new-reputation
      })
    )
    (ok true)
  )
)

;; Update user curation statistics
(define-private (update-user-curation-stats (user principal))
  (let (
    (current-curator (default-to { total-curated: u0, quality-rating: u50, is-verified: false }
                      (map-get? question-curators { user: user })))
  )
    (map-set question-curators { user: user }
      (merge current-curator {
        total-curated: (+ (get total-curated current-curator) u1)
      })
    )
    (ok true)
  )
)