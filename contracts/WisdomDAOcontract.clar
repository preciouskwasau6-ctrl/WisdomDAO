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

(define-constant MIN_STAKE u1000000) ;; 1 WISDOM token (6 decimals)
(define-constant PLATFORM_FEE u50) ;; 5% fee (basis points / 100)
(define-constant ACCURACY_THRESHOLD u80) ;; 80% accuracy needed for NFT minting
(define-constant MAX_PREDICTION_DURATION u144) ;; ~1 day in blocks

;; data vars
(define-data-var next-prediction-id uint u1)
(define-data-var next-nft-id uint u1)
(define-data-var total-wisdom-supply uint u0)
(define-data-var platform-treasury uint u0)

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
