;; OmniCrafting - Dynamic Resource Management System
;; A blockchain-based crafting ecosystem with temporal mechanics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-resources (err u102))
(define-constant err-invalid-craft (err u103))
(define-constant err-already-exists (err u104))

;; Data Variables
(define-data-var next-resource-id uint u1)
(define-data-var next-item-id uint u1)

;; Resource Types
(define-map resource-types
  { type-id: uint }
  {
    name: (string-ascii 50),
    base-value: uint,
    decay-rate: uint,
    is-organic: bool
  }
)

;; Player Resources
(define-map player-resources
  { owner: principal, resource-id: uint }
  {
    type-id: uint,
    quantity: uint,
    creation-block: uint,
    last-update-block: uint,
    quality: uint
  }
)

;; Crafted Items
(define-map crafted-items
  { item-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    creation-block: uint,
    usage-count: uint,
    quality: uint,
    preserved: bool
  }
)

;; Crafting Recipes
(define-map crafting-recipes
  { recipe-id: uint }
  {
    name: (string-ascii 50),
    required-type-1: uint,
    required-qty-1: uint,
    required-type-2: uint,
    required-qty-2: uint,
    output-quality: uint
  }
)

;; Read-only functions

(define-read-only (get-resource-type (type-id uint))
  (map-get? resource-types { type-id: type-id })
)

(define-read-only (get-player-resource (owner principal) (resource-id uint))
  (map-get? player-resources { owner: owner, resource-id: resource-id })
)

(define-read-only (get-crafted-item (item-id uint))
  (map-get? crafted-items { item-id: item-id })
)

(define-read-only (get-recipe (recipe-id uint))
  (map-get? crafting-recipes { recipe-id: recipe-id })
)

(define-read-only (calculate-decay (creation-block uint) (decay-rate uint))
  (let
    (
      (blocks-passed (- block-height creation-block))
      (decay-amount (* blocks-passed decay-rate))
    )
    decay-amount
  )
)

(define-read-only (get-current-quality (owner principal) (resource-id uint))
  (match (get-player-resource owner resource-id)
    resource-data
      (let
        (
          (type-data (unwrap! (get-resource-type (get type-id resource-data)) u0))
          (decay (calculate-decay (get creation-block resource-data) (get decay-rate type-data)))
          (original-quality (get quality resource-data))
        )
        (if (> decay original-quality)
          u0
          (- original-quality decay)
        )
      )
    u0
  )
)

;; Public functions

(define-public (create-resource-type (name (string-ascii 50)) (base-value uint) (decay-rate uint) (is-organic bool))
  (let
    (
      (type-id (var-get next-resource-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set resource-types
      { type-id: type-id }
      {
        name: name,
        base-value: base-value,
        decay-rate: decay-rate,
        is-organic: is-organic
      }
    )
    (var-set next-resource-id (+ type-id u1))
    (ok type-id)
  )
)

(define-public (mint-resource (type-id uint) (quantity uint))
  (let
    (
      (resource-id (var-get next-resource-id))
      (type-data (unwrap! (get-resource-type type-id) err-not-found))
    )
    (map-set player-resources
      { owner: tx-sender, resource-id: resource-id }
      {
        type-id: type-id,
        quantity: quantity,
        creation-block: block-height,
        last-update-block: block-height,
        quality: u100
      }
    )
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (create-recipe 
  (name (string-ascii 50))
  (req-type-1 uint) (req-qty-1 uint)
  (req-type-2 uint) (req-qty-2 uint)
  (output-quality uint))
  (let
    (
      (recipe-id (var-get next-item-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crafting-recipes
      { recipe-id: recipe-id }
      {
        name: name,
        required-type-1: req-type-1,
        required-qty-1: req-qty-1,
        required-type-2: req-type-2,
        required-qty-2: req-qty-2,
        output-quality: output-quality
      }
    )
    (var-set next-item-id (+ recipe-id u1))
    (ok recipe-id)
  )
)

(define-public (craft-item (recipe-id uint) (resource-1-id uint) (resource-2-id uint))
  (let
    (
      (recipe (unwrap! (get-recipe recipe-id) err-not-found))
      (resource-1 (unwrap! (get-player-resource tx-sender resource-1-id) err-not-found))
      (resource-2 (unwrap! (get-player-resource tx-sender resource-2-id) err-not-found))
      (item-id (var-get next-item-id))
    )
    ;; Verify resource types and quantities
    (asserts! (is-eq (get type-id resource-1) (get required-type-1 recipe)) err-invalid-craft)
    (asserts! (is-eq (get type-id resource-2) (get required-type-2 recipe)) err-invalid-craft)
    (asserts! (>= (get quantity resource-1) (get required-qty-1 recipe)) err-insufficient-resources)
    (asserts! (>= (get quantity resource-2) (get required-qty-2 recipe)) err-insufficient-resources)
    
    ;; Consume resources
    (map-set player-resources
      { owner: tx-sender, resource-id: resource-1-id }
      (merge resource-1 { quantity: (- (get quantity resource-1) (get required-qty-1 recipe)) })
    )
    (map-set player-resources
      { owner: tx-sender, resource-id: resource-2-id }
      (merge resource-2 { quantity: (- (get quantity resource-2) (get required-qty-2 recipe)) })
    )
    
    ;; Create crafted item
    (map-set crafted-items
      { item-id: item-id }
      {
        owner: tx-sender,
        name: (get name recipe),
        creation-block: block-height,
        usage-count: u0,
        quality: (get output-quality recipe),
        preserved: false
      }
    )
    
    (var-set next-item-id (+ item-id u1))
    (ok item-id)
  )
)

(define-public (use-item (item-id uint))
  (let
    (
      (item (unwrap! (get-crafted-item item-id) err-not-found))
    )
    (asserts! (is-eq (get owner item) tx-sender) err-owner-only)
    (map-set crafted-items
      { item-id: item-id }
      (merge item { 
        usage-count: (+ (get usage-count item) u1),
        quality: (if (> (get quality item) u5) (- (get quality item) u5) u0)
      })
    )
    (ok true)
  )
)

(define-public (preserve-item (item-id uint))
  (let
    (
      (item (unwrap! (get-crafted-item item-id) err-not-found))
    )
    (asserts! (is-eq (get owner item) tx-sender) err-owner-only)
    (map-set crafted-items
      { item-id: item-id }
      (merge item { preserved: true })
    )
    (ok true)
  )
)

;; Initialize with default resource types
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (create-resource-type "Iron Ore" u10 u1 false))
    (try! (create-resource-type "Wood" u5 u2 true))
    (try! (create-resource-type "Crystal" u50 u0 false))
    (ok true)
  )
)