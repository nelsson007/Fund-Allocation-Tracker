;; Fund Allocation Tracker Smart Contract
;; This contract manages fund allocations, budgets, and spending tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-allocation-exists (err u104))
(define-constant err-budget-exceeded (err u105))
(define-constant err-invalid-category (err u106))
(define-constant err-already-initialized (err u107))
(define-constant err-category-update-failed (err u108))
(define-constant err-spending-update-failed (err u109))

;; Data Variables
(define-data-var total-fund uint u0)
(define-data-var next-allocation-id uint u1)
(define-data-var contract-active bool true)

;; Data Maps
(define-map allocations
  { allocation-id: uint }
  {
    category: (string-ascii 64),
    allocated-amount: uint,
    spent-amount: uint,
    created-by: principal,
    created-at: uint,
    active: bool
  }
)

(define-map category-budgets
  { category: (string-ascii 64) }
  {
    total-budget: uint,
    allocated: uint,
    spent: uint,
    manager: principal
  }
)

(define-map user-permissions
  { user: principal }
  {
    can-allocate: bool,
    can-spend: bool,
    can-manage-categories: bool
  }
)

(define-map spending-records
  { record-id: uint }
  {
    allocation-id: uint,
    amount: uint,
    description: (string-ascii 256),
    spent-by: principal,
    spent-at: uint
  }
)

(define-data-var next-record-id uint u1)

;; Private Functions
(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (has-permission (user principal) (permission (string-ascii 20)))
  (match (map-get? user-permissions { user: user })
    user-perms
    (if (is-eq permission "allocate")
      (get can-allocate user-perms)
      (if (is-eq permission "spend")
        (get can-spend user-perms)
        (if (is-eq permission "manage")
          (get can-manage-categories user-perms)
          false
        )
      )
    )
    false
  )
)

(define-private (update-category-allocated (category (string-ascii 64)) (amount uint))
  (match (map-get? category-budgets { category: category })
    budget-info
    (begin
      (map-set category-budgets
        { category: category }
        (merge budget-info { allocated: (+ (get allocated budget-info) amount) })
      )
      true
    )
    false
  )
)

(define-private (update-category-spent (category (string-ascii 64)) (amount uint))
  (match (map-get? category-budgets { category: category })
    budget-info
    (begin
      (map-set category-budgets
        { category: category }
        (merge budget-info { spent: (+ (get spent budget-info) amount) })
      )
      true
    )
    false
  )
)

;; Public Functions

;; Initialize contract with total fund
(define-public (initialize-fund (amount uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq (var-get total-fund) u0) err-already-initialized)
    (var-set total-fund amount)
    (ok true)
  )
)

;; Add funds to the contract
(define-public (add-funds (amount uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (var-set total-fund (+ (var-get total-fund) amount))
    (ok (var-get total-fund))
  )
)

;; Set user permissions
(define-public (set-user-permissions (user principal) (can-allocate bool) (can-spend bool) (can-manage bool))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (is-eq user tx-sender)) err-invalid-amount) ;; Cannot modify own permissions
    (ok (map-set user-permissions
      { user: user }
      {
        can-allocate: can-allocate,
        can-spend: can-spend,
        can-manage-categories: can-manage
      }
    ))
  )
)

;; Create a new category budget
(define-public (create-category (category (string-ascii 64)) (budget uint) (manager principal))
  (begin
    (asserts! (or (is-owner) (has-permission tx-sender "manage")) err-owner-only)
    (asserts! (> budget u0) err-invalid-amount)
    (asserts! (> (len category) u0) err-invalid-category) ;; Category name cannot be empty
    (asserts! (not (is-eq manager tx-sender)) err-invalid-amount) ;; Manager cannot be self
    (asserts! (is-none (map-get? category-budgets { category: category })) err-allocation-exists)
    (ok (map-set category-budgets
      { category: category }
      {
        total-budget: budget,
        allocated: u0,
        spent: u0,
        manager: manager
      }
    ))
  )
)

;; Update category budget
(define-public (update-category-budget (category (string-ascii 64)) (new-budget uint))
  (begin
    (asserts! (or (is-owner) (has-permission tx-sender "manage")) err-owner-only)
    (asserts! (> new-budget u0) err-invalid-amount)
    (asserts! (> (len category) u0) err-invalid-category) ;; Category name cannot be empty
    (match (map-get? category-budgets { category: category })
      budget-info
      (begin
        (asserts! (>= new-budget (get allocated budget-info)) err-budget-exceeded)
        (ok (map-set category-budgets
          { category: category }
          (merge budget-info { total-budget: new-budget })
        ))
      )
      err-not-found
    )
  )
)

;; Create a new allocation
(define-public (create-allocation (category (string-ascii 64)) (amount uint))
  (let
    (
      (allocation-id (var-get next-allocation-id))
      (current-fund (var-get total-fund))
    )
    (begin
      (asserts! (or (is-owner) (has-permission tx-sender "allocate")) err-owner-only)
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (>= current-fund amount) err-insufficient-funds)
      
      ;; Check if category exists and has budget available
      (match (map-get? category-budgets { category: category })
        budget-info
        (begin
          (asserts! (<= (+ (get allocated budget-info) amount) (get total-budget budget-info)) err-budget-exceeded)
          
          ;; Create allocation
          (map-set allocations
            { allocation-id: allocation-id }
            {
              category: category,
              allocated-amount: amount,
              spent-amount: u0,
              created-by: tx-sender,
              created-at: stacks-block-height,
              active: true
            }
          )
          
          ;; Update category allocated amount
          (asserts! (update-category-allocated category amount) err-category-update-failed)
          
          ;; Update total fund and next ID
          (var-set total-fund (- current-fund amount))
          (var-set next-allocation-id (+ allocation-id u1))
          
          (ok allocation-id)
        )
        err-invalid-category
      )
    )
  )
)

;; Spend from an allocation
(define-public (spend-from-allocation (allocation-id uint) (amount uint) (description (string-ascii 256)))
  (let
    (
      (record-id (var-get next-record-id))
    )
    (begin
      (asserts! (or (is-owner) (has-permission tx-sender "spend")) err-owner-only)
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (> allocation-id u0) err-not-found) ;; Allocation ID must be valid
      (asserts! (> (len description) u0) err-invalid-amount) ;; Description cannot be empty
      
      (match (map-get? allocations { allocation-id: allocation-id })
        allocation-info
        (begin
          (asserts! (get active allocation-info) err-not-found)
          (asserts! (>= (- (get allocated-amount allocation-info) (get spent-amount allocation-info)) amount) err-insufficient-funds)
          
          ;; Update allocation spent amount
          (map-set allocations
            { allocation-id: allocation-id }
            (merge allocation-info { spent-amount: (+ (get spent-amount allocation-info) amount) })
          )
          
          ;; Update category spent amount
          (asserts! (update-category-spent (get category allocation-info) amount) err-spending-update-failed)
          
          ;; Create spending record
          (map-set spending-records
            { record-id: record-id }
            {
              allocation-id: allocation-id,
              amount: amount,
              description: description,
              spent-by: tx-sender,
              spent-at: stacks-block-height
            }
          )
          
          (var-set next-record-id (+ record-id u1))
          (ok record-id)
        )
        err-not-found
      )
    )
  )
)

;; Deactivate an allocation
(define-public (deactivate-allocation (allocation-id uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (> allocation-id u0) err-not-found) ;; Allocation ID must be valid
    (match (map-get? allocations { allocation-id: allocation-id })
      allocation-info
      (ok (map-set allocations
        { allocation-id: allocation-id }
        (merge allocation-info { active: false })
      ))
      err-not-found
    )
  )
)

;; Read-only functions

;; Get total fund
(define-read-only (get-total-fund)
  (var-get total-fund)
)

;; Get allocation details
(define-read-only (get-allocation (allocation-id uint))
  (map-get? allocations { allocation-id: allocation-id })
)

;; Get category budget
(define-read-only (get-category-budget (category (string-ascii 64)))
  (map-get? category-budgets { category: category })
)

;; Get user permissions
(define-read-only (get-user-permissions (user principal))
  (map-get? user-permissions { user: user })
)

;; Get spending record
(define-read-only (get-spending-record (record-id uint))
  (map-get? spending-records { record-id: record-id })
)

;; Get allocation remaining balance
(define-read-only (get-allocation-balance (allocation-id uint))
  (match (map-get? allocations { allocation-id: allocation-id })
    allocation-info
    (ok (- (get allocated-amount allocation-info) (get spent-amount allocation-info)))
    err-not-found
  )
)

;; Get category remaining budget
(define-read-only (get-category-remaining-budget (category (string-ascii 64)))
  (match (map-get? category-budgets { category: category })
    budget-info
    (ok (- (get total-budget budget-info) (get allocated budget-info)))
    err-not-found
  )
)

;; Get contract status
(define-read-only (get-contract-status)
  {
    owner: contract-owner,
    total-fund: (var-get total-fund),
    next-allocation-id: (var-get next-allocation-id),
    next-record-id: (var-get next-record-id),
    active: (var-get contract-active)
  }
)

;; Emergency functions

;; Pause contract
(define-public (pause-contract)
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set contract-active false)
    (ok true)
  )
)

;; Resume contract
(define-public (resume-contract)
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set contract-active true)
    (ok true)
  )
)