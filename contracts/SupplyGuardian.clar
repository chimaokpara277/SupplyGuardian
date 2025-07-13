;; SupplyGuardian: Decentralized supply chain verification platform
;; Enables manufacturers to register products, distributors to track shipments, and auditors to verify authenticity

(define-data-var chief-auditor principal tx-sender)

(define-map product-catalog
  { product-id: uint }
  {
    manufacturer: principal,
    certification-cost: uint,
    product-name: (string-ascii 50),
    specifications: (string-ascii 500),
    shelf-life-days: uint,
    certified: bool
  }
)

(define-map shipment-trail
  { product-id: uint, trail-id: uint }
  {
    handler: principal,
    timestamp: uint,
    status: (string-ascii 20)
  }
)

(define-data-var next-product-id uint u1)

(define-map trail-counter
  { product-id: uint }
  { entries: uint }
)

;; Register new product in supply chain
(define-public (register-product (name-input (string-ascii 50)) (specs-input (string-ascii 500)) (shelf-life-input uint) (cost-input uint))
  (let
    (
      (product-id (var-get next-product-id))
      (trail-id u0)
      (name name-input)
      (specs specs-input)
      (shelf-life shelf-life-input)
      (cost cost-input)
    )
    ;; Input validation
    (asserts! (> cost u0) (err u1))
    (asserts! (> (len name) u0) (err u5))
    (asserts! (> (len specs) u0) (err u6))
    (asserts! (> shelf-life u0) (err u7))
    
    (map-set product-catalog
      { product-id: product-id }
      {
        manufacturer: tx-sender,
        certification-cost: cost,
        product-name: name,
        specifications: specs,
        shelf-life-days: shelf-life,
        certified: false
      }
    )
    (map-set shipment-trail
      { product-id: product-id, trail-id: trail-id }
      {
        handler: tx-sender,
        timestamp: product-id,
        status: "manufactured"
      }
    )
    (map-set trail-counter
      { product-id: product-id }
      { entries: u1 }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Track product shipment
(define-public (track-shipment (product-id-input uint))
  (let
    (
      (product-id product-id-input)
      (product-info (unwrap! (map-get? product-catalog { product-id: product-id }) (err u2)))
      (cost (get certification-cost product-info))
      (manufacturer (get manufacturer product-info))
      (trail-data (default-to { entries: u0 } (map-get? trail-counter { product-id: product-id })))
      (trail-id (get entries trail-data))
      (new-trail-id (+ trail-id u1))
    )
    ;; Input validation
    (asserts! (> product-id u0) (err u8))
    (asserts! (not (is-eq tx-sender manufacturer)) (err u3))
    
    (try! (stx-transfer? cost tx-sender manufacturer))
    (map-set shipment-trail
      { product-id: product-id, trail-id: trail-id }
      {
        handler: tx-sender,
        timestamp: (var-get next-product-id),
        status: "in-transit"
      }
    )
    (map-set trail-counter
      { product-id: product-id }
      { entries: new-trail-id }
    )
    (ok true)
  )
)

;; Certify product authenticity (auditor only)
(define-public (certify-product (product-id-input uint))
  (let
    (
      (product-id product-id-input)
      (product-info (unwrap! (map-get? product-catalog { product-id: product-id }) (err u2)))
      (trail-data (default-to { entries: u0 } (map-get? trail-counter { product-id: product-id })))
      (trail-id (get entries trail-data))
      (new-trail-id (+ trail-id u1))
    )
    ;; Input validation
    (asserts! (> product-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get chief-auditor)) (err u4))
    
    (map-set product-catalog
      { product-id: product-id }
      (merge product-info { certified: true })
    )
    (map-set shipment-trail
      { product-id: product-id, trail-id: trail-id }
      {
        handler: (get manufacturer product-info),
        timestamp: (var-get next-product-id),
        status: "certified"
      }
    )
    (map-set trail-counter
      { product-id: product-id }
      { entries: new-trail-id }
    )
    (ok true)
  )
)

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? product-catalog { product-id: product-id })
)

;; Get shipment trail entry
(define-read-only (get-shipment-trail (product-id uint) (trail-id uint))
  (map-get? shipment-trail { product-id: product-id, trail-id: trail-id })
)

;; Get total trail entries
(define-read-only (get-trail-count (product-id uint))
  (let
    (
      (trail-data (default-to { entries: u0 } (map-get? trail-counter { product-id: product-id })))
    )
    (get entries trail-data)
  )
)