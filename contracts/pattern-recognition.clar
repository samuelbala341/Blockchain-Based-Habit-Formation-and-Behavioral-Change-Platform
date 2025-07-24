;; Behavioral Pattern Recognition Contract
;; Identifies unconscious patterns that limit personal growth

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-PATTERN-NOT-FOUND (err u301))
(define-constant ERR-INVALID-INPUT (err u302))
(define-constant ERR-INSUFFICIENT-DATA (err u303))

;; Data Variables
(define-data-var next-pattern-id uint u1)
(define-data-var analysis-threshold uint u7) ;; Minimum data points for pattern recognition

;; Data Maps
(define-map behavioral-data
  { user: principal, timestamp: uint }
  {
    activity-type: (string-ascii 50),
    emotional-state: uint, ;; 1-10
    energy-level: uint, ;; 1-10
    context: (string-ascii 100),
    outcome-rating: uint, ;; 1-10
    trigger-identified: bool
  }
)

(define-map identified-patterns
  { user: principal, pattern-id: uint }
  {
    pattern-type: (string-ascii 50), ;; "procrastination", "emotional-eating", etc.
    frequency: uint,
    strength: uint, ;; 1-100
    triggers: (string-ascii 200),
    consequences: (string-ascii 200),
    identified-at: uint,
    disrupted: bool
  }
)

(define-map pattern-tracking
  { user: principal, pattern-id: uint, week: uint }
  {
    occurrences: uint,
    intensity-average: uint,
    disruption-attempts: uint,
    successful-disruptions: uint,
    improvement-score: uint
  }
)

(define-map disruption-strategies
  { user: principal, pattern-id: uint, strategy-id: uint }
  {
    strategy-name: (string-ascii 100),
    effectiveness-rating: uint, ;; 1-10
    usage-count: uint,
    success-rate: uint,
    created-at: uint
  }
)

(define-map pattern-insights
  { user: principal }
  {
    total-patterns-identified: uint,
    patterns-disrupted: uint,
    overall-awareness-score: uint,
    last-analysis: uint,
    growth-trajectory: uint
  }
)

;; Public Functions

;; Record behavioral data point
(define-public (record-behavioral-data (activity-type (string-ascii 50)) (emotional-state uint) (energy-level uint) (context (string-ascii 100)) (outcome-rating uint) (trigger-identified bool))
  (begin
    (asserts! (> (len activity-type) u0) ERR-INVALID-INPUT)
    (asserts! (and (>= emotional-state u1) (<= emotional-state u10)) ERR-INVALID-INPUT)
    (asserts! (and (>= energy-level u1) (<= energy-level u10)) ERR-INVALID-INPUT)
    (asserts! (and (>= outcome-rating u1) (<= outcome-rating u10)) ERR-INVALID-INPUT)

    (map-set behavioral-data
      { user: tx-sender, timestamp: block-height }
      {
        activity-type: activity-type,
        emotional-state: emotional-state,
        energy-level: energy-level,
        context: context,
        outcome-rating: outcome-rating,
        trigger-identified: trigger-identified
      }
    )

    ;; Check if enough data exists to analyze patterns
    (if (>= (get-data-point-count tx-sender) (var-get analysis-threshold))
      (analyze-patterns tx-sender)
      (ok true)
    )
  )
)

;; Manually trigger pattern analysis
(define-public (analyze-patterns (user principal))
  (let
    ((data-count (get-data-point-count user))
     (pattern-id (var-get next-pattern-id)))

    (begin
      (asserts! (>= data-count (var-get analysis-threshold)) ERR-INSUFFICIENT-DATA)

      ;; Simplified pattern recognition (in real implementation would be more sophisticated)
      (let
        ((pattern-strength (calculate-pattern-strength user))
         (pattern-type (identify-dominant-pattern-type user)))

        (if (> pattern-strength u30) ;; Threshold for pattern significance
          (begin
            (map-set identified-patterns
              { user: user, pattern-id: pattern-id }
              {
                pattern-type: pattern-type,
                frequency: (calculate-pattern-frequency user),
                strength: pattern-strength,
                triggers: (identify-common-triggers user),
                consequences: (analyze-consequences user),
                identified-at: block-height,
                disrupted: false
              }
            )

            (var-set next-pattern-id (+ pattern-id u1))
            (update-pattern-insights user)
            (ok true)
          )
          (ok false)
        )
      )
    )
  )
)

;; Record pattern disruption attempt
(define-public (record-disruption-attempt (pattern-id uint) (strategy-name (string-ascii 100)) (successful bool))
  (let
    ((pattern (unwrap! (map-get? identified-patterns { user: tx-sender, pattern-id: pattern-id }) ERR-PATTERN-NOT-FOUND))
     (current-week (/ block-height u1008))) ;; Approximate weekly blocks

    (begin
      (asserts! (> (len strategy-name) u0) ERR-INVALID-INPUT)

      ;; Update weekly tracking
      (let
        ((current-tracking (default-to
          { occurrences: u0, intensity-average: u0, disruption-attempts: u0, successful-disruptions: u0, improvement-score: u0 }
          (map-get? pattern-tracking { user: tx-sender, pattern-id: pattern-id, week: current-week }))))

        (map-set pattern-tracking
          { user: tx-sender, pattern-id: pattern-id, week: current-week }
          {
            occurrences: (get occurrences current-tracking),
            intensity-average: (get intensity-average current-tracking),
            disruption-attempts: (+ (get disruption-attempts current-tracking) u1),
            successful-disruptions: (if successful
              (+ (get successful-disruptions current-tracking) u1)
              (get successful-disruptions current-tracking)),
            improvement-score: (calculate-improvement-score
              (+ (get disruption-attempts current-tracking) u1)
              (if successful
                (+ (get successful-disruptions current-tracking) u1)
                (get successful-disruptions current-tracking)))
          }
        )
      )

      ;; Update strategy effectiveness
      (update-strategy-effectiveness tx-sender pattern-id strategy-name successful)

      (ok true)
    )
  )
)

;; Create disruption strategy
(define-public (create-disruption-strategy (pattern-id uint) (strategy-name (string-ascii 100)))
  (let
    ((pattern (unwrap! (map-get? identified-patterns { user: tx-sender, pattern-id: pattern-id }) ERR-PATTERN-NOT-FOUND))
     (strategy-id (get-next-strategy-id tx-sender pattern-id)))

    (begin
      (asserts! (> (len strategy-name) u0) ERR-INVALID-INPUT)

      (map-set disruption-strategies
        { user: tx-sender, pattern-id: pattern-id, strategy-id: strategy-id }
        {
          strategy-name: strategy-name,
          effectiveness-rating: u5, ;; Neutral starting point
          usage-count: u0,
          success-rate: u0,
          created-at: block-height
        }
      )

      (ok strategy-id)
    )
  )
)

;; Private Functions

(define-private (update-strategy-effectiveness (user principal) (pattern-id uint) (strategy-name (string-ascii 100)) (successful bool))
  (let
    ((strategy-id (get-strategy-id-by-name user pattern-id strategy-name)))
    (match strategy-id
      id (begin
        (let
          ((existing-strategy (unwrap-panic (map-get? disruption-strategies { user: user, pattern-id: pattern-id, strategy-id: id }))))
          (map-set disruption-strategies
            { user: user, pattern-id: pattern-id, strategy-id: id }
            {
              strategy-name: (get strategy-name existing-strategy),
              effectiveness-rating: (if successful
                (if (<= (+ (get effectiveness-rating existing-strategy) u1) u10)
                  (+ (get effectiveness-rating existing-strategy) u1)
                  u10)
                (if (>= (- (get effectiveness-rating existing-strategy) u1) u1)
                  (- (get effectiveness-rating existing-strategy) u1)
                  u1)),
              usage-count: (+ (get usage-count existing-strategy) u1),
              success-rate: (calculate-success-rate
                (+ (get usage-count existing-strategy) u1)
                (if successful u1 u0)),
              created-at: (get created-at existing-strategy)
            }
          )
        )
        true
      )
      false
    )
  )
)

(define-private (update-pattern-insights (user principal))
  (let
    ((current-insights (default-to
      { total-patterns-identified: u0, patterns-disrupted: u0, overall-awareness-score: u0, last-analysis: u0, growth-trajectory: u0 }
      (map-get? pattern-insights { user: user }))))

    (map-set pattern-insights
      { user: user }
      {
        total-patterns-identified: (+ (get total-patterns-identified current-insights) u1),
        patterns-disrupted: (get patterns-disrupted current-insights),
        overall-awareness-score: (calculate-awareness-score user),
        last-analysis: block-height,
        growth-trajectory: (calculate-growth-trajectory user)
      }
    )
  )
)

;; Read-only functions

(define-read-only (get-behavioral-data (user principal) (timestamp uint))
  (map-get? behavioral-data { user: user, timestamp: timestamp })
)

(define-read-only (get-identified-pattern (user principal) (pattern-id uint))
  (map-get? identified-patterns { user: user, pattern-id: pattern-id })
)

(define-read-only (get-pattern-tracking (user principal) (pattern-id uint) (week uint))
  (map-get? pattern-tracking { user: user, pattern-id: pattern-id, week: week })
)

(define-read-only (get-pattern-insights (user principal))
  (map-get? pattern-insights { user: user })
)

(define-read-only (get-data-point-count (user principal))
  ;; Simplified - in real implementation would count actual data points
  u10
)

(define-read-only (calculate-pattern-strength (user principal))
  ;; Simplified pattern strength calculation
  u75
)

(define-read-only (identify-dominant-pattern-type (user principal))
  "procrastination"
)

(define-read-only (calculate-pattern-frequency (user principal))
  u5
)

(define-read-only (identify-common-triggers (user principal))
  "stress, fatigue, social situations"
)

(define-read-only (analyze-consequences (user principal))
  "decreased productivity, negative emotions"
)

(define-read-only (calculate-improvement-score (attempts uint) (successes uint))
  (if (> attempts u0)
    (/ (* successes u100) attempts)
    u0
  )
)

(define-read-only (calculate-success-rate (total-usage uint) (new-success uint))
  (if (> total-usage u0)
    (/ (* new-success u100) total-usage)
    u0
  )
)

(define-read-only (calculate-awareness-score (user principal))
  u80
)

(define-read-only (calculate-growth-trajectory (user principal))
  u75
)

(define-read-only (get-next-strategy-id (user principal) (pattern-id uint))
  u1
)

(define-read-only (get-strategy-id-by-name (user principal) (pattern-id uint) (strategy-name (string-ascii 100)))
  (some u1)
)
