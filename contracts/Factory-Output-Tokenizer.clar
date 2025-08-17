(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PARAMS (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_FACTORY_NOT_ACTIVE (err u105))
(define-constant ERR_OUTPUT_ALREADY_TOKENIZED (err u106))
(define-constant ERR_TOKEN_NOT_FOUND (err u107))
(define-constant ERR_NOT_TOKEN_OWNER (err u108))

(define-data-var next-factory-id uint u1)
(define-data-var next-output-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var contract-paused bool false)

(define-map factories
    uint
    {
        name: (string-ascii 64),
        owner: principal,
        production-rate: uint,
        cost-per-output: uint,
        active: bool,
        created-at: uint,
        total-outputs: uint,
    }
)

(define-map factory-outputs
    uint
    {
        factory-id: uint,
        output-type: (string-ascii 32),
        quality-score: uint,
        production-cost: uint,
        created-at: uint,
        tokenized: bool,
    }
)

(define-map output-tokens
    uint
    {
        output-id: uint,
        owner: principal,
        metadata-uri: (optional (string-ascii 256)),
        minted-at: uint,
        last-transfer: uint,
    }
)

(define-map factory-owners
    principal
    (list 20 uint)
)

(define-map token-owners
    principal
    (list 50 uint)
)

(define-map factory-balances
    uint
    uint
)

(define-map user-stats
    principal
    {
        factories-created: uint,
        outputs-produced: uint,
        tokens-minted: uint,
        total-spent: uint,
        total-earned: uint,
    }
)

(define-read-only (get-contract-info)
    {
        total-factories: (- (var-get next-factory-id) u1),
        total-outputs: (- (var-get next-output-id) u1),
        total-tokens: (- (var-get next-token-id) u1),
        contract-owner: CONTRACT_OWNER,
        paused: (var-get contract-paused),
    }
)

(define-read-only (get-factory (factory-id uint))
    (map-get? factories factory-id)
)

(define-read-only (get-output (output-id uint))
    (map-get? factory-outputs output-id)
)

(define-read-only (get-token (token-id uint))
    (map-get? output-tokens token-id)
)

(define-read-only (get-factory-balance (factory-id uint))
    (default-to u0 (map-get? factory-balances factory-id))
)

(define-read-only (get-user-factories (user principal))
    (default-to (list) (map-get? factory-owners user))
)

(define-read-only (get-user-tokens (user principal))
    (default-to (list) (map-get? token-owners user))
)

(define-read-only (get-user-stats (user principal))
    (default-to {
        factories-created: u0,
        outputs-produced: u0,
        tokens-minted: u0,
        total-spent: u0,
        total-earned: u0,
    }
        (map-get? user-stats user)
    )
)

(define-private (update-user-stats
        (user principal)
        (field (string-ascii 20))
        (amount uint)
    )
    (let ((current-stats (get-user-stats user)))
        (map-set user-stats user
            (if (is-eq field "factories-created")
                (merge current-stats { factories-created: (+ (get factories-created current-stats) amount) })
                (if (is-eq field "outputs-produced")
                    (merge current-stats { outputs-produced: (+ (get outputs-produced current-stats) amount) })
                    (if (is-eq field "tokens-minted")
                        (merge current-stats { tokens-minted: (+ (get tokens-minted current-stats) amount) })
                        (if (is-eq field "total-spent")
                            (merge current-stats { total-spent: (+ (get total-spent current-stats) amount) })
                            (merge current-stats { total-earned: (+ (get total-earned current-stats) amount) })
                        )
                    )
                )
            ))
    )
)

(define-private (add-factory-to-owner
        (owner principal)
        (factory-id uint)
    )
    (let ((current-factories (get-user-factories owner)))
        (map-set factory-owners owner
            (unwrap! (as-max-len? (append current-factories factory-id) u20)
                false
            ))
    )
)

(define-private (add-token-to-owner
        (owner principal)
        (token-id uint)
    )
    (let ((current-tokens (get-user-tokens owner)))
        (map-set token-owners owner
            (unwrap! (as-max-len? (append current-tokens token-id) u50) false)
        )
    )
)

(define-private (remove-token-from-owner
        (owner principal)
        (target-token-id uint)
    )
    (map-set token-owners owner (list))
)

(define-public (create-factory
        (name (string-ascii 64))
        (production-rate uint)
        (cost-per-output uint)
    )
    (let ((factory-id (var-get next-factory-id)))
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (> (len name) u0) ERR_INVALID_PARAMS)
        (asserts! (> production-rate u0) ERR_INVALID_PARAMS)
        (asserts! (> cost-per-output u0) ERR_INVALID_PARAMS)

        (map-set factories factory-id {
            name: name,
            owner: tx-sender,
            production-rate: production-rate,
            cost-per-output: cost-per-output,
            active: true,
            created-at: stacks-block-height,
            total-outputs: u0,
        })

        (add-factory-to-owner tx-sender factory-id)
        (update-user-stats tx-sender "factories-created" u1)
        (var-set next-factory-id (+ factory-id u1))
        (ok factory-id)
    )
)

(define-public (toggle-factory-status (factory-id uint))
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)

        (map-set factories factory-id
            (merge factory { active: (not (get active factory)) })
        )
        (ok (not (get active factory)))
    )
)

(define-public (produce-output
        (factory-id uint)
        (output-type (string-ascii 32))
        (quality-score uint)
    )
    (let (
            (factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND))
            (output-id (var-get next-output-id))
            (production-cost (get cost-per-output factory))
        )
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (get active factory) ERR_FACTORY_NOT_ACTIVE)
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)
        (asserts! (> (len output-type) u0) ERR_INVALID_PARAMS)
        (asserts! (<= quality-score u100) ERR_INVALID_PARAMS)

        (let ((factory-balance (get-factory-balance factory-id)))
            (asserts! (>= factory-balance production-cost)
                ERR_INSUFFICIENT_BALANCE
            )
            (map-set factory-balances factory-id
                (- factory-balance production-cost)
            )
        )

        (map-set factory-outputs output-id {
            factory-id: factory-id,
            output-type: output-type,
            quality-score: quality-score,
            production-cost: production-cost,
            created-at: stacks-block-height,
            tokenized: false,
        })

        (map-set factories factory-id
            (merge factory { total-outputs: (+ (get total-outputs factory) u1) })
        )

        (update-user-stats tx-sender "outputs-produced" u1)
        (update-user-stats tx-sender "total-spent" production-cost)
        (var-set next-output-id (+ output-id u1))
        (ok output-id)
    )
)

(define-public (fund-factory
        (factory-id uint)
        (amount uint)
    )
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_PARAMS)

        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (let ((current-balance (get-factory-balance factory-id)))
            (map-set factory-balances factory-id (+ current-balance amount))
        )
        (ok true)
    )
)

(define-public (tokenize-output
        (output-id uint)
        (metadata-uri (optional (string-ascii 256)))
    )
    (let (
            (output (unwrap! (map-get? factory-outputs output-id) ERR_NOT_FOUND))
            (factory (unwrap! (map-get? factories (get factory-id output)) ERR_NOT_FOUND))
            (token-id (var-get next-token-id))
        )
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)
        (asserts! (not (get tokenized output)) ERR_OUTPUT_ALREADY_TOKENIZED)

        (map-set output-tokens token-id {
            output-id: output-id,
            owner: tx-sender,
            metadata-uri: metadata-uri,
            minted-at: stacks-block-height,
            last-transfer: stacks-block-height,
        })

        (map-set factory-outputs output-id (merge output { tokenized: true }))

        (add-token-to-owner tx-sender token-id)
        (update-user-stats tx-sender "tokens-minted" u1)
        (var-set next-token-id (+ token-id u1))
        (ok token-id)
    )
)

(define-public (transfer-token
        (token-id uint)
        (recipient principal)
    )
    (let ((token (unwrap! (map-get? output-tokens token-id) ERR_TOKEN_NOT_FOUND)))
        (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
        (asserts! (is-eq tx-sender (get owner token)) ERR_NOT_TOKEN_OWNER)
        (asserts! (not (is-eq tx-sender recipient)) ERR_INVALID_PARAMS)

        (map-set output-tokens token-id
            (merge token {
                owner: recipient,
                last-transfer: stacks-block-height,
            })
        )

        (remove-token-from-owner tx-sender token-id)
        (add-token-to-owner recipient token-id)
        (ok true)
    )
)

(define-public (set-token-metadata
        (token-id uint)
        (metadata-uri (string-ascii 256))
    )
    (let ((token (unwrap! (map-get? output-tokens token-id) ERR_TOKEN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get owner token)) ERR_NOT_TOKEN_OWNER)

        (map-set output-tokens token-id
            (merge token { metadata-uri: (some metadata-uri) })
        )
        (ok true)
    )
)

(define-public (withdraw-factory-funds
        (factory-id uint)
        (amount uint)
    )
    (let (
            (factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND))
            (current-balance (get-factory-balance factory-id))
        )
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)
        (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
        (asserts! (> amount u0) ERR_INVALID_PARAMS)

        (try! (as-contract (stx-transfer? amount tx-sender (get owner factory))))
        (map-set factory-balances factory-id (- current-balance amount))
        (update-user-stats tx-sender "total-earned" amount)
        (ok true)
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

(define-read-only (get-factory-outputs (factory-id uint))
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (ok (get total-outputs factory))
    )
)

(define-read-only (get-output-efficiency (output-id uint))
    (let ((output (unwrap! (map-get? factory-outputs output-id) ERR_NOT_FOUND)))
        (ok (/ (* (get quality-score output) u100) (get production-cost output)))
    )
)

(define-read-only (get-factory-performance (factory-id uint))
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (ok {
            total-outputs: (get total-outputs factory),
            production-rate: (get production-rate factory),
            efficiency-rating: (/ (get total-outputs factory)
                (+ (- stacks-block-height (get created-at factory)) u1)
            ),
            profitability: (get-factory-balance factory-id),
        })
    )
)

(define-public (upgrade-factory
        (factory-id uint)
        (upgrade-cost uint)
    )
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)
        (asserts! (> upgrade-cost u0) ERR_INVALID_PARAMS)

        (let ((current-balance (get-factory-balance factory-id)))
            (asserts! (>= current-balance upgrade-cost) ERR_INSUFFICIENT_BALANCE)
            (map-set factory-balances factory-id (- current-balance upgrade-cost))
        )

        (map-set factories factory-id
            (merge factory { production-rate: (+ (get production-rate factory) (/ upgrade-cost u100)) })
        )
        (ok true)
    )
)

(define-public (emergency-withdraw)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender CONTRACT_OWNER)))
        (ok true)
    )
)

(define-read-only (get-token-history (token-id uint))
    (let ((token (unwrap! (map-get? output-tokens token-id) ERR_TOKEN_NOT_FOUND)))
        (ok {
            token-id: token-id,
            output-id: (get output-id token),
            current-owner: (get owner token),
            minted-at: (get minted-at token),
            last-transfer: (get last-transfer token),
            metadata-uri: (get metadata-uri token),
        })
    )
)

(define-read-only (calculate-token-value (token-id uint))
    (let (
            (token (unwrap! (map-get? output-tokens token-id) ERR_TOKEN_NOT_FOUND))
            (output (unwrap! (map-get? factory-outputs (get output-id token))
                ERR_NOT_FOUND
            ))
        )
        (ok (* (get quality-score output) (get production-cost output)))
    )
)

(define-public (update-factory-settings
        (factory-id uint)
        (new-rate uint)
        (new-cost uint)
    )
    (let ((factory (unwrap! (map-get? factories factory-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get owner factory)) ERR_UNAUTHORIZED)
        (asserts! (> new-rate u0) ERR_INVALID_PARAMS)
        (asserts! (> new-cost u0) ERR_INVALID_PARAMS)

        (map-set factories factory-id
            (merge factory {
                production-rate: new-rate,
                cost-per-output: new-cost,
            })
        )
        (ok true)
    )
)

(define-read-only (get-marketplace-data)
    (ok {
        total-factories: (- (var-get next-factory-id) u1),
        total-outputs: (- (var-get next-output-id) u1),
        total-tokens: (- (var-get next-token-id) u1),
    })
)
