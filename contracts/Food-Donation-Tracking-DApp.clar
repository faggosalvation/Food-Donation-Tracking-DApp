;; Food Donation Tracking Smart Contract

(define-data-var admin principal tx-sender)

(define-map basic-donations
    { donation-id: uint }
    {
        donor: principal,
        recipient: principal,
        amount: uint,
        status: (string-ascii 20),
        timestamp: uint,
        verified: bool,
    }
)

(define-map donor-stats
    { donor: principal }
    {
        total-donations: uint,
        verified-donations: uint,
    }
)

(define-map recipient-stats
    { recipient: principal }
    {
        received-donations: uint,
        verified-receipts: uint,
    }
)

(define-data-var donation-counter uint u0)

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-DONATION-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VERIFIED (err u103))

(define-public (initialize-contract)
    (begin
        (var-set admin tx-sender)
        (ok true)
    )
)

(define-public (create-donation
        (recipient principal)
        (amount uint)
    )
    (let (
            (donation-id (+ (var-get donation-counter) u1))
            (donor-key { donor: tx-sender })
            (recipient-key { recipient: recipient })
            (current-donor-stats (default-to {
                total-donations: u0,
                verified-donations: u0,
            }
                (map-get? donor-stats donor-key)
            ))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (map-set basic-donations { donation-id: donation-id } {
            donor: tx-sender,
            recipient: recipient,
            amount: amount,
            status: "pending",
            timestamp: burn-block-height,
            verified: false,
        })
        (map-set donor-stats donor-key {
            total-donations: (+ (get total-donations current-donor-stats) u1),
            verified-donations: (get verified-donations current-donor-stats),
        })
        (var-set donation-counter donation-id)
        (ok donation-id)
    )
)
(define-public (verify-donation (donation-id uint))
    (let (
            (donation (unwrap! (map-get? basic-donations { donation-id: donation-id })
                ERR-DONATION-NOT-FOUND
            ))
            (recipient-key { recipient: (get recipient donation) })
            (donor-key { donor: (get donor donation) })
        )
        (asserts! (is-eq tx-sender (get recipient donation)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified donation)) ERR-ALREADY-VERIFIED)
        (map-set basic-donations { donation-id: donation-id }
            (merge donation {
                status: "verified",
                verified: true,
            })
        )
        (update-stats recipient-key donor-key)
        (ok true)
    )
)

(define-private (update-stats
        (recipient-key { recipient: principal })
        (donor-key { donor: principal })
    )
    (let (
            (current-recipient-stats (default-to {
                received-donations: u0,
                verified-receipts: u0,
            }
                (map-get? recipient-stats recipient-key)
            ))
            (current-donor-stats (default-to {
                total-donations: u0,
                verified-donations: u0,
            }
                (map-get? donor-stats donor-key)
            ))
        )
        (map-set recipient-stats recipient-key {
            received-donations: (get received-donations current-recipient-stats),
            verified-receipts: (+ (get verified-receipts current-recipient-stats) u1),
        })
        (map-set donor-stats donor-key {
            total-donations: (get total-donations current-donor-stats),
            verified-donations: (+ (get verified-donations current-donor-stats) u1),
        })
    )
)

(define-read-only (get-basic-donation (donation-id uint))
    (map-get? basic-donations { donation-id: donation-id })
)

(define-read-only (get-donor-stats (donor principal))
    (map-get? donor-stats { donor: donor })
)

(define-read-only (get-recipient-stats (recipient principal))
    (map-get? recipient-stats { recipient: recipient })
)

(define-public (update-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-EXPIRED (err u405))
(define-constant ERR-ALREADY-CLAIMED (err u406))

(define-data-var next-donation-id uint u1)

(define-map expiring-donations
    { donation-id: uint }
    {
        donor: principal,
        original-recipient: principal,
        amount: uint,
        created-at: uint,
        expiry-block: uint,
        status: (string-ascii 20),
        claimed-by: (optional principal),
    }
)

(define-map alternative-recipients
    {
        donation-id: uint,
        recipient-index: uint,
    }
    { recipient: principal }
)

(define-public (create-expiring-donation
        (recipient principal)
        (amount uint)
        (expiry-blocks uint)
        (alternatives (list 5 principal))
    )
    (let (
            (donation-id (var-get next-donation-id))
            (current-block burn-block-height)
            (expiry-block (+ current-block expiry-blocks))
        )
        (asserts! (> amount u0) (err u400))
        (asserts! (> expiry-blocks u0) (err u401))
        (map-set expiring-donations { donation-id: donation-id } {
            donor: tx-sender,
            original-recipient: recipient,
            amount: amount,
            created-at: current-block,
            expiry-block: expiry-block,
            status: "active",
            claimed-by: none,
        })
        (set-alternative-recipients donation-id alternatives)
        (var-set next-donation-id (+ donation-id u1))
        (ok donation-id)
    )
)

(define-private (set-alternative-recipients
        (donation-id uint)
        (alternatives (list 5 principal))
    )
    (fold add-alternative-recipient alternatives {
        donation-id: donation-id,
        index: u0,
    })
)

(define-private (add-alternative-recipient
        (recipient principal)
        (data {
            donation-id: uint,
            index: uint,
        })
    )
    (begin
        (map-set alternative-recipients {
            donation-id: (get donation-id data),
            recipient-index: (get index data),
        } { recipient: recipient }
        )
        {
            donation-id: (get donation-id data),
            index: (+ (get index data) u1),
        }
    )
)

(define-public (claim-donation (donation-id uint))
    (let (
            (donation-data (unwrap! (map-get? expiring-donations { donation-id: donation-id })
                ERR-NOT-FOUND
            ))
            (current-block burn-block-height)
        )
        (asserts! (is-eq (get status donation-data) "active") ERR-ALREADY-CLAIMED)
        (asserts! (< current-block (get expiry-block donation-data)) ERR-EXPIRED)
        (asserts! (is-eq tx-sender (get original-recipient donation-data))
            ERR-NOT-AUTHORIZED
        )
        (map-set expiring-donations { donation-id: donation-id }
            (merge donation-data {
                status: "claimed",
                claimed-by: (some tx-sender),
            })
        )
        (ok true)
    )
)

(define-public (redistribute-expired-donation
        (donation-id uint)
        (recipient-index uint)
    )
    (let (
            (donation-data (unwrap! (map-get? expiring-donations { donation-id: donation-id })
                ERR-NOT-FOUND
            ))
            (alternative-data (unwrap!
                (map-get? alternative-recipients {
                    donation-id: donation-id,
                    recipient-index: recipient-index,
                })
                ERR-NOT-FOUND
            ))
            (current-block burn-block-height)
        )
        (asserts! (is-eq (get status donation-data) "active") ERR-ALREADY-CLAIMED)
        (asserts! (>= current-block (get expiry-block donation-data)) (err u402))
        (asserts! (is-eq tx-sender (get recipient alternative-data))
            ERR-NOT-AUTHORIZED
        )
        (map-set expiring-donations { donation-id: donation-id }
            (merge donation-data {
                status: "redistributed",
                claimed-by: (some tx-sender),
            })
        )
        (ok true)
    )
)

(define-read-only (get-expiring-donation (donation-id uint))
    (map-get? expiring-donations { donation-id: donation-id })
)

(define-read-only (get-alternative-recipient
        (donation-id uint)
        (recipient-index uint)
    )
    (map-get? alternative-recipients {
        donation-id: donation-id,
        recipient-index: recipient-index,
    })
)
(define-constant ERR-ALREADY-RATED (err u408))
(define-constant ERR-INVALID-RATING (err u409))
(define-constant ERR-CANNOT-RATE-SELF (err u410))

(define-data-var next-rating-id uint u1)

(define-map user-reputation
    { user: principal }
    {
        total-donations: uint,
        total-received: uint,
        average-rating: uint,
        total-ratings: uint,
        reputation-score: uint,
    }
)

(define-map donation-ratings
    { donation-id: uint }
    {
        donor-rating: (optional uint),
        recipient-rating: (optional uint),
        donor-feedback: (optional (string-ascii 200)),
        recipient-feedback: (optional (string-ascii 200)),
        rated-by-donor: bool,
        rated-by-recipient: bool,
    }
)

(define-map individual-ratings
    { rating-id: uint }
    {
        rater: principal,
        rated-user: principal,
        donation-id: uint,
        rating: uint,
        feedback: (string-ascii 200),
        timestamp: uint,
    }
)

(define-public (initialize-donation-rating
        (donation-id uint)
        (donor principal)
        (recipient principal)
    )
    (begin
        (map-set donation-ratings { donation-id: donation-id } {
            donor-rating: none,
            recipient-rating: none,
            donor-feedback: none,
            recipient-feedback: none,
            rated-by-donor: false,
            rated-by-recipient: false,
        })
        (ok true)
    )
)

(define-public (rate-user
        (donation-id uint)
        (rated-user principal)
        (rating uint)
        (feedback (string-ascii 200))
    )
    (let (
            (rating-data (unwrap! (map-get? donation-ratings { donation-id: donation-id })
                ERR-NOT-FOUND
            ))
            (rating-id (var-get next-rating-id))
        )
        (asserts! (not (is-eq tx-sender rated-user)) ERR-CANNOT-RATE-SELF)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (let (
                (is-donor-rating (not (get rated-by-donor rating-data)))
                (is-recipient-rating (not (get rated-by-recipient rating-data)))
            )
            (asserts! (or is-donor-rating is-recipient-rating) ERR-ALREADY-RATED)
            (map-set individual-ratings { rating-id: rating-id } {
                rater: tx-sender,
                rated-user: rated-user,
                donation-id: donation-id,
                rating: rating,
                feedback: feedback,
                timestamp: burn-block-height,
            })
            (if is-donor-rating
                (map-set donation-ratings { donation-id: donation-id }
                    (merge rating-data {
                        donor-rating: (some rating),
                        donor-feedback: (some feedback),
                        rated-by-donor: true,
                    })
                )
                (map-set donation-ratings { donation-id: donation-id }
                    (merge rating-data {
                        recipient-rating: (some rating),
                        recipient-feedback: (some feedback),
                        rated-by-recipient: true,
                    })
                )
            )
            (update-user-reputation rated-user rating)
            (var-set next-rating-id (+ rating-id u1))
            (ok rating-id)
        )
    )
)

(define-private (update-user-reputation
        (user principal)
        (new-rating uint)
    )
    (let (
            (current-rep (default-to {
                total-donations: u0,
                total-received: u0,
                average-rating: u0,
                total-ratings: u0,
                reputation-score: u0,
            }
                (map-get? user-reputation { user: user })
            ))
            (new-total-ratings (+ (get total-ratings current-rep) u1))
            (new-average (/
                (+
                    (* (get average-rating current-rep)
                        (get total-ratings current-rep)
                    )
                    new-rating
                )
                new-total-ratings
            ))
            (new-reputation-score (calculate-reputation-score new-average new-total-ratings))
        )
        (map-set user-reputation { user: user }
            (merge current-rep {
                average-rating: new-average,
                total-ratings: new-total-ratings,
                reputation-score: new-reputation-score,
            })
        )
    )
)

(define-private (calculate-reputation-score
        (average-rating uint)
        (total-ratings uint)
    )
    (let (
            (base-score (* average-rating u20))
            (volume-bonus (if (> total-ratings u10)
                u10
                total-ratings
            ))
        )
        (+ base-score volume-bonus)
    )
)

(define-public (update-donation-count
        (user principal)
        (is-donor bool)
    )
    (let ((current-rep (default-to {
            total-donations: u0,
            total-received: u0,
            average-rating: u0,
            total-ratings: u0,
            reputation-score: u0,
        }
            (map-get? user-reputation { user: user })
        )))
        (if is-donor
            (map-set user-reputation { user: user }
                (merge current-rep { total-donations: (+ (get total-donations current-rep) u1) })
            )
            (map-set user-reputation { user: user }
                (merge current-rep { total-received: (+ (get total-received current-rep) u1) })
            )
        )
        (ok true)
    )
)

(define-read-only (get-user-reputation (user principal))
    (map-get? user-reputation { user: user })
)

(define-read-only (get-donation-ratings (donation-id uint))
    (map-get? donation-ratings { donation-id: donation-id })
)

(define-read-only (get-individual-rating (rating-id uint))
    (map-get? individual-ratings { rating-id: rating-id })
)

(define-read-only (get-reputation-score (user principal))
    (match (map-get? user-reputation { user: user })
        reputation (ok (get reputation-score reputation))
        ERR-NOT-FOUND
    )
)

(define-constant ERR-MILESTONE-NOT-FOUND (err u411))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u412))
(define-constant ERR-ALL-MILESTONES-NOT-COMPLETED (err u413))
(define-constant ERR-FUNDS-ALREADY-RELEASED (err u414))

(define-data-var next-milestone-donation-id uint u1)

(define-map milestone-donations
    { milestone-donation-id: uint }
    {
        donor: principal,
        recipient: principal,
        total-amount: uint,
        released-amount: uint,
        milestone-count: uint,
        completed-milestones: uint,
        created-at: uint,
        status: (string-ascii 20),
    }
)

(define-map donation-milestones
    {
        milestone-donation-id: uint,
        milestone-index: uint,
    }
    {
        description: (string-ascii 100),
        amount: uint,
        completed: bool,
        completed-by: (optional principal),
        approved-by-donor: bool,
        completion-timestamp: (optional uint),
        approval-timestamp: (optional uint),
    }
)

(define-public (create-milestone-donation
        (recipient principal)
        (milestone-descriptions (list 5 (string-ascii 100)))
        (milestone-amounts (list 5 uint))
    )
    (let (
            (milestone-donation-id (var-get next-milestone-donation-id))
            (total-amount (fold + milestone-amounts u0))
            (milestone-count (len milestone-descriptions))
        )
        (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts))
            (err u415)
        )
        (asserts! (> milestone-count u0) (err u416))
        (map-set milestone-donations { milestone-donation-id: milestone-donation-id } {
            donor: tx-sender,
            recipient: recipient,
            total-amount: total-amount,
            released-amount: u0,
            milestone-count: milestone-count,
            completed-milestones: u0,
            created-at: burn-block-height,
            status: "active",
        })
        (fold setup-milestone-with-index milestone-descriptions {
            milestone-donation-id: milestone-donation-id,
            index: u0,
            amounts: milestone-amounts,
        })
        (var-set next-milestone-donation-id (+ milestone-donation-id u1))
        (ok milestone-donation-id)
    )
)

(define-private (setup-milestone-with-index
        (description (string-ascii 100))
        (context {
            milestone-donation-id: uint,
            index: uint,
            amounts: (list 5 uint),
        })
    )
    (let ((amount (default-to u0 (element-at (get amounts context) (get index context)))))
        (map-set donation-milestones {
            milestone-donation-id: (get milestone-donation-id context),
            milestone-index: (get index context),
        } {
            description: description,
            amount: amount,
            completed: false,
            completed-by: none,
            approved-by-donor: false,
            completion-timestamp: none,
            approval-timestamp: none,
        })
        {
            milestone-donation-id: (get milestone-donation-id context),
            index: (+ (get index context) u1),
            amounts: (get amounts context),
        }
    )
)

(define-public (complete-milestone
        (milestone-donation-id uint)
        (milestone-index uint)
    )
    (let (
            (donation-data (unwrap!
                (map-get? milestone-donations { milestone-donation-id: milestone-donation-id })
                ERR-NOT-FOUND
            ))
            (milestone-data (unwrap!
                (map-get? donation-milestones {
                    milestone-donation-id: milestone-donation-id,
                    milestone-index: milestone-index,
                })
                ERR-MILESTONE-NOT-FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get recipient donation-data))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (not (get completed milestone-data))
            ERR-MILESTONE-ALREADY-COMPLETED
        )
        (map-set donation-milestones {
            milestone-donation-id: milestone-donation-id,
            milestone-index: milestone-index,
        }
            (merge milestone-data {
                completed: true,
                completed-by: (some tx-sender),
                completion-timestamp: (some burn-block-height),
            })
        )
        (ok true)
    )
)

(define-public (approve-milestone
        (milestone-donation-id uint)
        (milestone-index uint)
    )
    (let (
            (donation-data (unwrap!
                (map-get? milestone-donations { milestone-donation-id: milestone-donation-id })
                ERR-NOT-FOUND
            ))
            (milestone-data (unwrap!
                (map-get? donation-milestones {
                    milestone-donation-id: milestone-donation-id,
                    milestone-index: milestone-index,
                })
                ERR-MILESTONE-NOT-FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get donor donation-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get completed milestone-data) (err u417))
        (asserts! (not (get approved-by-donor milestone-data)) (err u418))
        (map-set donation-milestones {
            milestone-donation-id: milestone-donation-id,
            milestone-index: milestone-index,
        }
            (merge milestone-data {
                approved-by-donor: true,
                approval-timestamp: (some burn-block-height),
            })
        )
        (map-set milestone-donations { milestone-donation-id: milestone-donation-id }
            (merge donation-data {
                completed-milestones: (+ (get completed-milestones donation-data) u1),
                released-amount: (+ (get released-amount donation-data)
                    (get amount milestone-data)
                ),
            })
        )
        (ok true)
    )
)

(define-public (release-completed-funds (milestone-donation-id uint))
    (let ((donation-data (unwrap!
            (map-get? milestone-donations { milestone-donation-id: milestone-donation-id })
            ERR-NOT-FOUND
        )))
        (asserts! (is-eq tx-sender (get recipient donation-data))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (> (get released-amount donation-data) u0) (err u419))
        (ok (get released-amount donation-data))
    )
)

(define-read-only (get-milestone-donation (milestone-donation-id uint))
    (map-get? milestone-donations { milestone-donation-id: milestone-donation-id })
)

(define-read-only (get-milestone-details
        (milestone-donation-id uint)
        (milestone-index uint)
    )
    (map-get? donation-milestones {
        milestone-donation-id: milestone-donation-id,
        milestone-index: milestone-index,
    })
)

(define-read-only (get-milestone-progress (milestone-donation-id uint))
    (match (map-get? milestone-donations { milestone-donation-id: milestone-donation-id })
        donation-data (ok {
            total-milestones: (get milestone-count donation-data),
            completed-milestones: (get completed-milestones donation-data),
            progress-percentage: (/ (* (get completed-milestones donation-data) u100)
                (get milestone-count donation-data)
            ),
            funds-released: (get released-amount donation-data),
            total-funds: (get total-amount donation-data),
        })
        ERR-NOT-FOUND
    )
)

(define-constant ERR-PLEDGE-NOT-FOUND (err u420))
(define-constant ERR-INSUFFICIENT-PLEDGE-FUNDS (err u421))
(define-constant ERR-PLEDGE-EXPIRED (err u422))
(define-constant ERR-PLEDGE-EXHAUSTED (err u423))
(define-constant ERR-DONATION-NOT-ELIGIBLE (err u424))

(define-data-var next-pledge-id uint u1)

(define-map matching-pledges
    { pledge-id: uint }
    {
        pledger: principal,
        target-recipient: (optional principal),
        max-match-amount: uint,
        remaining-amount: uint,
        match-ratio: uint,
        created-at: uint,
        expires-at: uint,
        active: bool,
    }
)

(define-map matched-donations
    { donation-id: uint }
    {
        original-donor: principal,
        recipient: principal,
        original-amount: uint,
        matched-amount: uint,
        pledge-id: uint,
        pledger: principal,
        verified: bool,
        created-at: uint,
    }
)

(define-map pledge-stats
    { pledger: principal }
    {
        total-pledges-created: uint,
        total-amount-pledged: uint,
        total-amount-matched: uint,
        active-pledges: uint,
    }
)

(define-public (create-matching-pledge
        (target-recipient (optional principal))
        (max-match-amount uint)
        (match-ratio uint)
        (expiry-blocks uint)
    )
    (let (
            (pledge-id (var-get next-pledge-id))
            (current-block burn-block-height)
            (expires-at (+ current-block expiry-blocks))
            (pledger-key { pledger: tx-sender })
            (current-stats (default-to {
                total-pledges-created: u0,
                total-amount-pledged: u0,
                total-amount-matched: u0,
                active-pledges: u0,
            }
                (map-get? pledge-stats pledger-key)
            ))
        )
        (asserts! (> max-match-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (and (> match-ratio u0) (<= match-ratio u100)) (err u425))
        (asserts! (> expiry-blocks u0) (err u426))
        (map-set matching-pledges { pledge-id: pledge-id } {
            pledger: tx-sender,
            target-recipient: target-recipient,
            max-match-amount: max-match-amount,
            remaining-amount: max-match-amount,
            match-ratio: match-ratio,
            created-at: current-block,
            expires-at: expires-at,
            active: true,
        })
        (map-set pledge-stats pledger-key {
            total-pledges-created: (+ (get total-pledges-created current-stats) u1),
            total-amount-pledged: (+ (get total-amount-pledged current-stats) max-match-amount),
            total-amount-matched: (get total-amount-matched current-stats),
            active-pledges: (+ (get active-pledges current-stats) u1),
        })
        (var-set next-pledge-id (+ pledge-id u1))
        (ok pledge-id)
    )
)

(define-public (make-matched-donation
        (recipient principal)
        (amount uint)
        (pledge-id uint)
    )
    (let (
            (pledge-data (unwrap! (map-get? matching-pledges { pledge-id: pledge-id })
                ERR-PLEDGE-NOT-FOUND
            ))
            (donation-id (+ (var-get donation-counter) u1))
            (current-block burn-block-height)
            (match-amount (/ (* amount (get match-ratio pledge-data)) u100))
            (pledger-key { pledger: (get pledger pledge-data) })
            (current-stats (default-to {
                total-pledges-created: u0,
                total-amount-pledged: u0,
                total-amount-matched: u0,
                active-pledges: u0,
            }
                (map-get? pledge-stats pledger-key)
            ))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (get active pledge-data) (err u427))
        (asserts! (< current-block (get expires-at pledge-data))
            ERR-PLEDGE-EXPIRED
        )
        (asserts! (>= (get remaining-amount pledge-data) match-amount)
            ERR-INSUFFICIENT-PLEDGE-FUNDS
        )
        (match (get target-recipient pledge-data)
            target-recipient (asserts! (is-eq recipient target-recipient)
                ERR-DONATION-NOT-ELIGIBLE
            )
            true
        )
        (map-set basic-donations { donation-id: donation-id } {
            donor: tx-sender,
            recipient: recipient,
            amount: amount,
            status: "pending",
            timestamp: current-block,
            verified: false,
        })
        (map-set matched-donations { donation-id: donation-id } {
            original-donor: tx-sender,
            recipient: recipient,
            original-amount: amount,
            matched-amount: match-amount,
            pledge-id: pledge-id,
            pledger: (get pledger pledge-data),
            verified: false,
            created-at: current-block,
        })
        (map-set matching-pledges { pledge-id: pledge-id }
            (merge pledge-data { remaining-amount: (- (get remaining-amount pledge-data) match-amount) })
        )
        (map-set pledge-stats pledger-key {
            total-pledges-created: (get total-pledges-created current-stats),
            total-amount-pledged: (get total-amount-pledged current-stats),
            total-amount-matched: (+ (get total-amount-matched current-stats) match-amount),
            active-pledges: (get active-pledges current-stats),
        })
        (var-set donation-counter donation-id)
        (ok donation-id)
    )
)

(define-public (verify-matched-donation (donation-id uint))
    (let (
            (donation-data (unwrap! (map-get? basic-donations { donation-id: donation-id })
                ERR-DONATION-NOT-FOUND
            ))
            (matched-data (unwrap! (map-get? matched-donations { donation-id: donation-id })
                ERR-NOT-FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get recipient donation-data))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (not (get verified donation-data)) ERR-ALREADY-VERIFIED)
        (map-set basic-donations { donation-id: donation-id }
            (merge donation-data {
                status: "verified",
                verified: true,
            })
        )
        (map-set matched-donations { donation-id: donation-id }
            (merge matched-data { verified: true })
        )
        (ok true)
    )
)

(define-public (deactivate-pledge (pledge-id uint))
    (let (
            (pledge-data (unwrap! (map-get? matching-pledges { pledge-id: pledge-id })
                ERR-PLEDGE-NOT-FOUND
            ))
            (pledger-key { pledger: (get pledger pledge-data) })
            (current-stats (default-to {
                total-pledges-created: u0,
                total-amount-pledged: u0,
                total-amount-matched: u0,
                active-pledges: u0,
            }
                (map-get? pledge-stats pledger-key)
            ))
        )
        (asserts! (is-eq tx-sender (get pledger pledge-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active pledge-data) (err u428))
        (map-set matching-pledges { pledge-id: pledge-id }
            (merge pledge-data { active: false })
        )
        (map-set pledge-stats pledger-key {
            total-pledges-created: (get total-pledges-created current-stats),
            total-amount-pledged: (get total-amount-pledged current-stats),
            total-amount-matched: (get total-amount-matched current-stats),
            active-pledges: (- (get active-pledges current-stats) u1),
        })
        (ok true)
    )
)

(define-read-only (get-matching-pledge (pledge-id uint))
    (map-get? matching-pledges { pledge-id: pledge-id })
)

(define-read-only (get-matched-donation (donation-id uint))
    (map-get? matched-donations { donation-id: donation-id })
)

(define-read-only (get-pledge-stats (pledger principal))
    (map-get? pledge-stats { pledger: pledger })
)

(define-read-only (get-pledge-efficiency (pledge-id uint))
    (match (map-get? matching-pledges { pledge-id: pledge-id })
        pledge-data (let (
                (utilized (- (get max-match-amount pledge-data)
                    (get remaining-amount pledge-data)
                ))
                (efficiency (/ (* utilized u100) (get max-match-amount pledge-data)))
            )
            (ok {
                total-pledged: (get max-match-amount pledge-data),
                amount-utilized: utilized,
                remaining-funds: (get remaining-amount pledge-data),
                utilization-rate: efficiency,
            })
        )
        ERR-PLEDGE-NOT-FOUND
    )
)

(define-constant ERR-POOL-NOT-FOUND (err u430))
(define-constant ERR-POOL-CLOSED (err u431))
(define-constant ERR-POOL-EXPIRED (err u432))
(define-constant ERR-GOAL-ALREADY-REACHED (err u433))
(define-constant ERR-GOAL-NOT-REACHED (err u434))
(define-constant ERR-NO-CONTRIBUTION (err u435))
(define-constant ERR-POOL-ALREADY-CLAIMED (err u436))

(define-data-var next-pool-id uint u1)

(define-map donation-pools
    { pool-id: uint }
    {
        creator: principal,
        recipient: principal,
        target-amount: uint,
        raised-amount: uint,
        deadline: uint,
        created-at: uint,
        status: (string-ascii 20),
        contributor-count: uint,
    }
)

(define-map pool-contributions
    {
        pool-id: uint,
        contributor: principal,
    }
    {
        amount: uint,
        contributed-at: uint,
        refunded: bool,
    }
)

(define-map contributor-pools
    { contributor: principal }
    {
        total-pools-joined: uint,
        total-contributed: uint,
        total-refunded: uint,
        successful-contributions: uint,
    }
)

(define-public (create-donation-pool
        (recipient principal)
        (target-amount uint)
        (deadline-blocks uint)
    )
    (let (
            (pool-id (var-get next-pool-id))
            (current-block burn-block-height)
            (deadline (+ current-block deadline-blocks))
        )
        (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> deadline-blocks u0) (err u437))
        (map-set donation-pools { pool-id: pool-id } {
            creator: tx-sender,
            recipient: recipient,
            target-amount: target-amount,
            raised-amount: u0,
            deadline: deadline,
            created-at: current-block,
            status: "active",
            contributor-count: u0,
        })
        (var-set next-pool-id (+ pool-id u1))
        (ok pool-id)
    )
)

(define-public (contribute-to-pool
        (pool-id uint)
        (amount uint)
    )
    (let (
            (pool-data (unwrap! (map-get? donation-pools { pool-id: pool-id })
                ERR-POOL-NOT-FOUND
            ))
            (current-block burn-block-height)
            (contribution-key {
                pool-id: pool-id,
                contributor: tx-sender,
            })
            (existing-contribution (default-to {
                amount: u0,
                contributed-at: current-block,
                refunded: false,
            }
                (map-get? pool-contributions contribution-key)
            ))
            (contributor-key { contributor: tx-sender })
            (contributor-stats (default-to {
                total-pools-joined: u0,
                total-contributed: u0,
                total-refunded: u0,
                successful-contributions: u0,
            }
                (map-get? contributor-pools contributor-key)
            ))
            (new-raised (+ (get raised-amount pool-data) amount))
            (is-new-contributor (is-eq (get amount existing-contribution) u0))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (get status pool-data) "active") ERR-POOL-CLOSED)
        (asserts! (< current-block (get deadline pool-data)) ERR-POOL-EXPIRED)
        (map-set pool-contributions contribution-key {
            amount: (+ (get amount existing-contribution) amount),
            contributed-at: current-block,
            refunded: false,
        })
        (map-set donation-pools { pool-id: pool-id }
            (merge pool-data {
                raised-amount: new-raised,
                contributor-count: (if is-new-contributor
                    (+ (get contributor-count pool-data) u1)
                    (get contributor-count pool-data)
                ),
                status: (if (>= new-raised (get target-amount pool-data))
                    "funded"
                    "active"
                ),
            })
        )
        (map-set contributor-pools contributor-key {
            total-pools-joined: (if is-new-contributor
                (+ (get total-pools-joined contributor-stats) u1)
                (get total-pools-joined contributor-stats)
            ),
            total-contributed: (+ (get total-contributed contributor-stats) amount),
            total-refunded: (get total-refunded contributor-stats),
            successful-contributions: (get successful-contributions contributor-stats),
        })
        (ok true)
    )
)

(define-public (claim-pool-funds (pool-id uint))
    (let (
            (pool-data (unwrap! (map-get? donation-pools { pool-id: pool-id })
                ERR-POOL-NOT-FOUND
            ))
            (current-block burn-block-height)
        )
        (asserts! (is-eq tx-sender (get recipient pool-data)) ERR-NOT-AUTHORIZED)
        (asserts!
            (>= (get raised-amount pool-data) (get target-amount pool-data))
            ERR-GOAL-NOT-REACHED
        )
        (asserts! (not (is-eq (get status pool-data) "claimed"))
            ERR-POOL-ALREADY-CLAIMED
        )
        (map-set donation-pools { pool-id: pool-id }
            (merge pool-data { status: "claimed" })
        )
        (ok (get raised-amount pool-data))
    )
)

(define-public (request-refund (pool-id uint))
    (let (
            (pool-data (unwrap! (map-get? donation-pools { pool-id: pool-id })
                ERR-POOL-NOT-FOUND
            ))
            (current-block burn-block-height)
            (contribution-key {
                pool-id: pool-id,
                contributor: tx-sender,
            })
            (contribution-data (unwrap! (map-get? pool-contributions contribution-key)
                ERR-NO-CONTRIBUTION
            ))
            (contributor-key { contributor: tx-sender })
            (contributor-stats (default-to {
                total-pools-joined: u0,
                total-contributed: u0,
                total-refunded: u0,
                successful-contributions: u0,
            }
                (map-get? contributor-pools contributor-key)
            ))
        )
        (asserts! (>= current-block (get deadline pool-data)) (err u438))
        (asserts! (< (get raised-amount pool-data) (get target-amount pool-data))
            ERR-GOAL-ALREADY-REACHED
        )
        (asserts! (not (get refunded contribution-data)) (err u439))
        (map-set pool-contributions contribution-key
            (merge contribution-data { refunded: true })
        )
        (map-set contributor-pools contributor-key {
            total-pools-joined: (get total-pools-joined contributor-stats),
            total-contributed: (get total-contributed contributor-stats),
            total-refunded: (+ (get total-refunded contributor-stats)
                (get amount contribution-data)
            ),
            successful-contributions: (get successful-contributions contributor-stats),
        })
        (ok (get amount contribution-data))
    )
)

(define-public (finalize-successful-pool (pool-id uint))
    (let ((pool-data (unwrap! (map-get? donation-pools { pool-id: pool-id })
            ERR-POOL-NOT-FOUND
        )))
        (asserts! (is-eq (get status pool-data) "claimed") (err u440))
        (ok true)
    )
)

(define-read-only (get-donation-pool (pool-id uint))
    (map-get? donation-pools { pool-id: pool-id })
)

(define-read-only (get-pool-contribution
        (pool-id uint)
        (contributor principal)
    )
    (map-get? pool-contributions {
        pool-id: pool-id,
        contributor: contributor,
    })
)

(define-read-only (get-contributor-stats (contributor principal))
    (map-get? contributor-pools { contributor: contributor })
)

(define-read-only (get-pool-progress (pool-id uint))
    (match (map-get? donation-pools { pool-id: pool-id })
        pool-data (ok {
            target: (get target-amount pool-data),
            raised: (get raised-amount pool-data),
            remaining: (if (> (get target-amount pool-data) (get raised-amount pool-data))
                (- (get target-amount pool-data) (get raised-amount pool-data))
                u0
            ),
            progress-percentage: (/ (* (get raised-amount pool-data) u100)
                (get target-amount pool-data)
            ),
            contributors: (get contributor-count pool-data),
            status: (get status pool-data),
        })
        ERR-POOL-NOT-FOUND
    )
)

(define-map donation-metadata
    { donation-id: uint }
    {
        category: (string-ascii 32),
        location: (string-ascii 64),
        food-type: (string-ascii 32),
        expires-at: (optional uint),
        created-at: uint,
        updated-at: uint,
    }
)

(define-public (set-donation-metadata
        (donation-id uint)
        (category (string-ascii 32))
        (location (string-ascii 64))
        (food-type (string-ascii 32))
        (expires-at (optional uint))
    )
    (let (
            (donation (unwrap! (map-get? basic-donations { donation-id: donation-id })
                ERR-DONATION-NOT-FOUND
            ))
            (is-donor (is-eq tx-sender (get donor donation)))
            (is-admin (is-eq tx-sender (var-get admin)))
            (current-block burn-block-height)
            (existing (map-get? donation-metadata { donation-id: donation-id }))
            (created-at (match existing
                metadata (get created-at metadata)
                current-block
            ))
        )
        (asserts! (or is-donor is-admin) ERR-NOT-AUTHORIZED)
        (map-set donation-metadata { donation-id: donation-id } {
            category: category,
            location: location,
            food-type: food-type,
            expires-at: expires-at,
            created-at: created-at,
            updated-at: current-block,
        })
        (ok true)
    )
)

(define-read-only (get-donation-metadata (donation-id uint))
    (map-get? donation-metadata { donation-id: donation-id })
)
