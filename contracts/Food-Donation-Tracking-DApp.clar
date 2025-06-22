;; Food Donation Tracking Smart Contract

(define-data-var admin principal tx-sender)

(define-map donations
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
        (map-set donations { donation-id: donation-id } {
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
            (donation (unwrap! (map-get? donations { donation-id: donation-id })
                ERR-DONATION-NOT-FOUND
            ))
            (recipient-key { recipient: (get recipient donation) })
            (donor-key { donor: (get donor donation) })
        )
        (asserts! (is-eq tx-sender (get recipient donation)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified donation)) ERR-ALREADY-VERIFIED)
        (map-set donations { donation-id: donation-id }
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

(define-read-only (get-donation (donation-id uint))
    (map-get? donations { donation-id: donation-id })
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
