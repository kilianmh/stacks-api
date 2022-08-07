(uiop/package:define-package #:stacks-api
    (:nicknames #:stacks)
  (:use #:cl)
  (:shadow #:type
           #:search)
  (:shadowing-import-from #:dexador
                          #:get)
  (:import-from #:dexador
                #:post)
  (:shadowing-import-from #:closer-mop
                          #:defclass)
  (:import-from #:enhanced-eval-when
                #:eval-always)
  (:import-from #:trivial-types
                #:proper-list #:association-list)
  (:import-from #:trivial-utilities
                #:non-negative-fixnum #:positive-fixnum)
  (:import-from #:serapeum
                #:assure #:package-exports)
  (:import-from #:defstar
                #:defun* #:defvar* #:defparameter* #:*let #:-> #:flet*)
  (:import-from #:str
                #:concat #:downcase #:split #:starts-with-p #:join)
  (:import-from #:quri
                #:make-uri #:uri-https)
  (:import-from #:jonathan
                #:parse)
  (:import-from #:iterate
                #:iter #:for #:collect)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mstrings
                #:mstring-syntax)
  (:export #:*output*
           #:*network*

           ;; accounts
           #:get-account-balances
           #:get-account-stx-balances
           #:get-account-transactions
           #:get-account-transaction-information-for-specific-transaction
           #:get-account-transactions-including-stx-transfers-for-each-transaction
           #:get-the-latest-nonce-used-by-an-account
           #:get-account-assets
           #:get-inbound-stx-transfers
           #:get-account-info

           ;; blocks
           #:get-recent-blocks
           #:get-block-by-hash
           #:get-block-by-height
           #:get-block-by-burnchain-block-hash
           #:get-block-by-burnchain-height

           ;;fees
           #:get-estimated-fee

           ;; fungible-tokens
           #:fungible-tokens-metadata-list
           #:fungible-tokens-metadata-for-contract-id

           ;; info
           #:get-core-api-info
           #:api-status
           #:get-the-network-target-block-time
           #:get-a-given-network-target-block-time
           #:get-total-and-unlocked-stx-supply
           #:get-total-stx-supply-in-plain-text-format
           #:get-circulating-stx-supply-in-plain-text-format
           #:get-total-and-unlocked-stx-supply-legacy
           #:get-proof-of-transfer-details

           ;; microblocks
           #:get-recent-microblocks
           #:get-microblock
           #:get-the-list-of-current-transactions-that-belong-to-unanchored-microblocks

           ;; names
           #:get-namespace-price
           #:get-name-price
           #:get-all-namespaces
           #:get-namespace-names
           #:get-all-names
           #:get-name-details
           #:get-name-subdomains
           #:get-zone-file
           #:get-historical-zone-file
           #:get-names-owned-by-address

           #:non-fungible-token-holdings
           #:non-fungible-token-history
           #:non-fungible-token-mints

           ;; search
           #:search

           ;; smart-contracts
           #:get-contract-info
           #:get-contracts-by-trait
           #:get-contract-events
           #:get-contract-interface
           #:get-contract-source

           ;; stacking-rewards
           #:get-recent-reward-slot-holders
           #:get-recent-reward-slot-holder-entries-for-the-given-address
           #:get-recent-burnchain-reward-recipients
           #:get-recent-burnchain-reward-for-the-given-recipient
           #:get-total-burnchain-rewards-for-the-given-recipient

           ;; transactions
           #:get-recent-transactions
           #:get-mempool-transactions
           #:get-dropped-mempool-transactions
           #:get-list-of-details-for-transactions
           #:get-transaction
           #:get-raw-transaction
           #:transactions-by-block-hash
           #:transactions-by-block-height
           #:transactions-for-address

           ;; post-requests
           #:get-stx-testnet-tokens))
