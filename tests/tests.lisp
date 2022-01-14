(defpackage :cl-stacks-api/tests/tests
  (:use #:cl #:cl-stacks-api/src/all #:fiveam)
  (:export #:run!
           #:main-tests
           #:accounts-tests
           #:blocks-tests
           #:fees-tests
           #:fungible-tokens-tests
           #:info-tests
           #:microblocks-tests
           #:names-tests
           #:rosetta-tests
           #:search-tests
           #:smart-contracts
           #:stacking-rewards-tests
           #:transactions-tests))

(in-package :cl-stacks-api/tests/tests)

(defmacro generate-test (function-name &rest parameters)
  `(test ,(read-from-string (str:concat (string function-name) "-test"))
         (is (not (null ,(if parameters `(,function-name ,@parameters) `(,function-name))))
             ,(str:concat (string-downcase function-name) " failed"))))

(def-suite main-tests
           :description "Top Level Test suite")

;; Accounts

(def-suite accounts-tests :in main-tests)
(in-suite accounts-tests)

(defparameter account "SP33XEHK2SXXH625VG6W6665WBBPX1ENQVKNEYCYY")

(generate-test get-account-balances account)

(generate-test get-account-stx-balances account)

(generate-test
  get-account-information-for-specific-transaction
  account
  "0x2b518d5e27ac7b049576a5c13898a1164921a92fcf767367abf131537526a178")

(generate-test
  get-account-transactions-including-stx-transfers-for-each-transaction
  account)

(generate-test get-latest-nonce-used-by-account account)

(generate-test get-account-assets account)

(generate-test get-inbound-stx-transfers account)

;; Blocks

(def-suite blocks-tests :in main-tests)
(in-suite blocks-tests)

(generate-test get-recent-blocks)

(generate-test
  get-block-by-hash
  "0x6fddfeab9b1b2e4c3d90d37ec773efabc64d7907dbe6faf40d3d75b5e3b96be0")

(generate-test get-block-by-height "42932")

(generate-test
  get-block-by-burnchain-block-hash
  "0x0000000000000000000a820c6653a571f207420708cff2101a7ad76cad9841ca")

(generate-test get-block-by-burnchain-block-height "715972")

(generate-test
  get-block-by-hash
  "0x6fddfeab9b1b2e4c3d90d37ec773efabc64d7907dbe6faf40d3d75b5e3b96be0")


;; Fees

(def-suite fees-tests :in main-tests)
(in-suite fees-tests)

(generate-test get-estimated-fee)


;; Fungible Tokens

(def-suite fungible-tokens-tests :in main-tests)
(in-suite fungible-tokens-tests)

(generate-test fungible-tokens-metadata-list)

(generate-test fungible-tokens-metadata-for-contract-id
               "SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR.usda-token")


;; Info

(def-suite info-tests :in main-tests)
(in-suite info-tests)

(generate-test get-core-api-info)

(generate-test get-blockchain-api-status)

(generate-test get-network-target-block-time)

(generate-test get-given-network-target-block-time "mainnet")

(generate-test get-total-and-unlocked-stx-supply 1000)

(generate-test get-total-stx-supply-plain-text)

(generate-test get-circulating-stx-supply-plain-text)

(generate-test get-total-and-unlocked-stx-supply-legacy 1000)

(generate-test get-pox-details)


;; Microblocks

(def-suite microblocks-tests :in main-tests)
(in-suite microblocks-tests)

(generate-test get-recent-microblocks)

(generate-test get-microblock-by-hash
               "0x4da8f087655b806579064e3dfec34f582bec6e85513e4aa54d97ffd7c4780950")

(generate-test get-current-transactions-in-unanchored-microblocks)


;;Names

(def-suite names-tests :in main-tests)
(in-suite names-tests)

(generate-test get-namespace-price "btc")

(generate-test get-name-price "100.btc")

(generate-test get-all-namespaces)

(generate-test get-namespace-names "btc")

(generate-test get-all-names)

(generate-test get-name-details "100.btc")

(generate-test fetch-zone-file "11street.btc")

; (generate-test get-historical-zone-file)

(generate-test get-names-owned-by-address
               "stacks"
               "1QJQxDas5JhdiXhEbNS14iNjr8auFT96GP")

;(generate-test get-subdomain-at-transaction)

;; Rosetta

(def-suite rosetta-tests :in main-tests)
(in-suite rosetta-tests)

(generate-test get-list-of-available-networks)

;; Search

(def-suite search-tests :in main-tests)
(in-suite search-tests)

(generate-test search-by-hash
               "0x8584bbe8238a791767f7f5a80ad3ac4519098031fda168d6189439759c76ea6e")

;; Smart Contracts

(def-suite smart-contracts-tests :in main-tests)
(in-suite smart-contracts-tests)

(generate-test get-contract-info
               "SP248HH800501WYSG7Z2SS1ZWHQW1GGH85ME34NT2.layer-v1-1")

(generate-test get-contract-events
               "SP248HH800501WYSG7Z2SS1ZWHQW1GGH85ME34NT2.layer-v1-1")

(generate-test get-contract-interface
               "SP248HH800501WYSG7Z2SS1ZWHQW1GGH85ME34NT2"
               "layer-v1-1")

(generate-test get-contract-source
               "SP248HH800501WYSG7Z2SS1ZWHQW1GGH85ME34NT2"
               "layer-v1-1")


;; Stacking Rewards

(def-suite stacking-rewards-tests :in main-tests)
(in-suite stacking-rewards-tests)

(generate-test get-recent-reward-slot-holders)

(generate-test get-recent-reward-slot-holder-entries-for-the-given-address
               "1BFfc2e6Kk82ut7S3C5yaN3pWRxEFRLLu5")

(generate-test get-recent-burnchain-reward-recipients)

(generate-test get-recent-burnchain-reward-for-recipient
               "1BFfc2e6Kk82ut7S3C5yaN3pWRxEFRLLu5")

(generate-test get-total-burnchain-rewards-for-recipient
               "1BFfc2e6Kk82ut7S3C5yaN3pWRxEFRLLu5")


;; Transactions

(def-suite transactions-tests :in main-tests)
(in-suite transactions-tests)

(generate-test get-recent-transactions)

(generate-test get-mempool-transactions)

(generate-test get-dropped-mempool-transactions)

(generate-test get-transaction
               "0xd4e453b4bed7c7ffec09260077549557bdd3f540de3de41618f9957d2ca64385")

(generate-test get-raw-transaction
               "0xd4e453b4bed7c7ffec09260077549557bdd3f540de3de41618f9957d2ca64385")

(generate-test get-transactions-in-block-hash
               "0x2ac45337d7a4bff65c24a709244d03bc38024ab2a86cd252a059804e4b31fbb5")

(generate-test get-transactions-in-block-height "44123")

(generate-test get-transactions-for-address-in-mempool
               "SP3VYCKS11684SPB5M73AVS692B1GR35XBQPSYS2Z")
