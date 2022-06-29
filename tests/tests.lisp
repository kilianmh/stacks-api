(defpackage #:stacks-api/tests
  (:use #:cl
        #:stacks-api
        #:fiveam)
  (:shadowing-import-from #:str
                          #:concat
                          #:downcase)
  (:shadow #:type)
  (:shadowing-import-from #:stacks-api
                          #:search)
  (:export #:run!
           #:main-tests
           #:post-requests-tests
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
           #:transactions-tests
           #:nft-tests))

(in-package :stacks-api/tests)

(defparameter account "SP33XEHK2SXXH625VG6W6665WBBPX1ENQVKNEYCYY")
(defparameter account-transaction
  "0x2b518d5e27ac7b049576a5c13898a1164921a92fcf767367abf131537526a178")
(defparameter block-hash
  "0x6fddfeab9b1b2e4c3d90d37ec773efabc64d7907dbe6faf40d3d75b5e3b96be0")
(defparameter burnchain-block-hash
  "0x0000000000000000000a820c6653a571f207420708cff2101a7ad76cad9841ca")
(defparameter burnchain-block-height 715972)
(defparameter block-height 44123)
(defparameter fungible-token-contract-id "SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR.usda-token")
(defparameter microblock-hash "0x4da8f087655b806579064e3dfec34f582bec6e85513e4aa54d97ffd7c4780950")
(defparameter contract-address "SP248HH800501WYSG7Z2SS1ZWHQW1GGH85ME34NT2")
(defparameter contract-name "layer-v1-1")
(defparameter contract-identifier (concat contract-address "." contract-name))
(defparameter serialized-transaction "0x5e9f3933e358df6a73fec0d47ce3e1062c20812c129f5294e6f37a8d27c051d9")
(defparameter offset 5)

(defmacro generate-test (function-name &rest parameters)
  `(test ,(read-from-string (concat (string function-name)
                                    "-test"))
         (is (not (null ,(if parameters
                             `(,function-name ,@parameters)
                             `(,function-name))))
             ,(concat (downcase function-name)
                      " failed"))))

(def-suite main-tests
  :description "Top level test suite")

(def-suite get-requests-tests :in main-tests
  :description "REST GET Request test suite")

(def-suite accounts-tests :in get-requests-tests)
(in-suite accounts-tests)

(generate-test get-account-balances account)
(generate-test get-account-stx-balances account)
(generate-test get-account-transactions account)
(generate-test get-account-transaction-information-for-specific-transaction account account-transaction)
(generate-test get-account-transactions-including-stx-transfers-for-each-transaction account)
(generate-test get-the-latest-nonce-used-by-an-account account)
(generate-test get-account-assets account)
(generate-test get-inbound-stx-transfers account)
(generate-test get-account-info account)

(def-suite blocks-tests :in get-requests-tests)
(in-suite blocks-tests)

(generate-test get-recent-blocks)
(generate-test get-block-by-hash block-hash)
(generate-test get-block-by-height block-height)
(generate-test get-block-by-burnchain-block-hash  burnchain-block-hash)
(generate-test get-block-by-burnchain-height burnchain-block-height)
(generate-test get-block-by-hash block-hash)

(def-suite fees-tests :in get-requests-tests)
(in-suite fees-tests)

(generate-test get-estimated-fee)

(def-suite fungible-tokens-tests :in get-requests-tests)
(in-suite fungible-tokens-tests)

(generate-test fungible-tokens-metadata-list)
(generate-test fungible-tokens-metadata-for-contract-id fungible-token-contract-id)

(def-suite info-tests :in get-requests-tests)
(in-suite info-tests)

(defparameter height 1000)
(generate-test get-core-api-info)
(generate-test api-status)
(generate-test get-the-network-target-block-time)
(generate-test get-a-given-network-target-block-time "mainnet")
(generate-test get-total-and-unlocked-stx-supply :height height)
(generate-test get-total-stx-supply-in-plain-text-format)
(generate-test get-circulating-stx-supply-in-plain-text-format)
(generate-test get-total-and-unlocked-stx-supply-legacy :height height)
(generate-test get-proof-of-transfer-details)

(def-suite microblocks-tests :in get-requests-tests)
(in-suite microblocks-tests)

(generate-test get-recent-microblocks)

(generate-test get-microblock microblock-hash)

(generate-test get-the-list-of-current-transactions-that-belong-to-unanchored-microblocks)

(def-suite names-tests :in get-requests-tests)
(in-suite names-tests)
(defparameter namespace "btc")
(defparameter domain-name "100.btc")
(defparameter name "muneeb.id")
(defparameter stacks "stacks")
(defparameter lookup-address "1QJQxDas5JhdiXhEbNS14iNjr8auFT96GP")

(generate-test get-namespace-price namespace)
(generate-test get-name-price domain-name)
(generate-test get-all-namespaces)
(generate-test get-namespace-names namespace)
(generate-test get-all-names)
(generate-test get-name-details domain-name)
(generate-test get-zone-file "11street.btc")
(generate-test get-names-owned-by-address stacks lookup-address)

(defparameter nft-contract "SPNWZ5V2TPWGQGVDR6T7B6RQ4XMGZ4PXTEE0VQ0S.marketplace-v3")
(defparameter asset-identifiers '("SPQZF23W7SEYBFG5JQ496NMY0G7379SRYEDREMSV.Candy::candy"))
(defparameter asset-identifier "SP2X0TZ59D5SZ8ACQ6YMCHHNR2ZN51Z32E2CJ173.the-explorer-guild::The-Explorer-Guild")
(defparameter value "0x0100000000000000000000000000000803")

(def-suite nft-tests :in get-requests-tests)
(in-suite nft-tests)

(generate-test non-fungible-token-holdings :principal nft-contract
                                           :asset_identifiers[] asset-identifiers)
(generate-test non-fungible-token-history :asset_identifier asset-identifier :value value)
(generate-test non-fungible-token-mints :asset_identifier asset-identifier)

(def-suite search-tests :in get-requests-tests)
(in-suite search-tests)

(generate-test search block-hash)

(def-suite smart-contracts-tests :in get-requests-tests)
(in-suite smart-contracts-tests)

(generate-test get-contract-info contract-identifier)
(generate-test get-contract-events contract-identifier)
(generate-test get-contract-interface contract-address contract-name)
(generate-test get-contract-source contract-address contract-name)

(def-suite stacking-rewards-tests :in get-requests-tests)
(in-suite stacking-rewards-tests)
(defparameter stacking-address "1BFfc2e6Kk82ut7S3C5yaN3pWRxEFRLLu5")

(generate-test get-recent-reward-slot-holders)
(generate-test get-recent-reward-slot-holder-entries-for-the-given-address stacking-address)
(generate-test get-recent-burnchain-reward-recipients)
(generate-test get-recent-burnchain-reward-for-the-given-recipient stacking-address)
(generate-test get-total-burnchain-rewards-for-the-given-recipient stacking-address)

(def-suite transactions-tests :in get-requests-tests)
(in-suite transactions-tests)
(defparameter tx-id  "0xd4e453b4bed7c7ffec09260077549557bdd3f540de3de41618f9957d2ca64385")

(generate-test get-recent-transactions)
(generate-test get-mempool-transactions)
(generate-test get-dropped-mempool-transactions)
(generate-test
 get-list-of-details-for-transactions
 :tx_id[] '("0xea052bfb2b80732f392e1a16be30be41d84b8bc1bdcf259f58f4b1b5339de452"
            "0x8506a60971a586dcfaf01d758e9ff34d3b2fcd4ffea655e3ca759cbe18a6e4db"))
(generate-test get-transaction tx-id)
(generate-test get-raw-transaction tx-id)
(generate-test transactions-by-block-hash block-hash)
(generate-test transactions-by-block-height block-height)
(generate-test transactions-for-address account)
