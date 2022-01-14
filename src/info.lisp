(defpackage cl-stacks-api/src/info
  (:nicknames :stacks/info)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:get-core-api-info
    #:get-blockchain-api-status
    #:get-network-target-block-time
    #:get-given-network-target-block-time
    #:get-total-and-unlocked-stx-supply
    #:get-total-stx-supply-plain-text
    #:get-circulating-stx-supply-plain-text
    #:get-total-and-unlocked-stx-supply-legacy
    #:get-pox-details))
(in-package :cl-stacks-api/src/info)

(def function get-core-api-info (&key network)
  "Get Core API information"
  (rest-get :network network :path "/v2/info"))

(def function get-blockchain-api-status (&key network)
  "Get Blockchain API status"
  (rest-get :network network :path "/extended/v1/status"))

(def function get-network-target-block-time (&key network)
  "Get the network target block time."
  (rest-get :network network :path "/extended/v1/info/network_block_times"))

(def function get-given-network-target-block-time (network)
  "Get a given network's target block time.
    Required Parameter:
    - network: Enum: \"testnet\" \"mainnet\"
    -> Which network to retrieve the target block time of"
  (rest-get :network network
              :path (concat "/extended/v1/info/network_block_time/" network)))

(def function get-total-and-unlocked-stx-supply (height &key network)
  "Get total and unlocked STX supply.
    Required Parameter:
    - height: (number) The block height at which to query supply details
    from, if not provided then the latest block height is used"
  (rest-get :network network :path "/extended/v1/stx_supply" :query (query 'height height)))

(def function get-total-stx-supply-plain-text (&key network)
  "Get total STX supply in plain text format."
  (rest-get :network network :path "/extended/v1/stx_supply/total/plain"))

(def function get-circulating-stx-supply-plain-text (&key network)
  "Get circulating STX supply in plain text format."
  (rest-get :network network
              :path "/extended/v1/stx_supply/circulating/plain"))

(def function get-total-and-unlocked-stx-supply-legacy (height &key network)
  "Get total and unlocked STX supply
    (results formatted the same as the legacy 1.0 API)
    Required Parameter:
    - height (number) The block height at which to query supply details
    from, if not provided then the latest block height is used"
  (rest-get :network network :path "/extended/v1/stx_supply/legacy_format"
              :query (query 'height height)))

(def function get-pox-details (&key network)
  "Get Proof of Transfer (PoX) information. Can be used for Stacking."
  (rest-get :network network :path "/v2/pox"))
