(defpackage cl-stacks-api/src/smart-contracts
  (:nicknames :stacks/smart-contracts)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:get-contract-info
    #:get-contract-events
    #:get-contract-interface
    #:get-contract-source))
(in-package :cl-stacks-api/src/smart-contracts)

(def function get-contract-info (contract_id &key network unanchored)
  "Get contract info using the contract ID
    Required Parameter:
    - contract_id: (string) Contract identifier formatted as <contract_address>.<contract_name>
    Query Parameter
    - limit: (integer) max number of contract events to fetch
    - offset: (integer) index of first contract event to fetch
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/contract/" contract_id)
              :query (query 'unanchored unanchored)))

(def function get-contract-events (contract_id &key network limit offset unanchored)
  "Get contract info using the contract ID
    Required Parameter:
    - contract_id: (string) Contract identifier formatted as <contract_address>.<contract_name>
    Query Parameter
    - unanchored (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network
              :path (concat "/extended/v1/contract/" contract_id "/events")
              :query (query 'limit limit 'offset offset 'unanchored unanchored)))

(def function get-contract-interface (contract_address contract_name &key network tip)
  "Get contract interface using a contract_address and contract name
    Required Parameters:
    - contract_address: (string) Stacks address
    - contract_name: (string) Contract name
    Query Parameter
    - tip: The Stacks chain tip to query from"
  (rest-get :network network
              :path (concat "/v2/contracts/interface/" contract_address "/" contract_name)
              :query (query 'tip tip)))

(def function get-contract-source (contract_address contract_name &key network proof tip)
  "Returns the Clarity source code of a given contract, along with the block height it was published in, and the MARF proof for the data
    Path Parameters
    - contract_address (required, string) - Stacks address
    - contract_name (required, string) - Contract name
    Query Parameters
    - proof	(integer) - Returns object without the proof field if set to 0
    - tip	(string) - The Stacks chain tip to query from"
  (rest-get :network network
              :path (concat "/v2/contracts/source/" contract_address "/" contract_name)
              :query (query 'proof proof 'tip tip)))
