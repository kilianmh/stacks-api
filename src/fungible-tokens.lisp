(defpackage cl-stacks-api/src/fungible-tokens
  (:nicknames :stacks/fungible-tokens)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:str #:concat)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:export
    #:fungible-tokens-metadata-list
    #:fungible-tokens-metadata-for-contract-id))

(in-package :cl-stacks-api/src/fungible-tokens)

(def function fungible-tokens-metadata-list (&key network limit offset)
     "Get list of fungible tokens metadata
    Query Parameters
    - limit (integer) max number of tokens to fetch
    - offset (integer) index of first tokens to fetch"
     (rest-get :network network :path "/extended/v1/tokens/ft/metadata"
                 :query (query 'height limit 'offset offset)))

(def function fungible-tokens-metadata-for-contract-id (contract_id &key network)
     "Get fungible tokens metadata for given contract id
    Required Parameters:
    - contractId (string) token's contract id"
     (rest-get :network network
                 :path (concat "/extended/v1/tokens/" contract_id "/ft/metadata")))
