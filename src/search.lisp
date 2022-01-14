(defpackage cl-stacks-api/src/search
  (:nicknames :stacks/search)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:search-by-hash))
(in-package :cl-stacks-api/src/search)

(def function search-by-hash (id &key network)
  "Search blocks, transactions, contracts, or accounts by hash/ID
    Required Parameters:
    - id (string) The hex hash string for a block or transaction,
    account address, or contract address"
  (rest-get :network network :path (concat "/extended/v1/search/" id)))
