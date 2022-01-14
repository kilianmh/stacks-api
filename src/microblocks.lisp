(defpackage cl-stacks-api/src/microblocks
  (:nicknames :stacks/microblocks)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:str #:concat)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:export
    #:get-recent-microblocks
    #:get-microblock-by-hash
    #:get-current-transactions-in-unanchored-microblocks))
(in-package :cl-stacks-api/src/microblocks)

(def function get-recent-microblocks (&key network limit offset)
  "Get recent Microblocks
    Query Paramters
    - limit: (integer) max number of microblocks to fetch
    - offset: (integer) index of first microblock to fetch"
  (rest-get :network network
              :path "/extended/v1/microblock" :query (query 'limit limit 'offset offset)))

(def function get-microblock-by-hash (hash &key network)
  "Get a specific microblock by hash
    Required Paramter:
    - hash: (string) hash of the microblock"
  (rest-get :network network :path (concat "/extended/v1/microblock/" hash)))

(def function get-current-transactions-in-unanchored-microblocks (&key network)
  "This contains transactions that have been streamed in microblocks but
    not yet accepted or rejected in an anchor block"
  (rest-get :network network :path "/extended/v1/microblock/unanchored/txs"))
