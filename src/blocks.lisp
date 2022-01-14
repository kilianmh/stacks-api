(defpackage cl-stacks-api/src/blocks
  (:nicknames :stacks/blocks)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:str #:concat)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:export
    #:get-recent-blocks
    #:get-block-by-hash
    #:get-block-by-height
    #:get-block-by-burnchain-block-hash
    #:get-block-by-burnchain-block-height))
(in-package :cl-stacks-api/src/blocks)

(def function get-recent-blocks (&key network limit offset)
  "Get all recently mined blocks
    Query Paramters
    - limit: (integer) max number of blocks to fetch
    - offset: (integer) index of first blocks to fetch"
  (rest-get :network network :path "/extended/v1/block"
              :query (query 'limit limit 'offset offset)))

(def function get-block-by-hash (hash &key network)
  "Get a specific block by hash
    Required Parameter:
    hash: (string) Hash of the block"
  (rest-get :network network :path (concat "/extended/v1/block/" hash)))

(def function get-block-by-height (height &key network)
  "Get a specific block by height
    Required Parameter:
    - height: (number-string) Height of the block"
  (rest-get :network network
              :path (concat "/extended/v1/block/by_height/" height)))

(def function get-block-by-burnchain-block-hash (burn_block_hash &key network)
  "Get a specific block by burnchain block hash
    Required parameter
    burn_block_hash (string) Hash of the burnchain block"
  (rest-get :network network
              :path (concat "/extended/v1/block/by_burn_block_hash/" burn_block_hash)))

(def function get-block-by-burnchain-block-height (burn_block_height &key network)
  "Get a specific block by burn chain height
    Required Parameter
    - burn_block_height (number-string) Height of the burn chain block"
  (rest-get :network network
              :path (concat "/extended/v1/block/by_burn_block_height/" burn_block_height)))
