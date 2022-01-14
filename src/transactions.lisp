(defpackage cl-stacks-api/src/transactions
  (:nicknames :stacks/transactions)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:get-recent-transactions
    #:get-mempool-transactions
    #:get-dropped-mempool-transactions
    #:get-transaction
    #:get-raw-transaction
    #:get-transactions-in-block-hash
    #:get-transactions-in-block-height
    #:get-transactions-for-address-in-mempool))
(in-package :cl-stacks-api/src/transactions)

(def function get-recent-transactions (&key limit offset type unanchored network)
  "Query parameters:
    - limit: (integer) max number of transactions to fetch
    - offset: (integer) index of first transaction to fetch
    - type: (string), items enums = coinbase token_transfer
            smart_contract contract_call poison_microblock
    - unanchored: (string, default: false, alternative: true)
            = Include transaction data from unanchored
              (i.e.unconfirmed) microblocks"
  (rest-get :network network :path "/extended/v1/tx/"
              :query (query 'limit limit 'offset offset 'type type 'unanchored unanchored)))

(def function get-mempool-transactions
       (&key sender_address recipient_address address limit offset unanchored)
  "Get all recently-broadcast mempool transactions.
    Query Parameters:
    - sender_address: (string) = Filter to only return transactions with this sender address.
    - recipient_address: (string) = Filter to only return transactions with
        this recipient address (only applicable for STX transfer tx types).
    - address: (string) = Filter to only return transactions with this address as the sender
        or recipient (recipient only applicable for STX transfer tx types).
    - limit: (integer) = max number of mempool transactions to fetch
    - offset (integer) = index of first mempool transaction to fetch
    - unanchored (boolean-string, default: false, else true) =
        Include transaction data from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :path "/extended/v1/tx/mempool"
              :query (query 'sender_address sender_address 'recipient_address recipient_address
                            'address address 'limit limit 'offset offset 'unanchored unanchored)))

(def function get-dropped-mempool-transactions (&key network limit offset)
  "Get all recently-broadcast mempool transactions
    Query parameters:
        - limit: (integer) = max number of mempool transactions to fetch
        - offset (integer) = index of first mempool transaction to fetch"
  (rest-get :network network :path "/extended/v1/tx/mempool/dropped"
              :query (query 'limit limit 'offset offset)))


(def function get-transaction (tx_id &key (network "mainnet") event_offset event_limit unanchored)
  "Get a specific transaction by ID.
    Required Parameter:
        - tx_id: (string) = Hash of transaction
    Optional parameters:
        - event_offset: (integer, default: 0) = number of events to skip
        - event_limit: (integer, default: 96) = number of events to return
        - unanchored: (string, default: false, alternative: true)
            = Include transaction data from unanchored (i.e.unconfirmed) microblocks"
  (rest-get :network network
              :path (concat "/extended/v1/tx/" tx_id)
              :query (query 'event_offset event_offset
                            'event_limit event_limit
                            'unanchored unanchored)))

(def function get-raw-transaction (tx_id &key (network "mainnet"))
  "Get raw transaction by ID.
    Required Parameter:
        transaction id (string) = Hash of transaction"
  (rest-get :network network
              :path (concat "/extended/v1/tx/" tx_id "/raw")))

(def function get-transactions-in-block-hash (block_hash &key (network "mainnet") limit offset)
  "Get all transactions in block by block-hash
    Required Parameter:
    - block_hash: (string) Hash of block
    Query Parameters:
    - limit: (integer) max number of transactions to fetch
    - offset: (integer) index of first transaction to fetch"
  (rest-get :network network
              :path (concat "/extended/v1/tx/block/" block_hash)
              :query (query 'limit limit 'offset offset)))

(def function get-transactions-in-block-height
       (block_height &key (network "mainnet") limit offset unanchored)
  "Get all transactions in block by height in blockchain
    Required Parameter:
    - height: (integer-string) Height of Block
    Query Parameters:
    - limit: (integer) max number of transactions to fetch
    - offset: (integer) index of first transaction to fetch
    - unanchored: (string, default: false, alternative: true)
            = Include transaction data from unanchored (i.e.unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/tx/block_height/" block_height)
              :query (query 'limit limit 'offset offset 'unanchored unanchored)))

(def function get-transactions-for-address-in-mempool
       (address &key (network "mainnet") limit offset unanchored)
  "Get all transactions for address in mempool
    Required Paramter
    - address (string) Transactions for the address
    Query Paramters
    - limit: (integer) max number of transactions to fetch
    - offset: (integer) index of first transaction to fetch
    - unanchored: (string, default: false, alternative: true)
            = Include transaction data from unanchored (i.e.unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" address "/mempool")
              :query (query 'limit limit 'offset offset 'unanchored unanchored)))
