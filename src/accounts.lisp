(defpackage cl-stacks-api/src/accounts
  (:nicknames :stacks/accounts)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:get-account-balances
    #:get-account-stx-balances
    #:get-account-transactions
    #:get-account-information-for-specific-transaction
    #:get-account-transactions-including-stx-transfers-for-each-transaction
    #:get-latest-nonce-used-by-account
    #:get-account-assets
    #:get-inbound-stx-transfers
    #:get-nft-events
    #:get-account-info))
(in-package :cl-stacks-api/src/accounts)

(def function get-account-balances (principal &key network unanchored)
  "Get account balances.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    Query Parameter
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" principal "/balances")
              :query (query 'unanchored unanchored)))

(def function get-account-stx-balances (principal &key network unanchored)
  "Get account STX balance.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    Query Parameter
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" principal "/stx")
              :query (query 'unanchored unanchored)))

(def function get-account-transactions
       (principal &key network limit offset height unanchored)
  "Get account STX balance.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    Query Parameters
    - limit: (integer) max number of account transactions to fetch
    - offset: (integer) index of first transaction to fetch
    - height: (number) Filter for transactions only at this given block height
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network
              :path (concat "/extended/v1/address/" principal "/transactions")
              :query (query 'limit limit 'offset offset 'height height 'unanchored unanchored)))

(def function get-account-information-for-specific-transaction (principal tx_id &key network)
  "Get account transaction information for specific transaction.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    - tx_id: (string) Transaction id"
  (rest-get :network network
              :path (concat "/extended/v1/address/" principal "/" tx_id "/with_transfers")))

(def function
     get-account-transactions-including-stx-transfers-for-each-transaction
     (principal &key network limit offset height unanchored)
     "Get account transactions including STX transfers for each transaction.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    Query Parameters
    - limit: (integer) max number of account transactions to fetch
    - offset: (integer) index of first transaction to fetch
    - height: (number) Filter for transactions only at this given block height
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
     (rest-get :network network
                 :path (concat "/extended/v1/address/" principal "/transactions_with_transfers")
                 :query (query 'limit limit 'offset offset 'height height 'unanchored unanchored)))

(def function get-latest-nonce-used-by-account (principal &key network)
  "Get the latest nonce values used by an account by inspecting the mempool, microblock
    transactions, and anchored transactions.
    Required Parameter:
    - principal (string) Stacks address or a Contract identifier"
  (rest-get :network network
              :path (concat "/extended/v1/address/" principal "/nonces")))

(def function get-account-assets (principal &key network limit offset unanchored)
  "Get account assets.
     - principal: (string) Stacks address or a Contract identifier
    Query Parameters
    - limit: (integer) max number of account assets to fetch
    - offset: (integer) index of first account asset to fetch
    - unanchored: (boolean-string, default: false, alternatively: true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" principal "/assets")
              :query (query 'limit limit 'offset offset 'unanchored unanchored)))

(def function get-inbound-stx-transfers
       (principal &key network limit offset height unanchored)
  "Get a list of STX transfers with memos to the given principal. This includes regular
    transfers from a stx-transfer transaction type, and transfers from contract-call
    transactions a the send-many-memo bulk sending contract.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    (e.g. SP31DA6FTSJX2WGTZ69SFY11BH51NZMB0ZW97B5P0.get-info)
    Query Parameters
    - limit: (integer) number of items to return
    - offset: (integer) index of items to skip
    - height: (number) Filter for transactions only at this given block height
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" principal "/stx_inbound")
              :query (query 'limit limit 'offset offset 'height height 'unanchored unanchored)))

(def function get-nft-events (principal &key network limit offset unanchored)
  "Get list of all nfts owned by an address, contains the
    clarity value of the identifier of the nft
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    (e.g. SP31DA6FTSJX2WGTZ69SFY11BH51NZMB0ZW97B5P0.get-info)
    Query Parameters:
    - limit: (integer) number of items to return
    - offset: (integer) index of items to skip
    - unanchored: (boolean-string, default: false, alternatively true) Include transaction data
    from unanchored (i.e. unconfirmed) microblocks"
  (rest-get :network network :path (concat "/extended/v1/address/" principal "/nft_events")
              :query (query 'limit limit 'offset offset 'unanchored unanchored)))

(def function get-account-info (principal &key network proof tip)
  "Get the account data for the provided principal.
    Where balance is the hex encoding of a unsigned 128-bit integer (big-endian),
    nonce is a unsigned 64-bit integer, and the proofs are provided as hex strings.
    For non-existent accounts, this does not 404, rather it returns an object
    with balance and nonce of 0.
    Required Parameters:
    - principal: (string) Stacks address or a Contract identifier
    (e.g. SP31DA6FTSJX2WGTZ69SFY11BH51NZMB0ZW97B5P0.get-info)
    Query Parameters:
    - proof: (integer) Returns object without the proof field if set 0
    - tip: (string) Stacks chain tip to query from"
  (rest-get :network network
              :path (concat "v2/accounts/" principal)
              :query (query 'proof proof 'tip tip)))
