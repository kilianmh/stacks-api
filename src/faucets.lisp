(defpackage cl-stacks-api/src/faucets
  (:nicknames :stacks/faucets)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-post)
  (:export
  #:get-stx-testnet-tokens))
(in-package :cl-stacks-api/src/faucets)

(def function get-stx-testnet-tokens (address &key ((:stacking stacking) "false"))
  "Get STX tokens for the testnet
    Query Parameters
    - address (required, string) STX address
    - stacking (boolean, default: false)
    Request the amount of STX needed for stacking"
  (rest-post :network "testnet" :path "/extended/v1/faucets/stx"
               :query (query 'address address 'stacking stacking)))
