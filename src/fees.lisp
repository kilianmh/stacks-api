(defpackage cl-stacks-api/src/fees
  (:nicknames :stacks/fees)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:rest-get)
  (:export
    #:get-estimated-fee
    #:fetch-fee-rate))
(in-package :cl-stacks-api/src/fees)

(def function get-estimated-fee (&key network)
  "Get an estimated fee rate for STX transfer transactions.
    This a a fee rate / byte, and is returned as a JSON integer."
  (rest-get :network network :path "/v2/fees/transfer"))