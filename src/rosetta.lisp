(defpackage cl-stacks-api/src/rosetta
  (:nicknames :stacks/rosetta)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-post)
  (:export
  #:get-list-of-available-networks))
(in-package :cl-stacks-api/src/rosetta)

(def function get-list-of-available-networks (&key network)
  "Get List of Available Networks.
    This endpoint returns a list of NetworkIdentifiers that the Rosetta server supports."
  (rest-post :network network :path "/rosetta/v1/network/list" :query (query)))
