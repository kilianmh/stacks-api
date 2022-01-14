(in-package #:cl-user)
(defpackage cl-stacks-api/src/core
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:str #:concat)
  (:export #:rest-get #:rest-post #:query))
(in-package :cl-stacks-api/src/core)

(def function generate-uri (network path query)
     "Generate URI for Stacks API"
     (quri:make-uri
       :host (concat "stacks-node-api."
                         (if (null network) "mainnet" network)
                         ".stacks.co")
       :scheme "https" :path path :query query))

(def function rest-get (&key path query network)
     "Get request for the Stacks API"
     (let ((uri (generate-uri network path query)))
       (jonathan:parse (dex:get uri) :as :hash-table)))

(def function rest-post (&key network path query content)
     "Post request for the Stacks 2.0 API"
      (let ((uri (generate-uri network path query)))
        (jonathan:parse (dexador:post uri :content content) :as :hash-table)))

(def function query (&rest query)
     "Generate query alist for quri"
     (iter:iter (iter:for (key value) on query by #'cddr)
                (when value (iter:collect (cons (str:downcase (format nil "~a" key)) value)))))
