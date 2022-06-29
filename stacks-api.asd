(defsystem "stacks-api"
  :license "AGPL-3.0-or-later"
  :author "Kilian M. Haemmerle"
  :mailto "kilian.haemmerle@protonmail.com"
  :version "0.2.0"  
  :description "Query the Stacks Blockchain API and the Stacks Core API."
  :long-description "- execute (almost) all GET requests and get-stx-testnet-tokens faucet.
                     - network support: mainnet (default) and testnet
                     - output types: hash-table (default), json, alist
                     - static type checking & further input validation"
  :source-control (:git "https://codeberg.org/kilianmh/stacks-api.git")
  :depends-on (closer-mop jonathan serapeum dexador str quri iterate
                          defstar enhanced-eval-when trivial-types trivial-utilities)
  :pathname "code/"
  :serial t
  :components ((:file "package") (:file "classes") (:file "core"))
  :in-order-to ((test-op (test-op "stacks-api/tests"))))

(defsystem "stacks-api/tests"
  :description "Test suite for the stacks-api library"
  :depends-on (stacks-api str fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (find-symbol* '#:main-tests
                                                    '#:stacks-api/tests))))
