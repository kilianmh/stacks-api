#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem #:cl-stacks-api
  :license "AGPLv3"
  :author "Kilian M. Haemmerle"
  :mailto "kilian.haemmerle@protonmail.com"
  :version "0.1.0"
  :description "Query the Stacks Blockchain API and the Stacks Core API."
  :source-control (:git "https://github.com/kilianmh/cl-stacks-api.git")
  :class :package-inferred-system
  :depends-on ("definer"
               "jonathan"
               "dexador"
               "str"
               "quri"
               "iterate"
               "serapeum"
               "cl-stacks-api/src/all")
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.adoc"))
  :in-order-to ((test-op (test-op "cl-stacks-api/tests")))
  )



(defsystem #:cl-stacks-api/tests
    :description "Test suite for the cl-stacks-api system"
    :author "Kilian M. Haemmerle <kilian.haemmerle@protonmail.com>"
    :version "0.0.2"
    :class package-inferred-system
    :depends-on ("cl-stacks-api/tests/all" "fiveam")
    :license "AGPL-3"
    :perform (test-op (o s) (uiop:symbol-call :fiveam '#:run!
    (find-symbol* '#:main-tests '#:cl-stacks-api/tests/tests))))

(register-system-packages "cl-stacks-api/src/all" '(:cl-stacks-api-implementation))
(register-system-packages "cl-stacks-api/tests/all" '(:cl-stacks-api/tests))
