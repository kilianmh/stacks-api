(in-package #:stacks-api)

(defun* (get-stacks-uri -> (values uri-https &optional))
    ((path (or string list)) &optional ((query list)()))
  "Generate REST uri"
  (flet* (((get-host -> (simple-array character (33)))
           () "Generate host string for mainnet and testnet"
           (cond ((string= *network* "mainnet") "stacks-node-api.mainnet.stacks.co")
                 ((string= *network* "testnet") "stacks-node-api.testnet.stacks.co")
                 (t (error "not a valid network identifier")))))
         (make-uri :scheme "https" :host (get-host) :path path :query query)))

(defun* (list-to-alist -> association-list) (&rest query)
  "Generate alist from list"
  (iter (for (key value) on query by #'cddr)
        (when value (collect (cons (downcase (symbol-name key)) value)))))

(eval-when t
  (defun* (get-list-to-alist -> proper-list) ((all-symbols proper-list))
    "Prepare processing of integers and boolean parameter to strings"
    `(list-to-alist ,@(mapcan (lambda (x) (case (get-slot x 'type)
                                            ((non-negative-fixnum positive-fixnum)
                                             `(',x (if ,x
                                                       (write-to-string ,x)
                                                       nil)))
                                            (boolean `(',x (if ,x "true" "false")))
                                            (otherwise `(',x ,x))))
                              all-symbols)))

  (defun* get-slot ((slot-name symbol) (slot symbol))
    "Extract slot value at compile time."
    (eval `(slot-value ,slot-name ',slot)))

  (defun* (typed-lambda-list -> proper-list) ((mandatory-symbols proper-list)
                                              (optional-symbols proper-list))
    "Generate a typed lambda list for functions"
    `(,@(mapcan (lambda (x) `((,x ,(get-slot x 'type))))
                mandatory-symbols)
      &key ,@(mapcar (lambda (x) `((,x (or null ,(get-slot x 'type))) ()))
                     optional-symbols))))

(defmacro get-query (query-symbols)
  "Generate code for handling of query parameters. 
   Result is different for an array/list parameters."
  (flet* (((get-array-symbol -> symbol) ((query-symbols proper-list))
           (car (remove nil
                        (mapcar (lambda (x)
                                  (case (get-slot x 'type)
                                    ((transaction-list type-list asset-identifier-list) x)
                                    (otherwise nil)))
                                query-symbols)))))
         (*let ((array-symbol symbol (get-array-symbol query-symbols))
                (query-symbols proper-list (remove array-symbol query-symbols))
                (list-alist proper-list (get-list-to-alist query-symbols)))
               (if array-symbol
                   `(append (iter (for x in ,array-symbol)
                                  (collect (cons ,(downcase (symbol-name array-symbol)) x)))
                            ,list-alist)
                   list-alist))))

(defmacro get-path-list (path-symbols)
  "All parameters need to be strings for quri.
   Number parameters are therefore converted at run time to string."
  `(concat ,@(mapcar (lambda (x)
                       (if (stringp x)
                           x
                           (case (get-slot x 'type)
                             ((bitcoin-block-height non-negative-fixnum positive-fixnum)
                              `(write-to-string ,x))
                             (otherwise x))))
                     path-symbols)))

(defmacro stacks-request (request-type &optional content)
  "Get/Post Request with type according to global setting"
  `(if (eq :json *output*)
       (,request-type uri :content ,content)
       (parse (,request-type uri :content ,content) :as *output*)))

(defmacro get-function (function-name)
  "Generates a type-checked REST GET function according get-function class instances."
  (flet* (((get-symbols-from-list -> proper-list) ((list proper-list))
           "Extract all symbols from a list"
           (remove nil `(,@(mapcar (lambda (x) (if (typep x 'symbol) x nil)) list)))))
         (*let ((path-params proper-list (get-slot function-name 'path))
                (path-list proper-list `(get-path-list ,path-params))
                (path-symbols proper-list (get-symbols-from-list path-params))
                (query-symbols proper-list (get-slot function-name 'query))
                (query-list proper-list `(get-query ,query-symbols))
                (typed-lambda-list proper-list
                                   (typed-lambda-list path-symbols query-symbols))
                (documentation-string (simple-array character (*))
                                      (get-slot function-name 'documentation)))
               `(defun* ,function-name ,typed-lambda-list
                  ,documentation-string
                  (let ((uri (get-stacks-uri ,path-list ,query-list)))
                    (declare (uri-https uri))
                    (stacks-request get))))))

(defmacro post-function (function-name)
  "Generates a type-checked REST POST function according post-function class instances."
  (*let ((path proper-list (get-slot function-name 'path))
         (mandatory-content-symbols proper-list (get-slot function-name 'mandatory-content))
         (optional-content-symbols proper-list (get-slot function-name 'optional-content))
         (content-symbols proper-list
                          (append mandatory-content-symbols optional-content-symbols))
         (content proper-list (get-list-to-alist content-symbols))
         (typed-lambda-list proper-list (typed-lambda-list mandatory-content-symbols
                                                           optional-content-symbols))
         (documentation-string (simple-array character (*))
                               (get-slot function-name 'documentation)))
        `(defun* ,function-name ,typed-lambda-list
           ,documentation-string
           (let ((uri (get-stacks-uri ,@path)))
             (declare (uri-https uri))
             (stacks-request post ,content)))))

(eval-when t
  (defparameter* (all-function-symbols proper-list)
      (remove nil `(,@(mapcar (lambda (x) 
                                (if (boundp x) 
                                    (typecase (symbol-value x)
                                      (get-function `(get-function ,x))
                                      (post-function `(post-function ,x))
                                      (otherwise nil))
                                    nil))
                              (package-exports))))
      "Stores get and post functions prepared to be \"created\""))

(defmacro create-functions ()
  "Creates functions from all-function-symbols."
  `(values ,@all-function-symbols))

(create-functions)
