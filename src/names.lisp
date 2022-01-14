(defpackage cl-stacks-api/src/names
  (:nicknames :stacks/names)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:str #:concat)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:export
    #:get-namespace-price
    #:get-name-price
    #:get-all-namespaces
    #:get-namespace-names
    #:get-all-names
    #:get-name-details
    #:fetch-zone-file
    #:get-historical-zone-file
    #:get-names-owned-by-address
    #:get-subdomain-at-transaction))
(in-package :cl-stacks-api/src/names)

(def function get-namespace-price (tld &key network)
  "Get the price of a namespace. The amount given will
    be in the smallest possible units of the currency.
    Required Parameter:
    tld (string) the namespace to fetch price for"
  (rest-get :network network
              :path (concat "/v2/prices/namespaces/" tld)))

(def function get-name-price (name &key network)
  "Get the price of a name. The amount given will be
    in the smallest possible units of the currency.
    Required Parameter:
    - name: (string) example: muneeb.id; the
    name to query price information for"
  (rest-get :network network
              :path (concat "/v2/prices/names/" name)))

(def function get-all-namespaces (&key network)
  "Fetch a list of all namespaces known to the node."
  (rest-get :network network
              :path "/v1/namespaces"))

(def function get-namespace-names (tld &key network page)
  "Fetch a list of names from the namespace.
    Required Parameter
    - tld: (string) Example: id; the namespace to fetch names from
    Query Parameters
    - page (integer) Example: page=23; names are returned
    in pages of size 100, so specify the page number"
  (rest-get :network network
              :path (concat "/v1/namespaces/" tld "/names")
              :query (query 'page page)))

(def function get-all-names (&key network page)
  "Fetch a list of all names known to the node.
    Query Parameter:
    - page (integer)  Example: page=23; names are returned
    in pages of size 100, so specify the page number."
  (rest-get :network network
              :path "/v1/names"
              :query (query 'page page)))

(def function get-name-details (name &key network)
  "Required Parameter
    - name (string) Example 133.stx"
  (rest-get :network network
              :path (concat "/v1/names/" name)))

(def function fetch-zone-file (name &key network)
  "Fetch a user’s raw zone file. This only works for RFC-compliant zone files.
    This method returns an error for names that have non-standard zone files.
    Required Parameters
    - name: (string) Example: 11street.btc; fully-qualified name"
  (rest-get :network network
              :path (concat "/v1/names/" name "/zonefile")))

(def function get-historical-zone-file (name zone_file_hash &key network)
  "Fetches the historical zonefile specified by the username and zone hash."
  (rest-get :network network
              :path (concat "/v1/names/" name "/zonefile/" zone_file_hash)))

(def function get-names-owned-by-address (blockchain address &key network)
  "Retrieves a list of names owned by the address provided.
    Required Parameters
    - blockchain (string) Values: bitcoin/stacks;
    the layer-1 blockchain for the address
    - address (string) Example: 1QJQxDas5JhdiXhEbNS14iNjr8auFT96GP;
    the address to lookup"
  (rest-get :network network
              :path (concat "/v1/addresses/" blockchain "/" address)))

(def function get-subdomain-at-transaction (tx_id &key network)
  "Fetches the list of subdomain operations processed by a given transaction.
    The returned array includes subdomain operations that have not yet been accepted
    as part of any subdomain’s history (checkable via the accepted field).
    If the given transaction ID does not correspond to a Blockstack transaction that
    introduced new subdomain operations, and empty array will be returned.
    Required Parameter:
    - tx_id (string)
    Example: d04d708472ea3c147f50e43264efdb1535f71974053126dc4db67b3ac19d41fe
    transaction id"
  (rest-get :network network
              :path (concat "/v1/subdomains/" tx_id)))
