(in-package #:stacks-api)

(defmacro check-standardized (parameter assertions)
  "Assertion checking template."
  `(null
    (assert ,assertions
            (,parameter)
            "~A is not a valid ~A" ,parameter (downcase (symbol-name ',parameter)))))

(eval-always
 (defun get-simple-array-character (number)
   `(simple-array character (,number)))

 (defun character-array (&rest numbers)
   "Generate (simple-array character ...) declarations for all supplied numbers"
   (mapcar #'get-simple-array-character numbers)))

(defclass parameter ()
  ((name :initarg :name)
   (documentation :initarg :documentation)
   (type :initarg :type)))

(defmacro make-parameter (name type documentation)
  "Parameter Constructor"
  `(defparameter ,name
     (make-instance 'parameter :name ',name
                               :type ',type
                               :documentation ',documentation)))

(deftype output () '(and keyword (member :hash-table :alist :json)))

(defvar* (*output* output) :hash-table "Default output type")

;; string
(make-parameter tip string "The Stacks chain tip to query from")
(make-parameter zonefilehash string "Example: b100a68235244b012854a95f9114695679002af9")
(make-parameter trait_abi string "JSON abi of the trait.")
(make-parameter contract_name string "Contract name")
(make-parameter map_name string "Map name")
(make-parameter function_name string "Function name")
(make-parameter transaction_payload string "hex-encoded serialization of the TransactionPayload for the transaction")

(deftype network-length () `(simple-array character (7)))

(deftype network () `(and network-length (satisfies network-predicate)))

(defun* (network-predicate -> list) ((network-identifier network-length))
  (member network-identifier '("mainnet" "testnet") :test #'string=))

(make-parameter network network
                "Enum: testnet or mainnet. Which network to retrieve the target block time of")

(defvar* (*network* network) "mainnet" "Default network setting")

(deftype stacks-address-length () `(or ,@(character-array 29 40 41)))

(deftype stacks-address ()
  '(and stacks-address-length (satisfies stacks-address-predicate)))

(defun* (stacks-address-predicate -> t) ((stacks-address stacks-address-length))
  (check-standardized
   stacks-address
   (or (null stacks-address)
       (*let ((stacks-address-length (integer 29 41) (length stacks-address)))
             (cond
               ((and (= stacks-address-length 29)
                     (string/= stacks-address "SP000000000000000000002Q6VF78"))
                (error "The Stacks address ~S is not the genesis address" stacks-address))
               ((string= *network* "mainnet")
                (or (starts-with-p "SP" stacks-address)
                    (error "The stacks mainnet address ~S does not start with \"SP\"" stacks-address)))
               ((string= *network* "testnet")
                (or (starts-with-p "ST" stacks-address)
                    (error "The stacks testnet address ~S does not start with \"ST\"" stacks-address)))
               (t t))))))

(make-parameter sender_address stacks-address
                "Filter to only return transactions with this sender address.")
(make-parameter recipient_address stacks-address
                "Filter to only return transactions with this recipient address (only applicable for STX transfer tx types).")
(make-parameter contract_address stacks-address "Stacks address")

(deftype bitcoin-address-length ()
  `(or ,@(character-array 27 28 29 30 31 32 33 34 42 62)))

(deftype stacks-bitcoin-address ()
  '(and (or stacks-address-length bitcoin-address-length)
    (satisfies stacks-bitcoin-address-predicate)))

(defun* (stacks-bitcoin-address-predicate -> t)
    ((stacks-bitcoin-address (or stacks-address-length bitcoin-address-length)))
  (check-standardized
   stacks-bitcoin-address     
   (if (= (length stacks-bitcoin-address) (or 40 41))
       (stacks-address-predicate stacks-bitcoin-address)
       t)))

(make-parameter address stacks-bitcoin-address "address")

;; contract
(deftype contract () '(and string (satisfies contract-predicate)))

(defun* (contract-predicate -> t) ((contract string))
  (check-standardized
   contract
   (*let ((splitted-contract proper-list (split "." contract)))
         (assure stacks-address (first splitted-contract))
         (null (third splitted-contract)))))

(make-parameter contract_id contract
                "Contract identifier formatted as <contract_address>.<contract_name>")
(make-parameter contractId contract "token's contract id")

(deftype principal () `(and string (satisfies principal-predicate)))

(defun* (principal-predicate -> t) ((principal string))
  (check-standardized principal
                      (*let ((splitted-principal proper-list (split "." principal)))
                            (case (length splitted-principal)
                              (1 (stacks-address-predicate (first splitted-principal)))
                              (2 (contract-predicate principal))
                              (otherwise (error "~S has too many dots (.)" principal))))))

(make-parameter principal principal
                "Stacks address or a Contract identifier (e.g. SP31DA6FTSJX2WGTZ69SFY11BH51NZMB0ZW97B5P0.get-info)")
(make-parameter sender principal "The simulated tx-sender")

(deftype hash (length)
  `(and (simple-array character (,length)) (satisfies hash-predicate)))

(defun* (hash-predicate -> t) ((hash (simple-array character (*))))
  (check-standardized hash
                      (or (starts-with-p "0x" hash)
                          (warn "~S does not start with \"0x\"" hash))))

(make-parameter tx_id (hash 66) "Transaction id")
(make-parameter block_hash (hash 66) "block hash")
(make-parameter hash (hash 66) "Hash")
(make-parameter burn_block_hash (hash 66) "Hash of the burnchain block")

(make-parameter transaction (hash *) "A serialized transaction")
(make-parameter id (hash 66) "The hex hash for a block or transaction, account address, or contract address")
(make-parameter value (hash *)
                "Example: value=0x0100000000000000000000000000000803. hex representation of the token's unique value")

(deftype top-level-domain-length ()
  `(or ,@(character-array 2 3 5 6 7 8 10)))

(deftype top-level-domain ()
  `(and top-level-domain-length (satisfies top-level-domain-predicate)))

(defun* (top-level-domain-predicate -> t) ((top-level-domain top-level-domain-length))
  (*let ((top-level-domain-list list '("app" "blockstack" "btc" "graphite" "helloworld"
                                       "id" "miner" "mining" "podcast" "stacking" "stacks" "stx")))
        (check-standardized top-level-domain
                            (or (member top-level-domain top-level-domain-list :test #'string=)
                                (error "~S is not a top level domain identifier valid values are: ~A" top-level-domain (join " " top-level-domain-list))))))

(make-parameter tld top-level-domain "top level domain namespace")

(deftype blockchain-length ()
  `(or ,@(character-array 6 7)))

(deftype blockchain ()
  `(and top-level-domain-length (satisfies blockchain-predicate)))

(defun* (blockchain-predicate -> t) ((blockchain blockchain-length))
  (*let ((blockchain-list proper-list '("bitcoin" "stacks")))
        (check-standardized blockchain
                            (or (member blockchain blockchain-list :test #'string=)
                                (warn "~S is not a blockchain identifier. valid blockchain enumerators are: ~A"
                                      blockchain (join " " blockchain-list))))))


(make-parameter blockchain blockchain "The layer-1 blockchain for the address. Example: stacks.")

(deftype asset-identifier ()
  '(and string (satisfies asset-identifier-p))) 

(defun* (asset-identifier-p -> t) ((asset-identifier string))
  (check-standardized
   asset-identifier
   (*let ((splitted-asset-identifier proper-list (split "::" asset-identifier)))
         (assure contract (first splitted-asset-identifier))
         (null (third splitted-asset-identifier)))))

(make-parameter asset_identifier asset-identifier
                "Example: asset_identifier=SP2X0TZ59D5SZ8ACQ6YMCHHNR2ZN51Z32E2CJ173.the-explorer-guild::The-Explorer-Guild
                             token asset class identifier")

(deftype domain-name () 
  '(and string (satisfies name-p))) 

(defun* (name-p -> t) ((name string))
  "Name predicate checking"
  (check-standardized
   name
   (*let ((splitted-name proper-list (split "." name)))
         (assure top-level-domain (second splitted-name))
         (null (third splitted-name)))))

(make-parameter name domain-name "Example: id.blockstack. fully-qualified name")

(deftype positive-integer-string () 
  '(and string (satisfies positive-integer-string-predicate)))

(defun* (positive-integer-string-predicate -> t) ((positive-integer-string string))
  "Name predicate checking"
  (check-standardized
   positive-integer-string
   (assure positive-fixnum (parse-integer positive-integer-string))))

(make-parameter until_block positive-integer-string
                "returned data representing the state up until that point in time,
                                               rather than the current block.")

;; boolean
(make-parameter unanchored boolean
                "Include transaction data from unanchored (i.e. unconfirmed) blocks")
(make-parameter stacking boolean
                "Request the amount of STX tokens needed for individual address stacking")
(make-parameter tx_metadata boolean
                "whether or not to include the complete transaction metadata instead of just tx_id.
Enabling this option can affect performance and response times.")
(make-parameter include_metadata boolean
                "This includes the detailed data for particular hash in the response")

(make-parameter limit positive-fixnum "max number of items to fetch")
(make-parameter offset non-negative-fixnum "index of first item to fetch")
(make-parameter height positive-fixnum "Height of block")
(make-parameter block_height positive-fixnum "block height")
(make-parameter proof non-negative-fixnum "Returns object without the proof field if set to 0")
(make-parameter page positive-fixnum
                "Example: page=23
                       names are returned in pages of size 100, so specify the page number. /
                               the page (in 20-entry pages) of the history to fetch")
(make-parameter event_offset non-negative-fixnum "The number of events to skip")
(make-parameter event_limit positive-fixnum "The numbers of events to return")

(deftype bitcoin-block-height ()
  "Defines all bitcoin block height numbers from the stacks block 1 (bitcoin block 666053)"
  `(integer 666053 ,most-positive-fixnum))


(make-parameter burn_block_height bitcoin-block-height  "Height of the burn chain block")
(make-parameter estimated_len positive-fixnum
                "estimation of the final length (in bytes) of the transaction,
  including any post-conditions and signatures")

(deftype transaction-list ()
  `(and proper-list (satisfies transaction-list-predicate)))

(defun* (transaction-list-predicate -> t) ((transaction-list proper-list))
  (check-standardized transaction-list (every #'hash-predicate transaction-list)))

(make-parameter tx_id[] transaction-list "Array of transaction ids")

(deftype type-length ()
  `(or ,@(character-array 8 13 14 17)))

(deftype type-list ()
  `(and proper-list (satisfies type-list-predicate)))

(defun* type-test ((type type-length))
  (*let ((enumerated-types proper-list '("coinbase" "token_transfer"
                                         "smart_contract" "contract_call"
                                         "poison_microblock")))
        (or (member type enumerated-types :test #'string=)
            (warn "~S is not a proper type identicator. Enumerated types are: ~S"
                  type
                  (join " " enumerated-types)))))

(defun* (type-list-predicate -> t) ((type-list proper-list))
        (check-standardized type-list (every #'type-test type-list)))

(make-parameter type[] type-list
                "Items Enum: \"coinbase\" \"token_transfer\" \"smart_contract\" \"contract_call\" \"poison_microblock\"
                       Filter by transaction type")

(deftype asset-identifier-list ()
  `(and proper-list (satisfies asset-identifier-list-predicate)))

(defun* (asset-identifier-list-predicate -> t) ((asset-identifier-list proper-list))
  (check-standardized asset-identifier-list
                      (every #'asset-identifier-p asset-identifier-list)))

(make-parameter asset_identifiers[] asset-identifier-list
                "Array of strings
  Example: asset_identifiers=SPQZF23W7SEYBFG5JQ496NMY0G7379SRYEDREMSV.Candy::candy
  identifiers of the token asset classes to filter for")

(defclass get-function ()
  ((name :initarg :name)
   (path :initarg :path)
   (query :initarg :query)
   (documentation :initarg :documentation)))

(defmacro make-get-function (name documentation path query)
  "Constructor for REST GET function instances"
  `(defparameter ,name
     (make-instance 'get-function :name ',name :documentation ',documentation
                                  :path ',path :query ',query)))

;; account functions
(make-get-function get-account-balances
                   "Get account balances."
                   ("/extended/v1/address/" principal "/balances")
                   (unanchored until_block))
(make-get-function get-account-stx-balances
                   "Retrieves STX token balance for a given Address or Contract Identifier."
                   ("/extended/v1/address/" principal "/stx")
                   (unanchored until_block))
(make-get-function get-account-transactions
                   "Retrieves a list of all Transactions for a given Address or Contract Identifier. More information on Transaction types can be found here.
                       If you need to actively monitor new transactions for an address or contract id, we highly recommend subscribing to WebSockets or Socket.io for real-time updates."
                   ("/extended/v1/address/" principal "/transactions")
                   (limit offset height unanchored until_block))
(make-get-function get-account-transaction-information-for-specific-transaction
                   "Retrieves transaction details for a given Transcation Id tx_id, for a given account or contract Identifier."
                   ("/extended/v1/address/" principal "/" tx_id "/with_transfers")
                   nil)
(make-get-function get-account-transactions-including-stx-transfers-for-each-transaction
                   "Retrieve all transactions for an account or contract identifier including STX transfers for each transaction."
                   ("/extended/v1/address/" principal "/transactions_with_transfers")
                   (limit offset height unanchored until_block))
(make-get-function get-the-latest-nonce-used-by-an-account
                   "Retrieves the latest nonce values used by an account by inspecting the mempool, microblock transactions, and anchored transactions."
                   ("/extended/v1/address/" principal "/nonces")
                   nil)
(make-get-function get-account-assets
                   "Retrieves a list of all assets events associated with an account or a Contract Identifier. This includes Transfers, Mints."
                   ("/extended/v1/address/" principal "/assets")
                   (limit offset unanchored until_block))
(make-get-function get-inbound-stx-transfers
                   "Retrieves a list of STX transfers with memos to the given principal. This includes regular transfers from a stx-transfer transaction type, and transfers from contract-call transactions a the send-many-memo bulk sending contract."
                   ("/extended/v1/address/" principal "/stx_inbound")
                   (limit offset height unanchored until_block))
(make-get-function get-account-info
                   "Retrieves the account data for a given Account or a Contract Identifier
                            Where balance is the hex encoding of a unsigned 128-bit integer (big-endian), nonce is an unsigned 64-bit integer, and the proofs are provided as hex strings.
                            For non-existent accounts, this does not return a 404 error, rather it returns an object with balance and nonce of 0."
                   ("/v2/accounts/" principal)
                   (proof tip))

;; block functions
(make-get-function get-recent-blocks
                   "Retrieves a list of recently mined blocks
                       If you need to actively monitor new blocks, we highly recommend subscribing to WebSockets or Socket.io for real-time updates."
                   ("/extended/v1/block")
                   (limit offset))
(make-get-function get-block-by-hash
                   "Retrieves block details of a specific block for a given block hash"
                   ("/extended/v1/block/" hash)
                   nil)
(make-get-function get-block-by-height
                   "Retrieves block details of a specific block for a given block height"
                   ("/extended/v1/block/by_height/" height)
                   nil)
(make-get-function get-block-by-burnchain-block-hash
                   "Retrieves block details of a specific block for a given burnchain block hash"
                   ("/extended/v1/block/by_burn_block_hash/" burn_block_hash)
                   nil)
(make-get-function get-block-by-burnchain-height
                   "Retrieves block details of a specific block for a given burn chain height"
                   ("/extended/v1/block/by_burn_block_height/" burn_block_height)
                   nil)

;;fees
(make-get-function get-estimated-fee
                   "Retrieves an estimated fee rate for STX transfer transactions. This a a fee rate / byte, and is returned as a integer"
                   ("/v2/fees/transfer")
                   nil)

;; fungible-tokens
(make-get-function fungible-tokens-metadata-list
                   "Retrieves list of fungible tokens with their metadata."
                   ("/extended/v1/tokens/ft/metadata")
                   (limit offset))
(make-get-function fungible-tokens-metadata-for-contract-id
                   "Retrieves the metadata for fungible tokens for a given contract id"
                   ("/extended/v1/tokens/" contract_id "/ft/metadata")
                   nil)

;; info
(make-get-function get-core-api-info
                   "Retrieves information about the Core API including the server version"
                   ("/v2/info")
                   nil)
(make-get-function api-status
                   "Retrieves the running status of the Stacks Blockchain API, including the server version and current chain tip information."
                   ("/extended/v1/status")
                   nil)

(make-get-function get-the-network-target-block-time
                   "Retrieves the target block times for mainnet and testnet.
   The block time is hardcoded and will change throughout the implementation phases of the testnet."
                   ("/extended/v1/info/network_block_times")
                   nil)

(make-get-function get-a-given-network-target-block-time
                   "Retrieves the target block time for a given network. The network can be mainnet or testnet.
                   The block time is hardcoded and will change throughout the implementation phases of the testnet."
                   ("/extended/v1/info/network_block_time/" network)
                   nil)

(make-get-function get-total-and-unlocked-stx-supply
                   "Retrieves the total and unlocked STX supply."
                   ("/extended/v1/stx_supply")
                   (height))
(make-get-function get-total-stx-supply-in-plain-text-format
                   "Retrieves the total supply for STX tokens as plain text."
                   ("/extended/v1/stx_supply/total/plain")
                   nil)
(make-get-function get-circulating-stx-supply-in-plain-text-format
                   "Retrieves the STX tokens currently in circulation that have been unlocked as plain text."
                   ("/extended/v1/stx_supply/circulating/plain")
                   nil)
(make-get-function get-total-and-unlocked-stx-supply-legacy
                   "Retrieves total supply of STX tokens including those currently in circulation that have been unlocked."
                   ("/extended/v1/stx_supply/legacy_format")
                   (height))
(make-get-function get-proof-of-transfer-details
                   "Retrieves Proof-of-Transfer (PoX) information. Can be used for Stacking."
                   ("/v2/pox")
                   nil)

;; microblocks   
(make-get-function get-recent-microblocks
                   "Retrieves a list of microblocks."
                   ("/extended/v1/microblock")
                   (limit offset))
(make-get-function get-microblock
                   "Retrieves a specific microblock by hash"
                   ("/extended/v1/microblock/" hash)
                   nil)
(make-get-function get-the-list-of-current-transactions-that-belong-to-unanchored-microblocks
                   "Retrieves transactions that have been streamed in microblocks but not yet accepted or rejected in an anchor block"
                   ("/extended/v1/microblock/unanchored/txs")
                   nil)

;; names
(make-get-function get-namespace-price
                   "Retrieves the price of a namespace. The amount given will be in the smallest possible units of the currency."
                   ("/v2/prices/namespaces/" tld)
                   nil)
(make-get-function get-name-price
                   "Retrieves the price of a name. The amount given will be in the smallest possible units of the currency."
                   ("/v2/prices/names/" name)
                   nil)
(make-get-function get-all-namespaces
                   "Retrieves a list of all namespaces known to the node."
                   ("/v1/namespaces")
                   nil)
(make-get-function get-namespace-names
                   "Retrieves a list of names within a given namespace."
                   ("/v1/namespaces/" tld "/names")
                   (page))
(make-get-function get-all-names
                   "Retrieves a list of all names known to the node."
                   ("/v1/names")
                   (page))
(make-get-function get-name-details
                   "Retrieves details of a given name including the address, status and last transaction id - last_txid."
                   ("/v1/names/" name)
                   nil)
(make-get-function get-name-subdomains
                   "Retrieves the list of subdomains for a specific name"
                   ("/v1/names/" name "/subdomains")
                   nil)
(make-get-function get-zone-file
                   "Retrieves a user’s raw zone file. This only works for RFC-compliant zone files.
                   This method returns an error for names that have non-standard zone files."
                   ("/v1/names/" name "/zonefile")
                   nil)
(make-get-function get-historical-zone-file
                   "Retrieves the historical zonefile specified by the username and zone hash."
                   ("/v1/names/" name "/zonefile/" zonefilehash)
                   nil)
(make-get-function get-names-owned-by-address
                   "Retrieves a list of names owned by the address provided."
                   ("/v1/addresses/" blockchain "/" address)
                   nil)

;; Non-Fungible Tokens processing not activated on api server
;; required query-parameters
(make-get-function non-fungible-token-holdings
                   "Retrieves the list of Non-Fungible Tokens owned by the given principal (STX address or Smart Contract ID).
    Results can be filtered by one or more asset identifiers and can include metadata about the transaction that made the principal own each token."
                   ("/extended/v1/tokens/nft/holdings")
                   (principal asset_identifiers[] limit offset unanchored tx_metadata))

(make-get-function non-fungible-token-history
                   "Retrieves all events relevant to a Non-Fungible Token.
  Useful to determine the ownership history of a particular asset."
                   ("/extended/v1/tokens/nft/history")
                   (asset_identifier value limit offset unanchored tx_metadata))

(make-get-function non-fungible-token-mints
                   "Retrieves all mint events for a Non-Fungible Token asset class.
Useful to determine which NFTs of a particular collection have been claimed."
                   ("/extended/v1/tokens/nft/mints")
                   (asset_identifier limit offset unanchored tx_metadata))

;; Search
(make-get-function search
                   "Search blocks, transactions, contracts, or accounts by hash/ID"
                   ("/extended/v1/search/" id)
                   (include_metadata))

;; smart-contracts
(make-get-function get-contract-info
                   "Retrieves details of a contract with a given contract_id"
                   ("/extended/v1/contract/" contract_id)
                   (unanchored))
(make-get-function get-contracts-by-trait
                   "Retrieves a list of contracts based on the following traits listed in JSON format
                   - functions, variables, maps, fungible tokens and non-fungible tokens"
                   ("/extended/v1/contract/by_trait")
                   (trait_abi limit offset))
(make-get-function get-contract-events
                   "Retrieves a list of events that have been triggered by a given contract_id"
                   ("/extended/v1/contract/" contract_id "/events")
                   (limit offset unanchored))
(make-get-function get-contract-interface
                   "Retrieves a contract interface with a given contract_address and contract name"
                   ("/v2/contracts/interface/" contract_address "/" contract_name)
                   (tip))
(make-get-function get-contract-source
                   "Retrieves the Clarity source code of a given contract, along with the block height it was published in, and the MARF proof for the data"
                   ("/v2/contracts/source/" contract_address "/"contract_name)
                   (proof tip))

;; stacking-rewards
(make-get-function get-recent-reward-slot-holders
                   "Retrieves a list of the Bitcoin addresses that would validly receive Proof-of-Transfer commitments."
                   ("/extended/v1/burnchain/reward_slot_holders")
                   (limit offset))
(make-get-function get-recent-reward-slot-holder-entries-for-the-given-address
                   "Retrieves a list of the Bitcoin addresses that would validly receive Proof-of-Transfer commitments for a given reward slot holder recipient address."
                   ("/extended/v1/burnchain/reward_slot_holders/" address)
                   (limit offset))
(make-get-function get-recent-burnchain-reward-recipients
                   "Retrieves a list of recent burnchain (e.g. Bitcoin) reward recipients with the associated amounts and block info"
                   ("/extended/v1/burnchain/rewards")
                   (limit offset))
(make-get-function get-recent-burnchain-reward-for-the-given-recipient
                   "Retrieves a list of recent burnchain (e.g. Bitcoin) rewards for the given recipient with the associated amounts and block info"
                   ("/extended/v1/burnchain/rewards/" address)
                   (limit offset))
(make-get-function get-total-burnchain-rewards-for-the-given-recipient
                   "Retrieves the total burnchain (e.g. Bitcoin) rewards for a given recipient address"
                   ("/extended/v1/burnchain/rewards/" address)
                   nil)

;; transactions
(make-get-function get-recent-transactions
                   "Retrieves all recently mined transactions"
                   ("/extended/v1/tx/")
                   (limit offset type[] unanchored))
(make-get-function get-mempool-transactions
                   "Retrieves all transactions that have been recently broadcast to the mempool.
                   These are pending transactions awaiting confirmation."
                   ("/extended/v1/tx/mempool")
                   (sender_address recipient_address address limit offset unanchored))
(make-get-function get-dropped-mempool-transactions
                   "Retrieves all recently-broadcast transactions that have been dropped from the mempool.
                   Transactions are dropped from the mempool if: they were stale and awaiting garbage collection
                   or, were expensive, or were replaced with a new fee"
                   ("/extended/v1/tx/mempool/dropped")
                   (limit offset))
(make-get-function get-list-of-details-for-transactions
                   "Retrieves a list of transactions for a given list of transaction IDs"
                   ("/extended/v1/tx/multiple")
                   (event_offset event_limit unanchored tx_id[]))
(make-get-function get-transaction
                   "Retrieves transaction details for a given transaction ID"
                   ("/extended/v1/tx/" tx_id)
                   (event_offset event_limit unanchored))
(make-get-function get-raw-transaction
                   "Retrieves a hex encoded serialized transaction for a given ID"
                   ("/extended/v1/tx/" tx_id "/raw")
                   nil)
(make-get-function transactions-by-block-hash
                   "Retrieves a list of all transactions within a block for a given block hash."
                   ("/extended/v1/tx/block/" block_hash)
                   (limit offset))
(make-get-function transactions-by-block-height
                   "Retrieves all transactions within a block at a given height"
                   ("/extended/v1/tx/block_height/" block_height)
                   (limit offset unanchored))
(make-get-function transactions-for-address
                   "Retrieves all transactions for a given address that are currently in mempool"
                   ("/extended/v1/address/" address "/mempool")
                   (limit offset unanchored))

(defclass post-function ()
  ((name :initarg :name)
   (path :initarg :path)
   (mandatory-content :initarg :mandatory-content)
   (optional-content :initarg :optional-content)
   (documentation :initarg :documentation)))

(defmacro make-post-function (name documentation path mandatory-content optional-content)
  "Constructor for REST POST function instances"
  `(defparameter ,name
     (make-instance 'post-function
                    :name ',name
                    :documentation ,documentation
                    :path ',path
                    :mandatory-content ',mandatory-content
                    :optional-content ',optional-content)))

;; Faucets
(make-post-function get-stx-testnet-tokens
                    "Add 500 STX tokens to the specified testnet address. Testnet STX addresses begin with ST. If the stacking parameter is set to true, the faucet will add the required number of tokens for individual stacking to the specified testnet address.
                         The endpoint returns the transaction ID, which you can use to view the transaction in the Stacks Explorer. The tokens are delivered once the transaction has been included in an anchor block.
                         A common reason for failed faucet transactions is that the faucet has run out of tokens. If you are experiencing failed faucet transactions to a testnet address, you can get help in Discord.
                         Note: This is a testnet only endpoint. This endpoint will not work on the mainnet."
                    ("/extended/v1/faucets/stx")
                    (address)
                    (stacking))

;; "error": "NFT metadata processing is not enabled on this server"
(make-get-function non-fungible-tokens-metadata-list
                   "Retrieves a list of non fungible tokens with their metadata."
                   ("/extended/v1/tokens/nft/metadata")
                   (limit offset))

(make-get-function non-fungible-tokens-metadata-for-contract-id
                   "Retrieves metadata for non fungible tokens for a given contract id."
                   ("/extended/v1/tokens/" contractId "/nft/metadata")
                   nil)
