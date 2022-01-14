(uiop:define-package :cl-stacks-api/src/all
  (:nicknames :stacks-user)
  (:documentation "Query the Stacks 2.0 blockchain and interact with smart contracts")
  (:use-reexport
   :cl-stacks-api/src/accounts
   :cl-stacks-api/src/blocks
   :cl-stacks-api/src/faucets
   :cl-stacks-api/src/fees
   :cl-stacks-api/src/fungible-tokens
   :cl-stacks-api/src/info
   :cl-stacks-api/src/microblocks
   :cl-stacks-api/src/names
   :cl-stacks-api/src/rosetta
   :cl-stacks-api/src/search
   :cl-stacks-api/src/smart-contracts
   :cl-stacks-api/src/stacking-rewards
   :cl-stacks-api/src/transactions))
