(defpackage cl-stacks-api/src/stacking-rewards
  (:nicknames :stacks/stacking-rewards)
  (:use :cl)
  (:import-from #:definer #:def)
  (:import-from #:cl-stacks-api/src/core #:query #:rest-get)
  (:import-from #:str #:concat)
  (:export
    #:get-recent-reward-slot-holders
    #:get-recent-reward-slot-holder-entries-for-the-given-address
    #:get-recent-burnchain-reward-recipients
    #:get-recent-burnchain-reward-for-recipient
    #:get-total-burnchain-rewards-for-recipient))
(in-package :cl-stacks-api/src/stacking-rewards)

(def function get-recent-reward-slot-holders (&key network limit offset)
  "Array of the Bitcoin addresses that would validly receive PoX commitments.
    Query Paramters
    - limit: (integer) max number of items to fetch
    - offset: (integer) index of first items to fetch"
  (rest-get :network network
              :path "/extended/v1/burnchain/reward_slot_holders"
              :query (query 'limit limit 'offset offset)))

(def function get-recent-reward-slot-holder-entries-for-the-given-address
       (address &key network limit offset)
  "Array of the Bitcoin addresses that would validly receive PoX commitments.
    Required Parameter:
    - address: (string) Reward slot holder recipient address. Should either be in the native
    burnchain's format (e.g. B58 for Bitcoin), or if a STX principal address is provided it will
    be encoded as into the equivalent burnchain format
    Query Paramters
    - limit: (integer) max number of items to fetch
    - offset: (integer) index of first items to fetch"
  (rest-get :network network
              :path (concat "/extended/v1/burnchain/reward_slot_holders/" address)
              :query (query 'limit limit 'offset offset)))

(def function get-recent-burnchain-reward-recipients (&key network limit offset)
  "Get a list of recent burnchain (e.g. Bitcoin) reward
    recipients with the associated amounts and block info
    Query Parameters
    - limit: (integer) max number of rewards to fetch
    - offset: (integer) index of first rewards to fetch"
  (rest-get :network network
              :path "/extended/v1/burnchain/rewards"
              :query (query 'limit limit 'offset offset)))

(def function get-recent-burnchain-reward-for-recipient
       (address &key network limit offset)
  "Get a list of recent burnchain (e.g. Bitcoin) rewards for the given recipient with the
    associated amounts and block info
    Required Parameter:
    - address: (string) Reward recipient address. Should either be in the native burnchain's
    format (e.g. B58 for Bitcoin), or if a STX principal address is provided it will be
    encoded as into the equivalent burnchain format
    Query Parameters
    - limit: (integer) max number of rewards to fetch
    - offset: (integer) index of first rewards to fetch"
  (rest-get :network network :path (concat "/extended/v1/burnchain/rewards/" address)
              :query (query 'limit limit 'offset offset)))

(def function get-total-burnchain-rewards-for-recipient (address &key network)
  "Get the total burnchain (e.g. Bitcoin) rewards for the given recipient
    Required Parameter:
    - address: (string) Reward recipient address. Should either be in the native burnchain's
    format (e.g. B58 for Bitcoin), or if a STX principal address is provided it will be
    encoded as into the equivalent burnchain format"
  (rest-get :network network
              :path (concat "/extended/v1/burnchain/rewards/" address "/total")))
