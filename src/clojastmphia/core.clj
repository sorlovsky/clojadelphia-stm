(ns clojastmphia.core
  (:gen-class))
;; Playing with STM
; - by Simon Orlovsky

;; motivation for this ELS, #eng-book-club, @johnconti brought up STM, started
;; looking into STM/atoms a bit more this past month
;; - one of the reasons I was intrigued by clojure back in 2016 was because of
;; STM, in college locks confused me, I didn't want to deal with them
;; - for some reason I thought that because of STM in clojure, it was
;; impossible
;; to write a race condition, boy was I wrong

;; What is STM?
;; Software Transactional Memory (STM) is a concurrency control technique
;analogous to database transactions for controlling access to shared
;memory in concurrent computing. It is an alternative to lock based
;synchronization.

;;          A faster program that doesn't work right is useless.
;;          - Simon Peyton-Jones (Haskell Designer) in "Beautiful Concurrency"

;; things STM makes easy
;; - consistent information
;; - no need for locks
;; - ACI from ACID: atomicity, consistency, isolation. Durability is missing
;; because STM resides in memory

;; Clojure provides four reference types to aide concurrent programming:
; - atoms: access to mutatable state, not necessarily always safe
; - refs: "coordinated": reads and writes to multiple refs guarantees no race
; conditions
; - agents: shared access to mutable state, I don't have a really great
; understanding of agents so i'm going to skip
; over them in this presentation
; - vars: we use these all the time (def x 1)


;; atom is a pointer to some data
(def a (atom 0))

;; you can add a validator
(def a (atom 0 :validator integer?))

;; instantaneous, just reads the value, syntactic sugar is @
(deref a)

;; reset!, has an exclamation point because
;; https://guide.clojure.style/#naming-unsafe-functions
(reset! a 0)

;; creates a function object that will invoke body in another thread
;; will block @/deref until body is done
;; will then cache the results for subsequent @/deref
(def f (future (Thread/sleep 5000) (println "done") 100))
(deref f)


;; LETS MAKE A RACE CONDITION!!

(let [fs (for [_ (range 100)] (future (dotimes [_ 1000] (reset! a (inc @a)))))]
  (doseq [f fs] @f))
(deref a)

;; CAS should solve this right?
;; only set atom to value if the current value is what we think it is
(reset! a 0)
(compare-and-set! a 0 5)
(compare-and-set! a 0 6)

(let [fs (for [_ (range 100)]
           (future
             (dotimes [_ 1000]
               (let [current-value @a]
                 (compare-and-set! a current-value (inc current-value))))))]
  (doseq [f fs] @f))
(deref a)

;; problem is what do we do if it fails?
;; We need to try again

(let [fs
      (for [_ (range 100)]
        (future
          (dotimes [_ 1000]
            (loop []
              (let [current-value @a]
                (when-not (compare-and-set! a current-value (inc current-value))
                  (recur)))))))]
  (doseq [f fs] @f))

;; could this deadlock?
;; every thread will continue to make progress

; Swap
(swap! a inc)
(reset! a 0)
(let [fs (for [_ (range 100)] (future (dotimes [_ 1000] (swap! a inc))))]
  (doseq [f fs] @f))
(deref a)

; Swap2 - lets implement swap ourselves
(defn swap2!
  [a f & args]
  (loop []
    (let [old-val @a
          new-val (apply f old-val args)]
      (if-not (compare-and-set! a old-val new-val) (recur)))))

(reset! a 0)
(let [fs (for [_ (range 100)] (future (dotimes [_ 1000] (swap2! a inc))))]
  (doseq [f fs] @f))
(deref a)

;; problem with atoms - simple bank account example
(def acc1 (atom 0 :validator #(>= % 0)))
(def acc2 (atom 0 :validator #(>= % 0)))
(defn transfer
  [from-acct to-acct amt]
  (swap! to-acct + amt)
  (swap! from-acct - amt))
(reset! acc1 1000)
(reset! acc2 1000)
(dotimes [_ 1000] (future (transfer acc2 acc1 100)))

(deref acc1)
(deref acc2)

;; account 2s validator is not yet checked when we add the account 1

;; refs can help with this because they work in a transactional way
(def acc1 (ref 1000 :validator #(>= % 0)))
(def acc2 (ref 1000 :validator #(>= % 0)))
(defn transfer
  [from-acct to-acct amt]
  (dosync (alter to-acct + amt) (alter from-acct - amt)))
(dotimes [_ 1000] (future (transfer acc2 acc1 100)))

(deref acc1)
(deref acc2)

;; sources -
;; atom example code comes from
;; https://www.youtube.com/watch?v=I72WLDu05Tw&t=325s&ab_channel=ClojureTutorials
;; refs example code comes from
;; https://jeffrytandiono.medium.com/understanding-clojure-stm-atoms-refs-and-agents-525e3230bca9
;; information about atoms/refs comes from The Joy of Clojure by Fogus, Ch.11
