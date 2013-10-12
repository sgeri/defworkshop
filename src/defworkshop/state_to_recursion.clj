(ns defworkshop.state-to-recursion
  (:require [workshoplib.tdd :refer […]]))

;; More often than not, state can be avoided by using recursion. Let's see how it works

(defn reverse-coll
  "Reverse the collection `coll`.

   You can use `loop/recur` construct to loop over the sequence.
   `cons` prepends items to the list, try that out."
  [coll]
  (loop [[head & tail] coll
         acc []]
    (if (nil? head)
      acc
      (recur tail (cons head acc)))))

(defn recursive-sum
  "We've already implemented sum using reduce, now let's move to implementing it via recursion!"
  ([coll]
   (recursive-sum coll 0))
  ([[head & tail] sum]
   (if (nil? head)
     sum
     (recursive-sum tail (+ sum head)))))

(defn recursive-sum-tc
  "with a tail-recursive version of sum, we can avoid stack overflows."
  ([coll]
     (recursive-sum-tc coll 0))
  ([[head & tail] acc]
   (if (nil? head)
     acc
     (recur tail (+ acc head)))))

(defn max-from-list
  "Get the maximum from list using recursion"
  [[head & tail]]
  (if (empty? tail)
    head
    (let [last-max (max-from-list tail)]
      (if (> head last-max)
        head
        last-max))))

(defn my-reduce
  "generalizing the recursive sum example, write your own implementation of reduce! (for empty coll, just return nil.)"
  ([f [head & tail]]
     (my-reduce f head tail))
  ([f acc-init coll]
    (let [next-head (first coll)
          next-tail (rest coll)]
     (if (nil? next-head)
       acc-init
       (my-reduce f (f acc-init next-head) next-tail)))))

(defn max-from-list-tc
  "Get the maximum from list using tail recursion (avoid stack overflow)"
  ([coll]
     (max-from-list-tc coll 0))
  ([[head & tail] max]
   (if (nil? head)
     max
     (let [new-max (if (> head max) head max)]
       (max-from-list-tc tail new-max)))))

(defn loop-recur-sum
  "This implementation is somewhat easier to understand for people coming from imperative style."
  [numbers]
  (loop [[head & tail] numbers
         sum 0]
    (if (nil? head)
      sum
      (recur tail (+ head sum)))))

(defn map-from-keys-and-vals
  "Something that we've already implemented previously in terms of `zipmap`, but are going to implement again
   in terms of recursion. Usually you use `loop/recur` construct whenever you have a one or multiple accumulators
   or several collections you iterate over."
  [keys vals]
  (loop [[key-head & key-tail] keys
         [val-head & val-tail] vals
         zipped-dict {}]
    (if (or (nil? key-head) (nil? val-head))
      zipped-dict
      (recur key-tail val-tail (assoc zipped-dict key-head val-head)))))

(defn parentheses-balanced?
  "Check wether the given string has balanced parantheses or no.

   You can use `cond` statement to avoid deeply nested ints.
   It's a recursive problem, so you'll have to build up stack to solve it.

   `inc` increments a number, `dec` decrements a number."
  ([str] (parentheses-balanced? str 0))
  ([[current & tail] count]
     (if (< count 0)
       false
       (if (nil? current)
         (= 0 count)
         (recur tail (cond (= \( current) (inc count)
                           (= \) current) (dec count) 
                           true count))))))
