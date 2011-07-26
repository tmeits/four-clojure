;; Connected. Hack and be merry!
(ns four.show50
  (:use clojure.test))

(deftest s1
  "Nothing but the Truth"
  (is (= true true))
  (is (= (integer? 1) true))
  (is (= (number? 1) true))
  (is (= (= [1 2 3 4] [1 2 3 4]) true))
  (is (= (not= '(a b c d) '(a b c a)) true))
  (is (= (string? "clojure") true))
  (is (= (seq? (seq '(a b c))) true))
  (is (= (seq? (seq [1 2 3])) true))
  (is (= (not (seq? [1 2 3])) true)))

(deftest s2 "Simple Math"
  (is (= (- 10 (* 2 3)) 4))
  (is (= (+ (* 2 4) (- 4 6)) 6))
  (is (= (+ 21 35 12 7) 75))            ; prefix notation
  (is (= (+ (* 3 5) (- 10 6)) 19))      ; nest
  (is (= (+ (* 3
               (+ (* 2 4)
                  (+ 3 5)))
            (+ (- 10 7)
               6)) 57)))                ; pretty printing

(deftest s3 "Intro to Strings"
  ;; Clojure strings are Java strings.
  ;; This means that you can use any of the Java string methods on Clojure strings.
  (is (= "HELLO WORLD" (.toUpperCase "hello world")))
  (is (= "ПРИВЕТ МИР" (.toUpperCase "Привет Мир")))) ; UTF-8

(deftest s4 "Intro to Lists"
  ;; Lists can be constructed with either a function or a quoted form.
  (is (= (list :a :b :c) '(:a :b :c)))
  (is (= (list 'a 'b 'c) '(a b c))))

(run-tests)

;; 21: Write a function which returns the Nth element from a sequence.
; (= (__ '(4 5 6 7) 2) 6)
; forbidden: nth
(fn [coll n] 
  ((apply comp (cons first (repeat n rest))) coll))
; We first compose n rest functions to get progressively shorter lists till the
; desired element is the head, then take the head. A less fancy version just
; uses nthnext, but it feels like cheating:
(fn [coll n]
  (first (nthnext coll n)))

; 22: Write a function which returns the total number of elements in a sequence.
; (= (__ '(1 2 3 3 1)) 5)
; forbidden: count
#(reduce + (map (fn [x] 1) %))
; We just turn each element into 1 and then add them up
; Note that (fn [x] 1) can be replaced by (constantly 1)

; 23: Write a function which reverses a sequence.
; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
; forbidden: reverse
#(into () %)
; We exploit the property of the list, which alway add new element
; in front of the head. Also that the clojure sequences' equality
; evaluation is element based, so [1 2 3] equals to '(1 2 3)

; 26: Write a function which returns the first X fibonacci numbers.
; (= (__ 6) '(1 1 2 3 5 8))
(fn [x]
  (take x
    ((fn fib [a b]
        (cons a (lazy-seq (fib b (+ a b))))) 
      1 1))) 
; we first recursively construct a lazy sequence of infinite number of
; fibonacci numbers

; 27: Write a function which returns true if the given sequence is a palindrome.
; (true? (__ '(1 1 3 3 1 1)))
(fn [coll]
  (let [rc (reverse coll) n (count coll)]
    (every? identity 
      (map #(= (nth coll %) (nth rc %)) (range (/ (dec n) 2))))))
; we naively compare half of the pairs of elment e(i) and e(n-i-1)

; 28: Write a function which flattens a sequence.
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
; forbidden: flatten
(fn flt [coll]
  (let [l (first coll) r (next coll)]
    (concat 
      (if (sequential? l)
        (flt l)
        [l])
      (when (sequential? r)
        (flt r)))))
; we basically treat the nested collection as a tree and recursively walk the
; tree. Clojure's flatten use a tree-seq to walk the tree.

; 29: Write a function which takes a string and returns a new string containing
;     only the capital letters.
; (= (__ "HeLlO, WoRlD!") "HLOWRD")    
(fn [coll]
  (apply str (filter #(Character/isUpperCase %) coll)))
; note the use of apply here, as str takes a number of args instead
; of a character collection

; 30: Write a function which removes consecutive duplicates from a sequence.
;  (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(fn cmprs [coll]
  (when-let [[f & r] (seq coll)] 
    (if (= f (first r)) 
      (cmprs r) 
      (cons f (cmprs r)))))  
; Basically a variant of the filter function. Note the sequence is destructed
; into first element f and the rest r.

; 31: Write a function which packs consecutive duplicates into sub-lists.
; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(fn [coll]
  ((fn pack [res prev coll]
    (if-let [[f & r] (seq coll)] 
      (if (= f (first prev)) 
        (pack res (conj prev f) r) 
        (pack (conj res prev) [f] r))) 
     (conj res prev))
    [] [(first coll)] (rest coll)))  
; res is the final list, prev keeps the immediate previous sub-list.
; A much simpler version use partition-by:
#(partition-by identity %)

; 33: Write a function which replicates each element of a sequence n number of
; times.
; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
(fn [coll n]
  (apply concat (map #(repeat n %) coll)))
; or more succintly:
(fn [coll n]
  (mapcat #(repeat n %) coll))

; 34: Write a function which creates a list of all integers in a given range.
; (= (__ 1 4) '(1 2 3))
; forbidden: range
(fn [s e]
  (take (- e s) (iterate inc s)))

; 38: Write a function which takes a variable number of parameters and returns
; the maximum value.
; forbidden: max, max-key
; (= (__ 1 8 3 4) 8)
(fn [x & xs]
  (reduce #(if (< %1 %2) %2 %1) x xs))

; 39: Write a function which takes two sequences and returns the first item
; from each, then the second item from each, then the third, etc.
; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
; forbidden: interleave
#(mapcat vector %1 %2) 

; 40: Write a function which separates the items of a sequence by an arbitrary
; value.
; (= (__ 0 [1 2 3]) [1 0 2 0 3])
; forbidden: interpose
(fn [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

; 41: Write a function which drops every Nth item from a sequence.
; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])  
(fn [coll n]
  (flatten 
    (concat 
      (map #(drop-last %) (partition n coll)) 
      (take-last (rem (count coll) n) coll))))
; We partition the sequence, drop last one from each, then stitch them back
; take care the remaining elements too

; 42: Write a function which calculates factorials.
; (= (__ 5) 120)
(fn [n]
  (apply * (range 1 (inc n))))
; clojure arithmetic functions can take a variable number of arguments

; 43: Write a function which reverses the interleave process into n number of
; subsequences.
; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(fn [coll n]
  (apply map list (partition n coll)))
; exploit map function's ability to take a variable number of collections as
; arguments

; 44: Write a function which can rotate a sequence in either direction.
; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
(fn [n coll]
  (let [ntime (if (neg? n) (- n) n)
        lshift #(concat (rest %) [(first %)])
        rshift #(cons (last %) (drop-last %))]
    ((apply comp (repeat ntime (if (neg? n) rshift lshift))) coll)))

; 50: Write a function which takes a sequence consisting of items with different
; types and splits them up into a set of homogeneous sub-sequences. The internal
; order of each sub-sequence should be maintained, but the sub-sequences
; themselves can be returned in any order (this is why 'set' is used in the
; test cases).
; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
#(vals (group-by type %))
