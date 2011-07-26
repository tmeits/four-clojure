; 53: Given a vector of integers, find the longest consecutive sub-sequence of
; increasing numbers.  If two sub-sequences have the same length, use the one
; that occurs first. An increasing sub-sequence must have a length of 2 or
; greater to qualify.
; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
; (= (__ [7 6 5 4]) [])
(fn [coll]
  (->> (partition 2 1 coll) 
    (partition-by #(- (second %) (first %))) 
    (filter #(= 1 (- (second (first %)) (ffirst %)))) 
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
; we first create a list of neighoring pairs, partition them by their pair
; differences, keep those with difference 1, finally return the longest one

; 54: Write a function which returns a sequence of lists of x items each.
; Lists of less than x items should not be returned.
; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
; forbidden: partition, partition-all
(fn partition2 [n coll]
  (when (<= n (count coll))
    (cons (take n coll) (partition2 n (drop n coll)))))
; we recursively take n items till not enough items

; 55: Write a function that returns a map containing the number of occurences
; of each distinct item in a sequence.
; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
; forbidden: frequencies
(fn [coll]
  (let [gp (group-by identity coll)] 
    (zipmap (keys gp) (map #(count (second %)) gp))))
; note a map entry is just a two item vector, first item is the key, the
; second item is the value

; 56: Write a function which removes the duplicates from a sequence. Order of
; the items must be maintained.
; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
; forbidden: distinct
(fn [coll] 
  ((fn step [[x & xs] seen] 
     (when x
       (if (seen x) 
         (step xs seen)
         (cons x (step xs (conj seen x)))))) 
   coll #{}))
; we recursively go through the sequence, use a set to keep track of items
; we've seen, only return those we have not seen before.
  

; 58: Write a function which allows you to create function compositions. The
; parameter list should take a variable number of functions, and create a
; function applies them from right-to-left.
; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
; forbidden: comp
(fn [x & xs]
  (fn [& args]
    ((fn step [[f & fs] a]
       (if fs
         (f (step fs a))
         (apply f a)))
     (cons x xs) args)))
; step function takes the function list and the arguments, recursively builds
; an ever deeper call stack till at the end of the list, where the right most
; function is called with the given arguments.
    

; 59: Take a set of functions and return a new function that takes a variable
; number of arguments and returns sequence containing the result of applying
; each function left-to-right to the argument list.
; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
; forbidden: juxt
(fn [x & xs]
  (fn [& args]
    (map #(apply % args) (cons x xs))))

; 60: Write a function which behaves like reduce, but returns each
; intermediate value of the reduction. Your function must accept either two
; or three arguments, and the return sequence must be lazy.
; (= (take 5 (__ + (range))) [0 1 3 6 10])
; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
; forbidden: reductions
(fn reductions2
  ([f init [x & xs]] 
   (cons init (lazy-seq (when x (reductions2 f (f init x) xs))))) 
  ([f coll] 
   (reductions2 f (first coll) (rest coll))))

; 61: Write a function which takes a vector of keys and a vector of values
; and constructs a map from them.
; (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
; forbidden: zipmap
#(into {} (map vector %1 %2))

; 62. Given a side-effect free function f and an initial value x
; write a function which returns an infinite lazy sequence of x,
; (f x), (f (f x)), (f (f (f x))), etc.  
; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
; forbidden: iterate
(fn iterate2 [f x]
  (cons x (lazy-seq (iterate2 f (f x)))))
; it turns out that clojure's own implmentation is the same

; 63. Given a function f and a sequence s, write a function which returns a
; map. The keys should be the values of f applied to each item in s. The value
; at each key should be a vector of corresponding items in the order they
; appear in s.
; (= (__ #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})
; forbidden group-by
(fn [f s]
  ((fn step [ret f [x & xs]]
     (if x
       (let [k (f x)]
         (step (assoc ret k (conj (get ret k []) x)) f xs))
       ret))
    {} f (seq s)))
; the get function takes a default argument for when the key is not found,
; which is used to initialize a vector here. Note the use of seq for s, as
; the collection may be a set, where the [x & xs] destructering doesn't work.
; Intead of recursively going over a sequence, we can also use reduce:
(fn [f s]
  (reduce 
    (fn [ret x]
      (let [k (f x)]
        (assoc ret k (conj (get ret k []) x))))
    {} s))

; 65: Write a function which takes a collection and returns one of :map, :set,
; :list, or :vector - describing the type of collection it was given.
; (= :map (__ {:a 1, :b 2}))
; forbidden: class, type, Class, vector?, sequential?, list?, seq?, map?, set?
; instance? getClass
(fn [coll]
  (let [x (rand-int 100) y (rand-int 100) 
        p [x y] c (conj coll z)]
    (cond 
      (= y (get c x)) :map
      (= p (get c p)) :set
      (= x (last (conj c x))) :vector
      :else :list)))
; we conj a random two element vector into the collection, map will treat it
; as a new key value pair, others treat it as a single item; set is a map too,
; so we can get the vector back with itself as the key; vector and list are
; differentiated by the position of the conj.

; 67: Write a function which returns the first x number of prime numbers.
(fn [x]
  (take x
        (remove 
          (fn [n] 
            (some #(= 0 (mod n %)) (range 2 (inc (int (Math/sqrt n))))))
          (iterate inc 2))))
; we just test each number n, each divided by numbers from 2 up to sqrt(n)

; 69: Write a function which takes a function f and a variable number of maps.
; Your function should return a map that consists of the rest of the maps
; conj-ed onto the first. If a key occurs in more than one map, the mapping(s)
; from the latter (left-to-right) should be combined with the mapping in the
; result by calling (f val-in-result val-in-latter)
; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15}) {1 7, 2 10, 3 15})
; forbidden: merge-with
(fn [f m & ms]
  (reduce 
    (fn [ret x]
      (reduce 
        (fn [r k] 
          (conj r (if (r k) [k (f (r k) (x k))] (find x k)))) 
        ret (keys x))) 
    (cons m ms)))
; note a map is a function itself, so (r k) and (x k) works

; 70: Write a function which splits a sentence up into a sorted list of words.
; Capitalization should not affect sort order and punctuation should be ignored
; (= (__  "Have a nice day.") ["a" "day" "Have" "nice"])
(fn [s]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

; 73: A tic-tac-toe board is represented by a two dimensional vector. X is
; represented by :x, O is represented by :o, and empty is represented by :e. A
; player wins by placing three Xs or three Os in a horizontal, vertical, or
; diagonal row. Write a function which analyzes a tic-tac-toe board and returns
; :x if X has won, :o if O has won, and nil if neither player has won.
; (= nil (__ [[:e :e :e]
            ;[:e :e :e]
            ;[:e :e :e]]))
;(= :x (__ [[:x :e :o]
           ;[:x :e :e]
           ;[:x :e :o]]))
(fn [board]
  (let [i [0 1 2]
        c (take 12 (cycle i))
        p (flatten (map #(repeat 3 %) i))
        zip #(map vector %1 %2)
        win? (fn [w] 
               (some 
                 (fn [x] (every? #(= w (get-in board %)) x)) 
                 (partition 
                   3 (into (zip (into i p) c) (zip c (into (reverse i) p))))))]
    (cond 
      (win? :x) :x
      (win? :o) :o)))
; we basically enumerate all possible winning positions, which fall into
; some regular patterns. I am sure there are better ways, but in the
; interest of time... Note the use of get-in to fetech value in a multiple
; dimensional vector: (get-in board [x y])

; 74: Given a string of comma separated integers, write a function which
; returns a new comma separated string that only contains the numbers
; which are perfect squares.
; (= (__ "4,5,6,7,8,9") "4,9")
(fn [s]
  (->> (re-seq #"\d+" s)
    (map #(Integer/parseInt %))
    (filter (fn [x]
              (let [r (int (Math/sqrt x))]
                (= x (* r r)))))
    (interpose ",")
    (apply str)))

; 75: Two numbers are coprime if their greatest common divisor equals 1.
; Euler's totient function f(x) is defined as the number of positive integers
; less than x which are coprime to x. The special case f(1) equals 1. Write a
; function which calculates Euler's totient function.
; (= (__ 10) (count '(1 3 7 9)) 4)
(fn [n]
  (->> (range 2 n)
    (filter (fn [x]
              (= 1 ((fn gcd [a b]
                      (if (= 0 b) a (gcd b (mod a b))))
                    x n))))
    count
    inc))
