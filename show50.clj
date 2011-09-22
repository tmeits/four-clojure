;; Connected. Hack and be merry!
;; http://lotrepls.appspot.com/
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
  (is (= "ПРИВЕТ МИР" (.toUpperCase "Привет Мир")))) ; translate russian UTF-8

(deftest s4 "Intro to Lists"
  ;; Lists can be constructed with either a function or a quoted form.
  (is (= (list :a :b :c) '(:a :b :c)))
  (is (= (list 'a 'b 'c) '(a b c))))

(deftest s5 "Lists: conj"
  ;; When operating on a list, the conj function will return a new list with one or more items "added" to the front.
  (is (= '(1 2 3 4) (conj '(2 3 4) 1)))
  (is (= '(1 2 3 4) (conj '(3 4) 2 1))))         ; [coll x & xs]

(deftest s6 "Intro to Vectors"
  ;; Vectors can be constructed several ways. You can compare them with lists.
  (is (= [:a :b :c]
         (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))))

(deftest s7 "Vectors: conj"
  ;; When operating on a Vector, the conj function will return a new vector with one or more items "added" to the end.
  (is (= [1 2 3 4] (conj [1 2 3] 4)))
  (is (= [1 2 3 4] (conj [1 2] 3 4))))

(deftest s8 "Intro to Sets"
  ;; Sets are collections of unique values.
  (is (= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d))))
  (is (= #{:a :c :b :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))))

(deftest s9 "Sets: conj"
  ;; When operating on a set, the conj function returns a new set with one or more keys "added".
  (is (= #{1 2 3 4} (conj #{1 4 3} 2))))

(deftest s10 "Intro to Maps"
  ;; Maps store key-value pairs. Both maps and keywords can be used as lookup functions. Commas can be used to make maps more readable, but they are not required.
  (is (= 20 ((hash-map :a 10, :b 20, :c 30) :b)))
  (is (= 20 (:b {:a 10, :b 20, :c 30}))))

(deftest s11 "Maps: conj"
  ;; When operating on a map, the conj function returns a new map with one or more key-value pairs "added".
  (is (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))
  (is (= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))))

(deftest s12 "Intro to Sequences"
  ;; All Clojure collections support sequencing. You can operate on sequences with functions like first, second, and last.
  (is (= 3 (first '(3 2 1))))	
  (is (= 3 (second [2 3 4])))	
  (is (= 3 (last (list 1 2 3)))))

(deftest s13 "Sequences: rest"
  ;; The rest function will return all the items of a sequence except the first.
  (is (= [20 30 40] (rest [10 20 30 40]))))

(deftest s14 "Intro to Functions"
  ;; Clojure has many different ways to create functions.
  (is (= 8 ((fn add-five [x] (+ x 5)) 3)))
  (is (= 8 ((fn [x] (+ x 5)) 3)))	
  (is (= 8 (#(+ % 5) 3)))	
  (is (= 8 ((partial + 5) 3))))

(deftest s15 "Double Down"
  ;; Write a function which doubles a number.
  (is (= (#(+ 2 %) 2) 4))
  (is (= ((fn [x] (+ x 3)) 3) 6))
  (is (= ((fn add-11 [x] (+ x 11)) 11) 22))
  (is (= ((partial + 7) 7) 14)))

(deftest s16 "Hello World"
  ;; Write a function which returns a personalized greeting.
  (is (= (#(str "Hello, " % "!") "Dave") "Hello, Dave!"))  ; There is no help how to put functions.
  (is (= ((fn [x] (str "Hello, " x "!")) "Jenn") "Hello, Jenn!"))
  (is (= ((fn concat-str [x] (str "Hello, " x "!")) "Rhea") "Hello, Rhea!"))
  (is (= ((partial str "Hello, " "CloJure") "!!!") "Hello, CloJure!!!")))

(deftest s17 "Sequences: map"
  ;; The map function takes two arguments: a function (f) and a sequence (s). Map returns a new sequence consisting of the result of applying f to each item of s. Do not confuse the map function with the map data structure.
  (is (= '(6 7 8) (map #(+ % 5) '(1 2 3)))))  ; We add to each element of the list number 5

(deftest s18 "Sequences: filter"
  ;; The filter function takes two arguments: a predicate function (f) and a sequence (s). Filter returns a new sequence consisting of all the items of s for which (f item) returns true.
 (is (= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))))

(deftest s19 "Last Element"
  ;; Write a function which returns the last element in a sequence.
  (is (= (#(peek %) [1 2 3 4 5]) 5))	
  (is (= (#(peek (vec %)) '(5 4 3)) 3))
  (is (= (#(peek %) ["b" "c" "d"]) "d")))

(deftest s20 "Penultimate Element"
  ;; Write a function which returns the second to last element from a sequence.
  (is (= (#(second (reverse %)) (list 1 2 3 4 5)) 4))	
  (is (= (#(second (reverse %)) ["a" "b" "c"]) "b"))	
  (is (= (first [[1 2] [3 4]]) [1 2])))

;; Tips Keeping parentheses balanced. (short of C-Q ( to force-insert a literal one, or other craziness). Inserting an open paren also inserts a closing one. C-u DEL http://www.slideshare.net/mudphone/paredit-preso http://p.hagelb.org/paredit-screencast.html

(deftest s21 "Write a function which returns the Nth element from a sequence. forbidden: nth"
  (is (= (nth '(4 5 6 7) 2) 6))         ; используем встроенную функцию nht, индексация с нуля.
  (is (= (first (drop 2 '(4 5 6 7))) 6)); удалим два элемента из последовательности, в оставшемся списке нужный на первом месте
  (is (= (#(first (nthnext %1 %2)) '(4 5 6 7) 2) 6))
  (is (=  ((fn [coll n] 
             ((apply comp (cons first (repeat n rest))) coll)) '(4 5 6 7) 2)) 6)
  (is (= ((fn [c n] ; рекурсивное решение
            (loop [i n cc c]
              (if (= i 0) (first cc) (recur (- i 1) (rest cc)))))
          '(4 5 6 7) 2) 6)))

;; Функция возвращающая количество элементов в последовательности.
;; Нельзя использовать стандарную count 
(deftest s22 "Write a function which returns the total number of elements in a sequence. forbidden: count"
  (is (= ((fn [x] (reduce + (map (constantly 1) x))) '(1 2 3 3 1)) (count '(1 2 3 3 1))))
  (is (= (#(reduce (fn [x y] (inc x)) 0 %) '(1 2 3 3 1))) 5))

;; Обращение списка (последовательности)
(deftest s23 "Write a function which reverses a sequence.  forbidden: reverse"
  (is (= (#(into nil %) [1 2 3 4 5]) [5 4 3 2 1]))
  ;; рекурсивное решение
  (is (= ((fn [c] (loop [cc c n nil]
                    (if (empty? cc)
                      n
                      (recur (rest cc) (conj n (first cc))))))
          [1 2 3 4 5])
         (reverse [1 2 3 4 5]))))

(deftest s24 "Write a function which returns the sum of a sequence of numbers."
  (defn sum24 [coll]
   (reduce + (seq coll)))               ; #(reduce + (seq coll)) for http://4clojure.com/problem/24
  (is (= (sum24 [1 2 3]) 6))
  (is (= (sum24 (list 0 -2 5 5)) 8))
  (is (= (sum24 #{4 2 1}) 7))
  (is (= (sum24 '(0 0 -1)) -1))
  (is (= (sum24 '(1 10 3)) 14)))

; Нечётное число — целое число, которое не делится без остатка на 2:   …, −3, −1, 1, 3, 5, 7, 9, …
(deftest s25 "Write a function which returns only the odd numbers from a sequence."
  ; #(filter odd? %) http://4clojure.com/problem/25 
  (is (= (#(filter odd? %) #{1 2 3 4 5}) '(1 3 5)))
  (is (= (#(filter odd? %) [4 2 1 6]) '(1)))
  (is (= (#(filter odd? %) [2 2 4 6]) '()))
  (is (= (#(filter odd? %) [1 1 1 3]) '(1 1 1 3))))

;; Время, которое уходит на оттачивание мастерства, лучше, чем время
;; потраченное на выражение себя на Flickr, Facebook, Twitter.
(deftest s26
  "Write a function which returns the first X fibonacci numbers."
  (defn fib [n]
    (second (reduce 
             (fn [[a b] _] [b (+ a b)]) ; function to calculate the next pair of values
             [0 1]         ; initial pair of fibonnaci numbers
             (range 0 n)))) ; a seq to specify how many iterations you want)
    (is (= (map fib  (range 0 6)) '(1 1 2 3 5 8))))


;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; C-c M-p to change the namespace of the repl session.
;; M-. to jump to a definition
;; http://en.wikibooks.org/wiki/Clojure_Programming/Examples/Lazy_Fibonacci

(deftest s27 "Write a function which returns true if the given sequence is a palindrome."
  (is (false? (#(= (reverse %) (seq %)) '(1 2 3 4 5))))
  (is (true?  (#(= (reverse %) (seq %)) "racecar")))
  (is (true?  (#(= (reverse %) (seq %)) [:foo :bar :foo])))
  (is (true?  (#(= (reverse %) (seq %)) '(1 1 3 3 1 1))))
  (is (false? (#(= (reverse %) (seq %)) '(:a :b :c)))))


;; Tips
;; http://groups.google.com/group/clojure/browse_thread/thread/7633d311ac63214f
;; http://www.infoq.com/articles/in-depth-look-clojure-collections
;; Keys
;; 

;; Чтобы перечислить листья дерева, мы можем использовать процедуру flatten.
(deftest s28 "Write a function which flattens a sequence. forbidden: flatten"
  (is (= (flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]) [1, 2, 3, 4, 5, 6, 7, 8]))
  (is (= (flatten-loop [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]) [1, 2, 3, 4, 5, 6, 7, 8])))

(defn flatten-loop-recur "loop and recur for flattens all embedded sequential"
  [coll]
  (loop [acc [] [head & tail :as coll] coll]
    (cond
     (empty? coll) acc
     (coll? head) (recur (into acc (flatten-loop-recur head)) tail)
     :else (recur (conj acc head) tail))))

;; http://habrahabr.ru/blogs/python/111768/
;; http://library.readscheme.org/page1.html

(defn flatten-tree [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn flatten-rec  "flattens all embedded sequential"
  [coll]
  (filter
   #(not (nil? %))
   (cond
    (not (coll? coll)) (list coll)
    :else (concat
           (flatten-rec (first coll))
           (flatten-rec (next coll))))))

(defn flatten-loop  [tree]
  (loop  [[self & todo :as src] tree dst []]
    (cond
     (empty? src) dst                                ; усе
     (not (coll? self)) (recur todo (conj dst self)) ; в руках атом!
     (empty? self) (recur todo dst)     ; в руках пустая
                                        ; последовательность, дальше
     :else (recur (list* (first self) (rest self) todo) dst))))

(defn flatten-loop-con  [tree]
  (loop  [[self & todo :as src] tree dst []]
    (cond
     (empty? src) (do (println "empty-src") dst) ; 
     (not (coll? self)) (do (println "not-coll-self") (recur todo (conj dst self))) ; атом!
     (empty? self) (do (println "empty-empty") (recur todo dst))   ; 
     :else (do (println "big-big") (recur (list* (first self) (rest self) todo) dst)))))

;; https://docs.google.com/document/d/1TJQw_KhNl2NYf5JoGx1dv3V9Uk4vwsDqcL9LxL7WOZM/edit?hl=ru&pli=1
;; https://gist.github.com/1120880
;; http://www.loufranco.com/blog/files/category-20-days-of-clojure.html

;; One project I'd love to see users take on is providing a small example
;; of the use of every library function (and its output). That would be a
;; great complement to the documentation. 


(defn flat-lazy-seq [[x & more :as coll]]
  (cond
    (empty? coll) []
    (coll? x) (lazy-seq (concat (flat-lazy-seq x) (flat-lazy-seq more)))
    :else (lazy-seq (concat (vector x) (flat-lazy-seq more)))))

;; Using lazy-cat
(defn flat-lazy-cat [[x & more :as coll]]
  (cond
    (empty? coll) []
    (coll? x) (lazy-cat (flat-lazy-cat x) (flat-lazy-cat more))
    :else (lazy-cat (vector x) (flat-lazy-cat more))))

(defn skl2 [tree]
  (loop [[self & todo :as src] [tree] dst [()]]
    (cond
     (empty? src) dst
     (not (coll? self)) (recur todo (first dst))
     (empty? self) (recur todo (conj (first dst) (rest dst)))
     :else (recur (list* (first self) (rest self) todo) [dst]))))

(defn nested-seq "Создадим вложенный список заданной глубины. функция нужна для
тестирования flatten разных вариантов"
  [n]
  (lazy-seq (take n (iterate list (rand-int 100000)))))

;; Result:
;; four.show50>  (time (count (flatten (nested-seq 50000))))
;; "Elapsed time: 2672722.607394 msecs"
;; 50000
;; four.show50>  (time (count (flatten-loop (nested-seq 50000))))
;; "Elapsed time: 1818748.542657 msecs"
;; 50000


;; http://habrahabr.ru/blogs/soft/105300/#habracut Про Org-Mode
;; http://www.softcraft.ru/paradigm/dp/dp05-06.shtml
;; http://e-maxx.ru/index.php
;; http://wikisec.ru/index.php?title=%D0%A2%D1%80%D0%B5%D0%B1%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F_%D0%BA_5_%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D1%83_%D0%B7%D0%B0%D1%89%D0%B8%D1%89%D0%B5%D0%BD%D0%BD%D0%BE%D1%81%D1%82%D0%B8_%D0%A1%D0%92%D0%A2
;; http://emeliyannikov.blogspot.com/2011/08/blog-post_22.html

;; Функция сканирующая стоку и оставляющая только прописные символы
;; (большие буквы)
(deftest only-capital 
  "Write a function which takes a string and returns a new string containing
   only the capital letters."
  (is (= (capital-str "HeLlO, WoRlD!") "HLOWRD"))
  (is (= (capital-str-match "HeLlO, WoRlD!") "HLOWRD")))

(defn capital-str [coll]
  (reduce str (filter #(Character/isUpperCase %) coll)))    

;; После функции filter мы получаем clojure.lang.LazySeq содержащий
;; java.lang.Character. Применим свертку для преобразования k
;; java.lang.String

(defn capital-str-match [coll] "Use regex substination"
  (.replaceAll coll "[^A-Z]"""))

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

(run-tests)
