; 77: Write a function which finds all the anagrams in a vector of words. A
; word x is an anagram of word y if all the letters in x can be rearranged in
; a different order to form y. Your function should return a set of sets, where
; each sub-set is a group of words which are anagrams of each other. Each
; sub-set should have at least two words. Words without any anagrams should not
; be included in the result.
; (= (__ ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}})
; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
(fn [coll]
  (->> (group-by frequencies coll)
    (vals)
    (filter #(> (count %) 1))
    (map set )
    set))
; anagrams have the same distribution of characters

; 79: Write a function which calculates the sum of the minimal path through a
; triangle. The triangle is represented as a vector of vectors. The path should
; start at the top of the triangle and move to an adjacent number on the next
; row until the bottom of the triangle is reached.
; (= (__ [  [1]
          ;[2 4]
         ;[5 1 4]
        ;[2 3 4 5]])
   ;(+ 1 2 1 3)
   ;7)
(fn [triangle] 
  (apply min ((fn path-sum [p] 
                (concat 
                  (if (= (count triangle) (count p)) 
                    [(reduce + (map-indexed #(get-in triangle [%1 %2]) p))] 
                    (let [x (last p)] 
                      (concat 
                        (path-sum (conj p x)) 
                        (path-sum (conj p (inc x)))))))) 
              [0])))
; We enumerate all possible paths. The next step in a path can only go to the
; same or the plus one row index as the previous step, so the paths form a
; binary tree. We walk the tree recursively, building a row index vector p for
; each path.

; 81: Reimplement set intersection
#(set (filter %1 %2))
; sets are functions too, so this works

; 82: A word chain consists of a set of words ordered so that each word differs
; by only one letter from the words directly before and after it. The one
; letter difference can be either an insertion, a deletion, or a substitution.
; Here is an example word chain:
; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
; Write a function which takes a sequence of words, and returns true if they
; can be arranged into one continous word chain, and false if they cannot.
; (= false (__ #{"cot" "hot" "bat" "fat"}))
; (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
(fn [word-set]
  (letfn [(edit-dist [a b] 
            (cond 
              (not (or a b)) 0 
              (not b) (count a) 
              (not a) (count b) 
              :else (let [ra (next a) rb (next b)] 
                      (if (= (first a) (first b)) 
                        (edit-dist ra rb) 
                        (+ 1 (min 
                               (edit-dist ra rb) 
                               (edit-dist ra b) 
                               (edit-dist a rb)))))))
          (find-paths [graph start seen] 
            (if (seen start) 
              seen
              (for [n (graph start)] 
                (find-paths graph n (conj seen start)))))] 
    (let [graph (into {} 
                      (for [s word-set] 
                        [s (filter #(= 1 (edit-dist s %)) word-set)]))]
      (if (some (fn [w] 
                  (some #(= word-set %) 
                        (flatten (find-paths graph w #{})))) 
                word-set) 
        true false))))
; This problem consists of two sub-problems: A. Determine the edit distance
; between two strings. For brevity, we just used the standard recursive
; algorithm instead of dynamic programming. B. For the graph of strings
; connected by edges of edit distance 1, find a simple (no loop) path that
; goes through all strings once and only once. The graph is represented as
; a map of adjacent node lists. We enumerate all simple paths in the graph
; until we found one going through all nodes.

; 84: Write a function which generates the transitive closure of a binary
; relation. The relation will be represented as a set of 2 item vectors.
; (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
;   (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
; (let [progeny
;       #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
;         (= (__ progeny)
;              #{["father" "son"] ["father" "grandson"]
;                     ["uncle" "cousin"] ["son" "grandson"]}))
(fn [relation]
  (letfn [(expand [r] 
            (let [m (into {} r)] 
              (->> (concat 
                     r
                     (for [[k v] m] 
                       (when-let [nv (m v)] [k nv]))) 
                (filter identity) 
                set)))
          (first-consecutive [pred [f & rs]] 
            (when rs
              (if (pred f (first rs))
                f
                (recur pred rs))))]
    (first-consecutive = (iterate expand relation))))
; we iteratively expand the set of transitive relation, until the set no
; longer changes

; 85: Write a function which generates the power set of a given set. The power
; set of a set x is the set of all subsets of x, including the empty set and x
; itself.
; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(fn [s]
  (reduce 
    (fn [init e] 
      (set (concat init (map #(conj % e) init) [#{e}])))
    #{#{}} s))
; we just add one element at a time

; 86: Happy numbers are positive integers that follow a particular formula: take
; each individual digit, square it, and then sum the squares to get a new number
; Repeat with the new number and eventually, you might get to a number whose
; squared sum is 1. This is a happy number. An unhappy number (or sad number) is
; one that loops endlessly. Write a function that determines if a number is
; happy or not.
; (= (__ 7) true)
; (= (__ 986543210) true)
(fn [x]
  (letfn [(digits [n]
            (for [y (iterate (partial * 10) 1) :while (<= y n)]
              (rem (int (/ n y)) 10)))
          (sqr-sum [ds]
            (reduce + (map #(* % %) ds)))]
    (let [r (some #{1 4} (iterate (comp sqr-sum digits) x))]
      (cond
        (= 1 r) true
        (= 4 r) false))))
; it turns out that 4 is a sad number, as it results into an infinite loop

; 88: Write a function which returns the symmetric difference of two sets. The
; symmetric difference is the set of items belonging to one but not both of
; the two sets.
; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
#(set (remove (set (filter %1 %2)) (into %1 %2)))
; we remove the intersection from the union

; 89: Starting with a graph you must write a function that returns true if it
; is possible to make a tour of the graph in which every edge is visited exactly
; once.  The graph is represented by a vector of tuples, where each tuple
; represents a single edge.  The rules are:
; - You can start at any node.  
; - You must visit each edge exactly once.  
; - All edges are undirected.
; (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
; (= false (__ [[1 2] [2 3] [2 4] [2 5]]))
; (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
; (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d]
;              [:c :e] [:d :e] [:c :f] [:d :f]]))
(fn [edge-list]
  (let [graph (apply merge-with 
                #(into %1 %2) 
                (apply concat 
                  (map-indexed 
                    (fn [i [k v]] 
                      [{k #{{:node v :index i}}} 
                       {v #{{:node k :index i}}}]) 
                    edge-list)))]
    (if (some
          (fn [node] 
            (some 
              identity 
              (flatten 
                ((fn visit [n vs] 
                   (if (every? #(vs (:index %)) (graph n)) 
                     (if (every? identity vs) true false) 
                     (for [x (graph n)] 
                       (when-not (vs (:index x)) 
                         (visit (:node x) (assoc vs (:index x) true))))))
                 node (vec (repeat (count edge-list) false)))))) 
          (set (apply concat edge-list))) 
      true false)))
; This problem looks similar to problem 82 as both are graph traversals, but
; the graphs are quite different. Here redundent edges exist, so we cannot use
; a set or a map to track edge visits, we instead use a vector of booleans.
; Also, the condition is to traverse all edges instead of all nodes. We again
; use a map of adjacent node lists as the graph, but supplement each adjacent
; node with the index of the corresponding edge. Finally, here a node can be
; visited multiple times, and we terminates a path at a node only when all of
; its edges have already been visited.

; 91: Given a graph, determine whether the graph is connected. A connected
; graph is such that a path exists between any two given nodes.  
; -Your function must return true if the graph is connected and false otherwise.
; -You will be given a set of tuples representing the edges of a graph. Each
;  member of a tuple being a vertex/node in the graph.  
; -Each edge is undirected (can be traversed either direction).
;  (= false (__ #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]}))
;  (= true (__ #{[1 2] [2 3] [3 1][4 5] [5 6] [6 4] [3 4]}))
(fn [edge-list]
  (let [graph (apply merge-with 
                #(into %1 %2) 
                (apply concat 
                  (map (fn [[k v]] [{k #{v}} {v #{k}}]) edeg-list)))] 
    (if (some #(= (count %) (count graph)) 
              (flatten 
                ((fn paths [node seen] 
                   (if (seen node) 
                     seen
                     (for [x (graph node)] 
                       (paths x (conj seen node))))) 
                 (ffirst graph) #{}))) 
      true false)))
;  This graph traversal problem is simpler than both 82 and 89. We only  need
;  to start searching from any one of the nodes instead of all nodes. But the
;  pattern of the code is similar.

; 92: Write a function to parse a Roman-numeral string and return the number it
; represents. You can assume that the input will be well-formed, in upper-case,
; and follow the subtractive principle. You don't need to handle any numbers
; greater than MMMCMXCIX (3999), the largest number representable with
; ordinary letters.
; (= 827 (__ "DCCCXXVII"))
; (= 48 (__ "XLVIII"))
(fn [s]
  (let [snum {[\C \M] 900  [\C \D] 400 [\X \C] 90 
             [\X \L] 40 [\I \X] 9 [\I \V] 4}
        nums {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (letfn [(sum-snum [[f & r]]
                      (if f
                        (+ (if-let [n (snum [f (first r)])] 
                             n 0)
                           (sum-snum r))
                        0))
            (del-snum [[f & r]]
                         (when f
                           (if (snum [f (first r)])
                             (del-snum (rest r))
                             (cons f (del-snum r)))))]
      (reduce + (sum-snum s) (map nums (del-snum s))))))
; We first find and sum the special numbers (4, 9, etc), remove them and sum
; the rest.  

; 93: Write a function which flattens any nested combination of sequential
; things (lists, vectors, etc.), but maintains the lowest level sequential
; items. The result should be a sequence of sequences with only one level of
; nesting.
; (= (__ '((1 2)((3 4)((((5 6))))))) '((1 2)(3 4)(5 6)))
(fn pf [coll]
  (let [l (first coll) r (next coll)]
    (concat 
      (if (and (sequential? l) (not (sequential? (first l))))
        [l]
        (pf l))
      (when (sequential? r)
        (pf r)))))
; this is just a slight modification of the solution to problem 28.

; 94: The game of life is a cellular automaton devised by mathematician John
; Conway.  The 'board' consists of both live (#) and dead ( ) cells. Each cell
; interacts with its eight neighbours (horizontal, vertical, diagonal), and its
; next state is dependent on the following rules: 1) Any live cell with fewer
; than two live neighbours dies, as if caused by under-population.  2) Any live
; cell with two or three live neighbours lives on to the next generation.  3)
; Any live cell with more than three live neighbours dies, as if by overcrowding
; . 4) Any dead cell with exactly three live neighbours becomes a live cell, as
; if by reproduction.  Write a function that accepts a board, and returns a
; board representing the next generation of cells.
;(= (__ ["      "
        ;" ##   "
        ;" ##   "
        ;"   ## "
        ;"   ## "
        ;"      "])
   ;["      "
    ;" ##   "
    ;" #    "
    ;"    # "
    ;"   ## "
    ;"      "])
(fn [board]
  (let [offsets [[-1 -1] [-1 0] [-1 1]
                 [0 -1] [0 1]
                 [1 -1] [1 0] [1 1]]
        height (count board)
        width (count (first board))
        get-state (fn [[x y] [dx dy]]
                    (let [c (+ x dx) r (+ y dy)] 
                      (if (or (< c 0) (= c width) (< r 0) (= r height))
                        \space
                        (get-in board [r c]))))
        count-lives (fn [p]
                      (reduce + (map #(if (= \# (get-state p %)) 1 0) offsets)))
        next-state (fn [s p]
                     (let [n (count-lives p)] 
                       (if (or (= n 3)
                               (and (= s \#) (= n 2)))
                         \#
                         \space)))] 
    (->> (for [y (range height) x (range width)]
           (next-state (get-in board [y x]) [x y]))
      (partition width)
      (map #(apply str %))
      vec)))
; This is straight-forward. The only tricky part is to remember that the order
; of paramaters for the get-in function and the x-y coordinates is opposite to
; each other.  

; 95: Write a predicate which checks whether or not a given sequence represents
; a binary tree. Each node in the tree must have a value, a left child, and a
; right child.
; (= (__ '(:a (:b nil nil) nil)) true)
; (= (__ '(:a (:b nil nil))) false)
; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]]) true)
(fn bt? [t]
  (if (or (not (sequential? t))
          (and (= 3 (count t))
               (bt? (second t))
               (bt? (last t))))
    true false))
; I think one of the unit tests of the problem is wrong:
; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil]) false)
; why shouldn't "false" be a legal tree node, or why should leaf have to be nil?

; 96: Let us define a binary tree as "symmetric" if the left half of the tree
; is the mirror image of the right half of the tree. Write a predicate to
; determine whether or not a given binary tree is symmetric.
; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
; (= (__ '(:a (:b nil nil) nil)) false)
; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          ;[2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true)
(fn [t]
  ((fn mir? [l r]
     (if (or (= nil l r)
             (and (= (first l) (first r))
                  (mir? (second l) (last r))
                  (mir? (last l) (second r))))
       true false)) 
   (second t) (last t)))

; 97: Pascal's triangle is a triangle of numbers computed using the following
; rules:
; - The first row is 1.
; - Each successive row is computed by adding together adjacent numbers in the
;   row above, and adding a 1 to the beginning and end of the row.  
; Write a function which returns the nth row of Pascal's Triangle.
; (= (map __ (range 1 6))
   ;[     [1]
        ;[1 1]
       ;[1 2 1]
      ;[1 3 3 1]
     ;[1 4 6 4 1]])
(fn [n]
  (nth (iterate 
         (fn [pre] 
           (vec 
             (concat 
               [1] 
               (map (fn [[f s]] (+ f s)) (partition 2 1 pre)) 
               [1])))
         [1])
       (dec n)))
