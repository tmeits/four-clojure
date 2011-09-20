;;; Alioth binary-tree benchmark
;;; http://groups.google.com/group/clojure/browse_thread/thread/f0303a9e00b38529

(ns btb.core)

(defn build-tree
  [item depth]
  (when (< 0 depth)
    (let [i (* 2 item)
          d (dec depth)]
      [item (build-tree (dec i) d) (build-tree i d)])))
(defn print-tree
  [tree]
  (println "tree:"))
(defn read-tree
  [coll]
  nil)
(defn check-node
  [z]
  (if z
    (+ (z 0) (check-node (z 1)) (- (check-node (z 2))))
    0))

(defn iterate-trees
  [mx mn d]
  (let [iterations (bit-shift-left 1 (+ mx mn (- d)))]
    (println (* 2 iterations) "\ttrees of depth" d "\tcheck:"
             (reduce + (map (fn [i]
                              (+ (check-node (build-tree i d))
                                 (check-node (build-tree (- i) d))))
                            (range 1 (inc iterations)))))))

(defn main
  [max-depth]
  (let [min-depth 4
        str-depth (inc max-depth)]
    (let [tree (build-tree 0 str-depth)
          x    (check-node tree)]
      (println "stretch tree of depth" str-depth "\tcheck:" x))
    (let [long-lived-tree (build-tree 0 max-depth)]
      (doseq d (range min-depth str-depth 2)
        (iterate-trees max-depth min-depth d))
      (println "long lived tree of depth" max-depth "\tcheck:"
               (check-node long-lived-tree)))))
(defn main-min
  [max-depth]
  (let [min-depth 4
        str-depth (inc max-depth)]
    (let [tree (build-tree 0 str-depth)
          x    (check-node tree)]
      (println "stretch tree of depth" str-depth "\tcheck:" x))
    ))

;; (time (main 16))

;; The code is available on Git: https://github.com/tmeits/four-clojure
;; http://inclojurewetrust.blogspot.com/2009/11/tail-recursion-and-function-composition.html
;; https://gist.github.com/1221519
(def *count* (atom 0))

(defstruct node :children)

(defn make-node [children] "ha ha ha"
  (swap! *count* inc)
  (struct node children))

(defn make-tree [depth]
  (if (> depth 1)
    (make-node [(make-tree (dec depth)) (make-tree (dec depth))])
    (make-node [])))

(let [depth (if *command-line-args*
              (Integer/parseInt (first *command-line-args*))
              10)]
  (make-tree depth)
  (println @*count*))


