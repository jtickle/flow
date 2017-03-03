(ns flow.core
  (:require [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/flow/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

;;;;;;;;;;;;
;; Utilities

(defn cartesian-product [& colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (apply cartesian-product (rest colls))]
        (cons x more))))

;;;;;;;;;;;;;;;;
;; Retrieve Data

(defn rel-pos [[x y] dx dy]
  (list (+ x dx) (+ y dy)))

; 0-right; 1-down; 2-left; 3-up
(defn get-neighbor [grid pos n]
  (rel-pos pos
           (condp = n
             0 1
             2 -1
             0)
           (condp = n
             1 1
             3 -1
             0)))

(defn has-neighbor [grid pos n]
  (not (nil? (get grid (get-neighbor grid pos n)))))

(defn get-neighbors [grid pos]
  (map #(get-neighbor grid pos %)
       (range 4)))

(defn grid-size [grid]
  (map #(+ 1 (apply max (map % (keys grid))))
       (list first second)))

(defn get-occupied-nodes [grid]
  (filter #(not (nil? (:type %2))) grid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Empty Game Graph

(defn gen-grid [w h]
  (reduce #(assoc %1 %2 {:pos %2})
          {}
          (cartesian-product (range w) (range h))))

(defn gen-n-edges [grid n]
  (map (fn [v] #{v (get-neighbor grid v n)})
       (filter #(has-neighbor grid % n) (keys grid))))

(defn gen-edges [grid]
  (reduce #(assoc %1 %2 false)
          {}
          (mapcat (partial gen-n-edges grid) [0 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Random Structures and Connections

(defn make-random-structure [grid t]
  (let [[Mx My] (grid-size grid)]
    (assoc grid (list (rand-int Mx) (rand-int My)) {:type t})))

(defn make-random-structures [grid]
  (-> grid
      (make-random-structure "W")
      (make-random-structure "W")
      (make-random-structure "W")
      (make-random-structure "W")
      (make-random-structure "W")))

;;;;;;;;;;;;
;; Test Data

; A 2x2 grid
(def tinygrid (atom (gen-grid 2 2)))
(def tinyedges (atom (gen-edges @tinygrid)))

; A 20x20 grid
(def grid (atom (gen-grid 20 20)))
(def edges (atom (gen-edges @grid)))

;;;;;;;;;;;;;;;;;;;;
;; Output Formatting

(defn node-to-char [node]
  (if (nil? node)
    nil
    (let [t (:type node)]
      (if (nil? t)
        " "
        t))))

(defn h-edge-to-char [edge]
  (if (nil? edge)
    nil
    (if (true? edge)
      "-"
      " ")))

(defn v-edge-to-char [edge]
  (if (nil? edge)
    nil
    (if (true? edge)
      "|"
      " ")))

(defn make-node-row [grid edges y Mx]
  (filter (comp not nil?)
                 (interleave (map node-to-char
                                  (map #(get grid (list % y))
                                       (range 0 Mx)))
                             (map h-edge-to-char
                                  (map #(get edges #{(list % y) (list (+ % 1) y)})
                                       (range 0 Mx))))))

(defn make-edge-row [edges y Mx]
  (butlast (interleave (map v-edge-to-char
                                   (map #(get edges #{(list % y) (list % (+ y 1))})
                                        (range 0 Mx)))
                              (map (fn [v] " ")
                                   (range 0 Mx)))))

(defn make-grid [grid edges Mx My]
  (butlast (interleave (map #(make-node-row grid edges % Mx)
                            (range 0 My))
                       (map #(make-edge-row edges % Mx)
                            (range 0 My)))))

(defn show-grid [grid edges]
  (let [[Mx My] (grid-size grid)]
    (dorun (map #(println (string/join %))
                (reduce (fn [acc cur]
                          (conj acc (concat (list 
                                              (if (= 0 (mod (count acc) 2))
                                                (/ (count acc) 2)
                                                " ")
                                              " ") cur)))
                        '()
                        (make-grid grid edges Mx My))))
    (apply print (conj (range 0 Mx) " "))))
