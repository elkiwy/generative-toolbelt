(ns generative-toolbelt.triangle
    "A set of functions to create, manage, and draw Triangles.
     Triangles are sets with three Points like this: {:a {:x 100 :y 100} :b {:x 100 :y 100} :c {:x 100 :y 100}}."
    (:require [quil.core :as quil]
              [generative-toolbelt
                  [utils :as gt-utils]
                  [point :as gt-point]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Triangle
(defn make-triangle
    "Creates a Triangle with three Points.
     Returns a Triangle."
    [a b c]
    {:a a :b b :c c})


(defn mktr
    "Creates a Triangle with canvas-relative coordinates.
     Returns a Triangle."
    [ax ay bx by cx cy]
    {:a (gt-point/mkpt ax ay) :b (gt-point/mkpt bx by) :c (gt-point/mkpt cx cy)})


(defn draw-triangle
    "Draws a Triangle into the current graphics canvas."
    [{:keys [a b c]} & [drawOptions]]
    (when (gt-utils/not-nil? drawOptions)
        (gt-utils/process-draw-options drawOptions))
    (quil/triangle (:x a) (:y a) (:x b) (:y b) (:x c) (:y c)))


(defn draw-triangles
    "Draws a sequence of Triangles into the current graphics canvas.
     Slightly faster than draw-triangle when a lot of triangles needs to be drawn with the same `drawOptions`."
    [triangles & [drawOptions]]
    (when (gt-utils/not-nil? drawOptions)
        (gt-utils/process-draw-options drawOptions))
    (doseq [tri triangles]
        (draw-triangle tri)))


(defn triangle-centroid
    "Calculate the centroid of a Triangle.
     Returns a Point."
    [tri]
    (gt-point/make-point (/ (+ (:x (:a tri)) (:x (:b tri)) (:x (:c tri))) 3)
                         (/ (+ (:y (:a tri)) (:y (:b tri)) (:y (:c tri))) 3)))


(defn move-triangle-by
    "Translate a Triangle by a certain distance Vector.
     Returns a new Triangle."
    [tri vect]
    (make-triangle
        (gt-point/move-point-by (:a tri) vect) 
        (gt-point/move-point-by (:b tri) vect) 
        (gt-point/move-point-by (:c tri) vect)))

(defn get-triangle-guide
    "Gets an alternative form of the Triangle structure that tells where the longest side is and what are its opposite sides."
    [tri]
    (let [ab (gt-point/points-distance (:a tri) (:b tri))
          bc (gt-point/points-distance (:b tri) (:c tri))
          ca (gt-point/points-distance (:c tri) (:a tri))
          mx (max ab bc ca)]
    (cond
      (= ab mx)
        {:l1 (:a tri), :l2 (:b tri), :opposite (:c tri)}
      (= bc mx)
        {:l1 (:b tri), :l2 (:c tri), :opposite (:a tri)}
      (= ca mx)
        {:l1 (:c tri), :l2 (:a tri), :opposite (:b tri)})))


(defn split-triangle
    "Splits a Triangle by its longest side, creating two new Triangles.
     Returns a list of two Triangles."
    [tri]
    (let [guide    (get-triangle-guide tri)
          midHypo  (gt-point/intermediate-point (:l1 guide) (:l2 guide) (gt-utils/r-gaussian 0.5 0.25))]
        (list 
            (make-triangle (:opposite guide) midHypo (:l1 guide))
            (make-triangle (:opposite guide) midHypo (:l2 guide)))))


(defn split-recursive
    "Recursivly splits a Triangle based on the current recursion depth and the odds to get split.
     `t` is a Triangle.
     `depth` is the current recursion step.
     `maxdepth` is the target recursion level.
     `oddsToSplit` is a 0-1 float indicating the odds to get split.
     Returns a non-flattened list of Triangles."
    [t depth maxdepth oddsToSplit]
    (if (or (>= depth maxdepth) (gt-utils/odds oddsToSplit))
        (list t)
        (map #(split-recursive % (inc depth) maxdepth) (split-triangle t))))


(defn triangle-subdivision
    "Split a list of Triangles recursively.
     `maxdepth` is the maximum depth level we want to reach with the recursion.
     `oddsToSplit` is a 0-1 float indicating the odds to get split.
     Returns a flattened list of Triangles."
    [triangles maxdepth oddsToSplit]
    (flatten (map #(split-recursive % 0 maxdepth oddsToSplit) triangles)))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Mesh Triangulation
(defn circumscribe-triangle
    "Calculates a special Circle structure that circumscribe a Triangle.
     NOTE: The Circlo structure that this function returns is different from generative-toolbelt.circle/make-circle since it has been optimize for the triangulation function.
     Returns a special Circle structure."
    [{:keys [a b c]}]
    (let [ax (:x a) ay (:y a)
          bx (:x b) by (:y b)
          cx (:x c) cy (:y c)
          A (- bx ax)
          B (- by ay)
          C (- cx ax)
          D (- cy ay)
          E (+ (* A (+ ax bx)) (* B (+ ay by)))
          F (+ (* C (+ ax cx)) (* D (+ ay cy)))
          G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
        (if (> (quil/abs G) 0.000001)
            (let [cx (/ (- (* D E) (* B F)) G)
                  cy (/ (- (* A F) (* C E)) G)
                  dx (- cx ax)
                  dy (- cy ay)
                  r  (+ (quil/pow dx 2) (quil/pow dy 2))]
                {:x cx :y cy :radius-squared r})
            (println "POINTS ARE COLLINERAR, IMPLEMENT THIS!"))))


(defn edges
    "Scompose a Triangle into a vector of Lines."
    [{:keys [a b c]}] 
    [(gt-point/make-line a b) (gt-point/make-line b c) (gt-point/make-line c a)])

(defn contains-point?
    "Checks if a Point is inside the special Circle structure or not.
     Returns a boolean."
    [{:keys [x y radius-squared]} point]
    (let [distance-squared (+ (quil/pow (- x (:x point)) 2) (quil/pow (- y (:y point)) 2))]
        (< distance-squared radius-squared)))


(defn outer-edges
    "Gets the outer edges of a list of Triangles.
     Returns a list of Lines."
    [triangles]
    (let [all-edges    (mapcat edges triangles)
          ;Matches is a function that filter all the edges with a specific edge
          matches      (fn [edge] (filter #{edge (gt-point/reverse-line edge)} all-edges))
          appears-once (fn [edge] (= (count (matches edge)) 1))]
        (filter appears-once all-edges)))


(defn make-new-triangles
    "Creates new Triangles from a list of Triangles and a Point.
     Returns a list of Triangles."
    [containers point]
    ;Take all the outer edges from cointainers, creates a new triangle with each edge and the new point, and place everything in a set so douplicates will be automatically removed.
    (->> containers
         outer-edges
         (map (fn [{:keys [a b]}] (make-triangle a b point)))
         set))


(defn add-point-to-triangles
    "Add a point to the triangulated mesh.
     Takes a list of Triangles and a Point.
     Returns a list of Triangles."
    [triangles point]
    ;Containers = all the triangles which circumscription cointains point
    (let [containers    (filter #(contains-point? (circumscribe-triangle %) point) triangles)
          ;Creates the new set of triangles to replace the old ones
          new-triangles (make-new-triangles containers point)]
        ;Remove the old triangles and insert the new ones
        (clojure.set/union (clojure.set/difference triangles containers) new-triangles)))


(defn bounds
    "Returns the bounding rectangle Point from a list of Points.
     Returns a vector of 4 Points."
    [points]
    (let [minx (->> points (map :x) (apply min) (+ -1000))
          maxx (->> points (map :x) (apply max) (+  1000))
          miny (->> points (map :y) (apply min) (+ -1000))
          maxy (->> points (map :y) (apply max) (+  1000))]
        [(gt-point/make-point minx miny) (gt-point/make-point maxx miny) (gt-point/make-point minx maxy) (gt-point/make-point maxx maxy)]))


(defn is-from-bounds?
    "Checks if a Triangle is on a bounding box or not.
     Returns a boolean."
    [boundSet {:keys [a b c]}]
    (some boundSet [a b c]))


(defn triangulate
    "Creates a triangulated mesh from a list of Points.
     Full explanation paper here: http://paulbourke.net/papers/triangulate/
     Returns a list of Triangles."
    [points]
    (let [[tl tr bl br]  (bounds points)
          initial        #{(make-triangle tl tr bl) (make-triangle bl tr br)}
          with-bounds    (reduce add-point-to-triangles initial points)]
        ;Remove the bounds and return
        (remove #(is-from-bounds? #{tl tr bl br} %) with-bounds)))



