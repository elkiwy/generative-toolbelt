(ns generative-toolbelt.point
    "A set of function to create, manipulate, and draw Points and also Vector structures.
    Points are structured as maps like this: {:x 100 :y 100}.
    Vectors are strucutred as maps like this: {:len 100 :dir 60}"
    (:require [quil.core :as quil]
              [generative-toolbelt.utils :as gt-utils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Vector
(defn make-vec
    "Creates a Vector structure with length `l` and direction `d` in radians."
    [l d] 
    {:len l :dir d})


(defn flip-dir
    "Flip the direction of a by 180 degrees vector.
     Returns a Vector structure."
    [vect]
    (make-vec (:len vect) (+ (:dir vect) 3.14159)))


(defn tweak-len
    "Changes the length of the vector multiplicating its length by `value`.
     Returns a Vector structure."
    [vect value]
    (make-vec (* (:len vect) value) (:dir vect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points
(defn make-point
    "Creates a Point with absolutes coordinates.
     Returns a Point."
    [x y]
    {:x (float x) :y (float y)})


(defn mkpt
    "Creates a Point with canvas-relative coordinates.
     Returns a Point."
    [x y]
    (make-point (gt-utils/w x) (gt-utils/h y)))


(defn point-distance 
    "Calculate the distance between two Points.
     Returns a number."
    [a b]
    (let [a2 (quil/pow (- (:x b) (:x a)) 2)
          b2 (quil/pow (- (:y b) (:y a)) 2)]
        (quil/sqrt (+ a2 b2))))


(defn point-angle
    "Calculate the angle between two Points.
     Returns a number."
    [a b]
    (quil/atan2 (- (- (:y b) (:y a))) (- (:x b) (:x a))))


(defn point-between
    "Creates a new intermediate point between two Points
     `pos` is a floating number between 0 and 1 that defines
     the distance between the two points. 0 returns point `a`,
     1 returns point `b`, 0.5 returns the middle point between
     `a` and `b`, etc...
     Returns a Point."
    [a b pos]
    (make-point 
        (+ (:x a) (* pos (- (:x b) (:x a))))
        (+ (:y a) (* pos (- (:y b) (:y a))))))


(defn point-move-by-vector
    "Creates a new Point which is moved by a certain
     ammount in a certain direction defined by a Vector
     structure.
     Returns a Point."
    [point vect]
    (make-point 
        (+ (:x point) (* (:len vect) (quil/cos (:dir vect))))
        (+ (:y point) (* -1 (:len vect) (quil/sin (:dir vect))))))


(defn vector-to-point
    "Converts a Vector structure to a Point structure.
     Returns a Point."
    [v]
    (make-point
        (* (:len v) (quil/cos (:dir v)))
        (* (:len v) (quil/sin (:dir v)))))


(defn point-to-vector
    "Converts a Point structure to a Vector structure.
     Returns a Vector."
    [p]
    (make-vec 
        (point-distance (make-point 0.0 0.0) p)
        (point-angle (make-point 0.0 0.0) p)))


(defn draw-point
    "Draws a Point into the current graphics canvas."
    ([p]
        (quil/ellipse (:x p) (:y p) 3 3))
    ([p size]
        (quil/ellipse (:x p) (:y p) size size)))



(defn point-mirror 
    "**NEEDS UPDATE** Mirror a sequence of points vertically, horizontally, or both.
     `mode` can be `:h` for horizontal only, `:v` for vertical
     only, or `:hv` for both horizontal and vertical.
     Returns a sequence of Points." {:doc/format :markdown}
    [points mode]
    (let [flipX (fn [p] (- (gt-utils/w) (:x p)))
          flipY (fn [p] (- (gt-utils/h) (:y p)))
          normX (fn [p] (:x p))
          normY (fn [p] (:y p))]
        (cond
            (= mode :h)
                (mapcat #(vector 
                            (make-point (normX %) (normY %)) 
                            (make-point (flipX %) (normY %))
                        ) points)
            (= mode :v)
                (mapcat #(vector 
                            (make-point (normX %) (normY %)) 
                            (make-point (normX %) (flipY %))
                        ) points)
            (= mode :hv)
                (mapcat #(vector 
                            (make-point (normX %) (normY %)) 
                            (make-point (normX %) (flipY %))
                            (make-point (flipX %) (normY %))
                            (make-point (flipX %) (flipY %))
                        ) points)
            (= mode :diag)
                (mapcat #(vector 
                            (make-point (normX %) (normY %)) 
                            (make-point (flipX %) (flipY %))
                        ) points)
            :else
                points)))


(defn point-position-respect-line
    [p l]
    (let [aaa (* (- (:x (:b l)) (:x (:a l)))  (- (:y p) (:y (:a l))))
          bbb (* (- (:y (:b l)) (:y (:a l)))  (- (:x p) (:x (:a l))))  
          ccc (- aaa bbb)]
        (gt-utils/sign ccc)))


(defn shape-centroid
    [points]
    (let [sum-x (reduce + (map :x points))
          sum-y (reduce + (map :y points))
          length (count points)]
        (make-point (/ sum-x length) (/ sum-y length))))

(defn points-sort-clockwise-around-centroid
    [points]
    (when (> (count points) 0)
        (let [cent (shape-centroid points)
            ordered (sort #(> (point-angle cent %1) (point-angle cent %2)) points)]
            ordered)))


(defn points-move-by-vector
    [shape v]
    (if (<= (count shape) 2)
        shape
        (map #(point-move-by-vector % v) shape)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Line
(defn make-line
    "Creates a Line with absolutes coordinates.
     Returns a Line."
    [a b]
    {:a a :b b})


(defn mkln
    "Creates a Line with canvas-relative coordinates.
     Returns a Line."
    [ax  ay bx by]
    {:a (mkpt ax ay) :b (mkpt bx by)})


(defn reverse-line
    "Reverse a Line swapping its Points.
     Returns a Line."
    [{:keys [a b]}]
    (make-line b a))


(defn draw-line
    "Draws a Line into the current graphics canvas."
    [l & [drawOptions]]
    (when (gt-utils/not-nil? drawOptions)
        (gt-utils/process-draw-options drawOptions))
    (quil/line (:x (:a l)) (:y (:a l)) (:x (:b l)) (:y (:b l))))




(defn line-slope [{:keys [a b]}]
    (if (zero? (- (:x a) (:x b)))
        nil
        (/ (- (:y a) (:y b)) (- (:x a) (:x b)))))
 
(defn line-intersection [l1 l2]
    (let [a1 (:a l1)
          b1 (:b l1)
          a2 (:a l2)
          b2 (:b l2)
          slope1 (line-slope l1)
          slope2 (line-slope l2)]
        (if (= slope1 slope2)
            nil
            (cond
                (and (nil? slope1) (not (nil? slope2)))
                    (make-point (:x a1)
                        (+ (* (- (:x a1) (:x a2)) slope2) (:y a2)))
                (and (not (nil? slope1)) (nil? slope2))
                    (make-point (:x a2)
                        (+ (* (- (:x a2) (:x a1)) slope1) (:y a1)))
                :else
                (let [x (/ (- (+ (- (* slope1 (:x a1))
                                    (* slope2 (:x a2)))
                                 (:y a2))
                              (:y a1))
                           (- slope1 slope2))]

                    (make-point x (+ (* slope2 (- x (:x a2))) (:y a2))))))))



(defn segment-intersection [l1 l2]
    (if-let [crossing (line-intersection l1 l2)]
        (let [minx-l1 (min (:x (:a l1)) (:x (:b l1)))
              maxx-l1 (max (:x (:a l1)) (:x (:b l1)))
              in-x-l1 (<= minx-l1 (:x crossing) maxx-l1)

              miny-l1 (min (:y (:a l1)) (:y (:b l1)))
              maxy-l1 (max (:y (:a l1)) (:y (:b l1)))
              in-y-l1 (<= miny-l1 (:y crossing) maxy-l1)]
            (if (and in-x-l1 in-y-l1)
                crossing
                nil))
        nil))




