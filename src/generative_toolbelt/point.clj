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
    (quil/atan2 (- (:y b) (:y a)) (- (:x b) (:x a))))


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
        (+ (:y point) (* (:len vect) (quil/sin (:dir vect))))))


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
    (when (not-nil? drawOptions)
        (processDrawOptions drawOptions))
    (quil/line (:x (:a l)) (:y (:a l)) (:x (:b l)) (:y (:b l))))



