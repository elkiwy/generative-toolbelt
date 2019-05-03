(ns generative-toolbelt.circle
    "A set of functions to create, manage, and draw Circles.
     Circles are sets with a Point and a number like this: {:c {:x 100 :y 100} :r 50}."
    (:require [quil.core :as quil]
              [generative-toolbelt
                  [utils :as gt-utils]
                  [point :as gt-point]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Circle
(defn make-circle
    "Creates a Circle with a Point and an absolute value.
     Returns a Circle."
    [c r]
    {:c c :r r})


(defn mkcr
    "Creates a Circle with canvas-relative coordinates.
     Returns a Circle."
    [cx cy r]
    (make-circle (gt-point/make-point (gt-utils/w cx) (gt-utils/h cy)) (gt-utils/w r)))


(defn point-on-circle 
    "Creates a Point from a Circle and an angle in radians.
     Returns a Point."
    [{:keys [c r]} angle]
    (gt-point/make-point 
        (+ (:x c) (* r (quil/cos angle)))
        (+ (:y c) (* (* -1 r) (quil/sin angle)))))


(defn draw-circle
    "Draws a Circle into the current graphics canvas."
    [c]
    (quil/ellipse (:x (:c c)) (:y (:c c)) (:r c) (:r c)))


(defn points-on-circle
    "Returns a sequence of `n` Points from a Circle.
     `from` and `to` can be defined as angles in degrees to target only a
     certain portion of the Circle instead of the full circle."
    ([n circle]
        (let [step      (/ (quil/radians 360) n)
              angles    (map #(* step %) (range (+ 1 n)))]
            (map #(point-on-circle circle %) angles)))

    ([n circle from to]
        (let [step      (/ (quil/radians (- to from)) n)
              angles    (map #(+ (quil/radians from) (* step %)) (range (+ n 1)))]
            (map #(point-on-circle circle %) angles))))


(defn circle-move-by-vector
    "TODO"
    {:added "0.1.2"}
    [circle vec]
    (let [center    (:c circle)
          newCenter (gt-point/point-move-by-vector center vec)]
        (make-circle newCenter (:r circle))))


(defn circle-scale-radius
    "TODO"
    {:added "0.1.2"}
    [circle scale]
    (make-circle (:c circle) (* (:r circle) scale)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ellipse
(defn make-ellipse 
    "Creates a Ellipse with absolutes coordinates.
     Returns a Ellipse."
    [c r1 r2]
    {:c c :r1 r1 :r2 r2})


(defn mkel 
    "Creates a Ellipse with canvas-relative coordinates.
     Returns a Ellipse."
    [cx cy r1 r2]
    (make-ellipse (gt-point/mkpt cx cy) (gt-utils/w r1) (gt-utils/h r2)))


(defn point-on-ellipse 
    "Creates a Point from a Ellipse and an angle in radians.
     Returns a Point."
    [{:keys [c r1 r2]} angle]
    (gt-point/make-point 
        (+ (:x c) (* r1 (quil/cos angle)))
        (+ (:y c) (* (* -1 r2) (quil/sin angle)))))


(defn points-on-ellipse
    "Returns a sequence of `n` Points from a Ellipse.
     `from` and `to` can be defined as angles in degrees to target only a
     certain portion of the Ellipse instead of the full circle."
    ([n ellipse]
        (let [step (/ (quil/radians 360) n)
              angles (map #(* step %) (range (+ n 1)))]
            (map #(point-on-ellipse ellipse %) angles)))

    ([n ellipse from to]
      ;Include mode
        (let [step (/ (quil/radians (- to from)) n)
              angles (map #(+ (quil/radians from) (* step %)) (range (+ n 1)))]
            (map #(point-on-ellipse ellipse %) angles))))




