(ns generative-toolbelt.utils
    "A handy set of utility functions."
    (:require [quil.core :as quil]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Misc
(defn not-nil? 
    "Shortcut for (not (nil? x))
     Returns a boolean."
    [something]
    (not (nil? something)))


(defn print-to-system-out
    "Prints to the original System/out stream.
     Useful when working with multiple tools and
     you want to output always on the repl console."
    [s]
    (.println (System/out) s))


(defn n-times
    "Call a certain function N times on its output."
    [n f]
    (apply comp (repeat n f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Random - Using Quil's random function as a base.
(defn r-float
    "Get a random float number between two values."
    [low high]
    (quil/random low high))


(defn r-int
    "Gets a random int number between two values (Exclusive)."
    ([high]
        (int (quil/random high)))
    ([low high]
        (int (quil/random low high))))


(defn odds
    "Returns true if the random is inside the value range (0-1)"
    [value]
    (< (quil/random 1) value))


(defn r-gaussian 
    "Returns a random number from a gaussian distribution.
     `center` can be changed to define a custom mean.
     `ampl` can be changed to define the amplitude of the distribution."
    ([]
        (let [r (/ (quil/random-gaussian) 2)]
            (cond
                (< r -1)
                    (-1)
                (> r 1)
                    (1)
                :else
                    r)))
    ([center]
        (let [r (+ (/ (quil/random-gaussian) 2) center)]
            (cond
                (< r (- center 1))
                    (- center 1)
                (> r (+ center 1))
                    (+ center 1)
                :else
                    (+ center r))))
    ([center ampl]
        (let [r (+ (* (/ (quil/random-gaussian) 2) ampl) center)]
            (cond
                (< r (- center ampl))
                    (- center ampl)
                (> r (+ center ampl))
                    (+ center ampl)
                :else
                    r))))


(defn r-gaussian-abs
    "Returns a random number from a positive half gaussian.
     See r-gaussian for more info."
    ([] 
        (quil/abs (r-gaussian)))
    ([center] 
        (quil/abs (r-gaussian center)))
    ([center ampl] 
        (quil/abs (r-gaussian center ampl))))


(defn r-normal
    "Returns a random number from a normal distribution.
     defaults return a float from 0 to 1.
     `to` can be defined to get a random float from 0 to `to` (Exclusive).
     `from` can be defined to get a random float from `from` (inclusive) to `to` (exclusive)."
    ([] 
        (quil/random 1))
    ([to]
        (* (quil/random 1) to))
    ([from to]
        (let [r (- to from)]
        (+ (r-normal r) from))))


(defn r-normal-int
    "Returns a random INTEGER number from a normal distribution.
     See r-normal for more info."
    ([to]
        (int (r-normal to)))
    ([from to]
        (int (r-normal from to))))


(defn r-seed
    "Sets the seed for the random functions."
    [seed]
    (quil/random-seed seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Canvas Operations
(defn w 
    "Gets the Width of the canvas.
     `n` is an optional float value to multiply to the canvas width."
    ([] 
        (quil/width))
    ([n] 
        (* (quil/width) n)))


(defn h  
    "Gets the Height of the canvas.
     `n` is an optional float value to multiply to the canvas height."
    ([] 
        (quil/height))
    ([n] 
        (* (quil/height) n)))


(defn in-canvas? 
    "Checks if a Point is inside the canvas or not.
     Returns a boolean."
    [point]
    (and (< (:x point) (w)) (> (:x point) 0) (< (:y point) (h)) (> (:y point) 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Draw operations
(defn process-draw-options
    "Process a drawing options dictionary."
    [drawOpt]
    (quil/color-mode :hsb 360 100 100 1.0)
    (let [f (:fill drawOpt)
          s (:stroke drawOpt)
          sw (:stroke-weight drawOpt)
          bm (:blend-mode drawOpt)]
        (when (not-nil? bm)
            (quil/blend-mode bm))
        (when (not-nil? s)
            (quil/stroke (mod (nth s 0) 360) (nth s 1) (nth s 2) (nth s 3)))
        (when (not-nil? f)
            (quil/fill (mod (nth f 0) 360) (nth f 1) (nth f 2) (nth f 3)))
        (when (not-nil? sw)
            (if (= sw -1)
                (quil/no-stroke)
                (quil/stroke-weight sw)))))


(defn reset-draw-options
    "Resets the drawing options to default values."
    []
    (quil/color-mode :hsb 360 100 100 1.0)
    (quil/stroke 0 0 0 0.5)
    (quil/stroke-weight 1)
    (quil/blend-mode :blend)
    (quil/fill 0 0 0 0.0))


(defn reset-blend-mode
    "Resets the blend mode to default."
    []
    (quil/blend-mode :blend))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Handy Math
(defn margin-range
    "Returns a sequence of `N` element going from `A` to `B`.
     `MARGIN` defines a certain margin from A and B."
    [A B N MARGIN]
    (let [size     (- B A)
          ratio    (/ size (- size (* MARGIN 2)))
          firstVal (+ A MARGIN)]
      (map #(+ firstVal (/ (* size (/ % (- N 1))) ratio)) (range N))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sequences operations
(defn replace-first 
    "Returns a new sequence from `SEQ` with the first element replaced with `NEW`."
    [SEQ NEW]
    (conj (rest SEQ) NEW))


(defn replace-last
    "Returns a new sequence from `SEQ` with the last element replaced with `NEW`."
    [SEQ NEW]
    (reverse (replace-first (reverse SEQ) NEW)))


(defn replace-first-and-last
    "Returns a new sequence from `SEQ` with the first and last element replaced with `NEWF` and `NEWL`."
    [SEQ NEWF NEWL]
    (let [step1 (replace-first SEQ NEWF)]
        (replace-last step1 NEWL)))


(defn vector-to-seq
    "**IMPROVE/CONTROL** Converts a Vector to a Sequence."{:doc/format :markdown}
    [v]
    (apply list v))


(defn seq-to-vector
    "**IMPROVE/CONTROL** Converts a Sequence to a Vector."{:doc/format :markdown}
    [s]
    (into [] s))


(defn replace-nth-seq
    "**IMPROVE/CONTROL** Replace the nth element from a sequence."{:doc/format :markdown}
    [s ele ind]
    (let [v           (seq-to-vector s)
          replacedV   (assoc v ind ele)]
        (vector-to-seq replacedV)))


(defn replace-pos-seq
    "**IMPROVE/CONTROL** Replace an element inside a sequence.
     `position` is a 0-1 float that indicates the relative position of the element to replace."{:doc/format :markdown}
    [s ele position]
    (let [ind         (int (* (count s) position))
          v           (seq-to-vector s)
          replacedV   (assoc v ind ele)]
        (vector-to-seq replacedV)))




