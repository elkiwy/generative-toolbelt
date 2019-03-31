(ns generative-toolbelt.path
    "A set of functions to create, manage, and draw Paths.
     Paths are lists of Points like this: ({:x 100 :y 100} {:x 200 :y 100} {:x 300 :y 100} {:x 300 :y 200} )."
    (:require [quil.core :as quil]
              [generative-toolbelt
                  [utils :as gt-utils]
                  [point :as gt-point]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Drawing operations
(defn draw-path-fill
    "Draw a Path with only the fill option"
    [pointList & [drawOptions]]
    (let [gr (quil/create-graphics (- (gt-utils/w) 1) (- (gt-utils/h) 1))]
        (quil/with-graphics gr
            (quil/color-mode :hsb 360 100 100 1.0)
            (quil/fill
                (nth (:fill drawOptions) 0)
                (nth (:fill drawOptions) 1)
                (nth (:fill drawOptions) 2)
                1.0)
            (quil/begin-shape)
            (doseq [point pointList]
                (quil/vertex
                    (min (- (w) 1) (max 0 (:x point)))
                    (min (- (h) 1) (max 0 (:y point)))))
            (quil/end-shape))
        (quil/tint 255 (nth (:fill drawOptions) 3))
        (quil/image gr 0 0)))

(defn draw-path-stroke
    "Draw a Path with only the stroke operation"
    [pointList & [drawOptions]]
    (let [gr (quil/create-graphics (- (gt-utils/w) 1) (- (gt-utils/h) 1))]
        (quil/with-graphics gr
            (quil/color-mode :hsb 360 100 100 1.0)
            (quil/fill nil)
            (quil/stroke
                (nth (:stroke drawOptions) 0)
                (nth (:stroke drawOptions) 1)
                (nth (:stroke drawOptions) 2)
                (nth (:stroke drawOptions) 3))
            (quil/stroke-weight (:stroke-weight drawOptions))
            (quil/blend-mode :replace)
            (quil/begin-shape)
            (doseq [point pointList]
                (quil/vertex
                    (min (- (gt-utils/w) 1) (max 0 (:x point)))
                    (min (- (gt-utils/h) 1) (max 0 (:y point)))))
            (quil/end-shape))
        (quil/image gr 0 0)))


(defn draw-path
    "Draw a Path with both fill and stroke options."
    [pointList & [drawOptions]]
    (let [gr (quil/create-graphics (- (gt-utils/w) 0) (- (gt-utils/h) 0))]
        (quil/with-graphics gr
            (quil/color-mode :hsb 360 100 100 1.0)
            (quil/fill (nth (:fill drawOptions) 0)
                (nth (:fill drawOptions) 1)
                (nth (:fill drawOptions) 2)
                (nth (:fill drawOptions) 3))
            (quil/stroke (nth (:stroke drawOptions) 0)
                (nth (:stroke drawOptions) 1)
                (nth (:stroke drawOptions) 2)
                (nth (:stroke drawOptions) 3))
            (quil/stroke-weight (:stroke-weight drawOptions))
            (quil/blend-mode :replace)
            (quil/begin-shape)
            (doseq [point pointList]
                (quil/vertex
                    (min (- (gt-utils/w) 0) (max 0 (:x point)))
                    (min (- (gt-utils/h) 0) (max 0 (:y point)))))
            (quil/end-shape))
        (quil/image gr 0 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Remove the first element of a path
(defn line-to-path
    "Convert a Line structure to a Path object
    `sectionCount` define how many point the path is made of."
    [line sectionCount]
    (let [step   (/ 1 sectionCount)
          cuts   (map #(* step %) (range (inc sectionCount)))]
        (map #(gt-point/point-between (:a line) (:b line) %) cuts)))


(defn dispose-path
    "Add random noise to ALL Path's points.
    `noiseX` and `noiseY` defines the aomplitude of the gaussian to generate the noise with."
    [p noiseX noiseY]
    (map #(gt-point/make-point (gt-utils/r-gaussian (:x %) noiseX) (gt-utils/r-gaussian (:y %) noiseY)) p))


(defn dispose-fixed-path
    "Add random noise to all the inner path's points, keeping the first and last point stable.
    `noiseX` and `noiseY` defines the aomplitude of the gaussian to generate the noise with."
    [p noiseX noiseY]
    (let [disposedPath (dispose-path p noiseX noiseY)]
        (gt-utils/replace-first-and-last disposedPath (first p) (last p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chaikin smoothing on paths
(defn chaikin-segment
    "Subfunction of `generative-toolbelt.path/chaikinSubdivision.
     Takes two Points and a chaikparam.
     Returns a list of two interpolated points."
    [pointA pointB segmentationPoint]
    (let [lowMargin  segmentationPoint
          highMargin (- 1 lowMargin)]
        (list (gt-point/point-between pointA pointB lowMargin) (gt-point/point-between pointA pointB highMargin))))



(defn chaikin-path
    "Approximate a Path with the chaikin subdivision alorithm, smoothing the original Path.
     `iteration` defines the desired iterations number.
     `chaikparam` defines the chaikin parameter to use.
        A single number will define a fixed parameter through all iterations, a list of two point will define a range in which a random value will be extracted for each iteration.
     Returns a new Path."
    [path iteration chaikParam]
    (if (> iteration 0)
        ;Recursive step
        (let [segmentationPoint   (if (number? chaikParam) chaikParam (gt-utils/r-normal (first chaikParam) (second chaikParam)))
              linesList           (partition 2 1 path)
              newLineList         (map #(chaikin-segment (first %) (second %) segmentationPoint) linesList)
              curve               (chaikinSubdivision (flatten newLineList) (- iteration 1) chaikParam)]
            ;Keep the first and last point where they are
            (gt-utils/replace-first-and-last curve (first path) (last path)))
        ;When i completed all the iteration I just return the point list
        path))
