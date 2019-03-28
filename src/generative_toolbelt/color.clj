(ns generative-toolbelt.color
    "A set of function to manage Colors easily."
    (:require [quil.core :as quil]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Colors
(defn palette 
   "IMPROVE/CONTROL returns a sequence of Colors." 
    ([]
        (vector {:h 100, :s 50, :v 50},
                {:h  80, :s 50, :v 50},
                {:h  60, :s 50, :v 50},
                {:h  40, :s 50, :v 50},
                {:h  20, :s 50, :v 50}))
    ([ind]
        (nth (palette) ind)))



