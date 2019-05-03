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





(defn save-stroke-settings 
    "Saves the current stroke settings into a map."
    ([]
        {:stroke (quil/current-stroke) :fill (quil/current-fill)})

    ([strokeWeight]
        {:stroke (quil/current-stroke) :stroke-weight strokeWeight :fill (quil/current-fill)}))
        

(defn set-stroke-settings
    "Sets all the previously saved stroke settings"
    [{:keys [stroke stroke-weight fill]}]
        (quil/stroke stroke)
        (quil/fill fill)
        (when (not (nil? stroke-weight))
            (quil/stroke-weight stroke-weight)))




