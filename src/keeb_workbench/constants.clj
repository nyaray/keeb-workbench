(ns keeb-workbench.constants
  ; TODO import model stuff
  (:require [scad-clj.model :as scad])
  )

;;; TODO move params to constants file or something
(def module-depth 10)

(def plate-thickness 4)
(def plate-hole-depth (+ plate-thickness 0.01))

(def keyswitch-side 14.4)
(def keyswitch-pitch 19)

(def single-plate
  (->> (scad/cube keyswitch-side keyswitch-side plate-hole-depth)
       (scad/difference (scad/cube keyswitch-pitch keyswitch-pitch plate-thickness))))

