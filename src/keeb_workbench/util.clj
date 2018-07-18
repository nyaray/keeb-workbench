(ns keeb-workbench.util
  (:require [scad-clj.scad :refer :all])
  (:require [scad-clj.model :refer :all])
  ;(require [moyamsek.core :refer :all :reload :all])
  (:require [keeb-workbench.constants
             :as cs
             :refer [plate-thickness
                     keyswitch-side
                     keyswitch-pitch]])
  )

;;
;; joiner pieces
;;

(defn joiner-dimensions [mode]
  (let [short-side (/ (- keyswitch-pitch keyswitch-side) 2)
        long-side keyswitch-pitch
        thin-side (/ short-side 2)]
    (cond
     (= mode [:vertical :thin])   [thin-side long-side]
     (= mode [:horizontal :thin]) [long-side thin-side]
     (= mode :vertical)           [short-side long-side]
     (= mode :horizontal)         [long-side short-side]
     :else (throw (new IllegalArgumentException
                       (str "bad dimension arguments: " mode))))))

(defn joiner-offsets [position joiner-side]
  (let [half-pitch (- (/ keyswitch-pitch 2) (/ joiner-side 2))
        half-side 0]
    (cond
     (= position :front) [0 half-pitch]
     (= position :back)  [0 (- half-pitch)]
     (= position :left)  [(- half-pitch) 0]
     (= position :right) [half-pitch 0]
     ; TODO merge with previous equivalents or change something?
     (= position :front-out) [0 half-pitch]
     (= position :back-out)  [0 (- half-pitch)]
     (= position :left-out)  [(- half-pitch) 0]
     (= position :right-out) [half-pitch 0]
     :else (throw (new IllegalArgumentException
                       (str "bad offset arguments: "
                            position ", "
                            joiner-side))))))

(defn switch-joiner-distance [position joiner-width joiner-height]
  (cond
   (or (= position :front-out) (= position :back-out))  (- joiner-height 4)
   (or (= position :left-out)  (= position :right-out)) (- joiner-width 4)
   (or (= position :front)     (= position :back))      joiner-height
   (or (= position :left)      (= position :right))     joiner-width
   :else (throw (new IllegalArgumentException
                     (str "bad distance arguments: "
                          position ", "
                          joiner-width ", "
                          joiner-height)))))

(defn switch-joiner [mode position]
  (let [[joiner-width joiner-height] (joiner-dimensions mode)
        offset-distance (switch-joiner-distance position joiner-width joiner-height)
        [joiner-offset-x joiner-offset-y] (joiner-offsets position
                                                          offset-distance)]
    (->> (cube joiner-width joiner-height plate-thickness)
         (translate [joiner-offset-x joiner-offset-y 0]))))

(defn- switch-corner-opts [opts]
  (let [corner-side (- keyswitch-pitch keyswitch-side)]
    (cond (and (coll? opts)
               (= (opts 1) :thick)) [:thick (/ corner-side 2) (opts 0)]
          :else [:normal (/ corner-side 4) opts])))

(defn switch-corner-offsets [mode position r]
  (let [r-half (* r 0.5)
        r-dub (* r 2)
        pitch-half (/ keyswitch-pitch 2)]
    ;(print (str "switch-corner-offsets: " mode " " position " " r "\n"))
    (cond
     (= mode :thick)
     (cond
      (= position :front-left)  [(- r-half pitch-half) (- pitch-half r-half)]
      (= position :front-right) [(- pitch-half r-half) (- pitch-half r-half)]
      (= position :back-left)   [(- r-half pitch-half) (- r-half pitch-half)]
      (= position :back-right)  [(- pitch-half r-half) (- r-half pitch-half)])
     (= position :front-left)  [(- r pitch-half) (- pitch-half r)]
     (= position :front-right) [(- pitch-half r) (- pitch-half r)]
     (= position :back-left)   [(- r pitch-half) (- r pitch-half)]
     (= position :back-right)  [(- pitch-half r) (- r pitch-half)]
     (= position :front-left-out)  [(- r-half pitch-half) (- pitch-half r-half)]
     (= position :front-right-out) [(- pitch-half r-half) (- pitch-half r-half)]
     (= position :back-left-out)   [(- r-half pitch-half) (- r-half pitch-half)]
     (= position :back-right-out)  [(- pitch-half r-half) (- r-half pitch-half)]
     :else (throw (new IllegalArgumentException
                       (str "bad corner arguments: "
                            position)))
     )))

(defn switch-corner [opts]
  (let [[mode r position] (switch-corner-opts opts)
        [off-x off-y] (switch-corner-offsets mode position r)]
    (->> (cube r r plate-thickness)
         (translate [off-x off-y 0])
         )))

;;
;; lego pieces/holes
;;

(def lego-pin-radius (/ 5.2 2))
(def lego-pin-countersink 0.9)
(def lego-pin-countersink-radius (/ 6.4 2))

(defn lego-pin-hole [material-thickness]
  (let [cylinder-fn 30
        countersink (->> (cylinder lego-pin-countersink-radius
                                   lego-pin-countersink)
                         (with-fn cylinder-fn))]
    (union
     (->> (cylinder lego-pin-radius (+ material-thickness 0.01))
          (with-fn cylinder-fn))
     ;countersink
     (->> countersink
          (translate [0 0 (+ (/ material-thickness -2)
                             (/ lego-pin-countersink 2))]))
     (->> countersink
          (translate [0 0 (- (/ material-thickness 2)
                             (/ lego-pin-countersink 2))]))
     )))

;(->> (lego-pin-hole plate-thickness)
;     (write-scad)
;     (spit "things/lego-pin-hole.scad"))

;;
;; arc placement
;;

(def arc-radius 150) ; was 180
(def arc-keys (range 0 3))

(defn arc-place [pos shape]
  ; TODO vary slice count by arc-radius??
  ; TODO extract slice-to-radian function
  ; TODO re-write to multimethod taking one of [:slice pos] or [:angle rad]
  (let [slices 24
        slice-theta (/ Math/PI slices)
        slice-offset-base 1
        slice-offset (+ pos (/ slices 2) slice-offset-base)
        theta (* slice-offset slice-theta)]
    (->> shape
         (rotate [0 0 (- theta (/ Math/PI 2))])
         (translate [(* arc-radius (Math/cos theta))
                     (* arc-radius (Math/sin theta))
                     0]))))

(defn arc-joiner [[a b]]
  (hull (->> (switch-joiner :vertical :left)  (arc-place a))
        (->> (switch-joiner :vertical :right) (arc-place b))))

;;
;; support walls
;;

(defn make-pillar [shape]
  (hull shape
        (->> shape
             (project)
             (extrude-linear {:height 1})
             (translate [0 0 0.5]))))

