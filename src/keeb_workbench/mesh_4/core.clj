(ns keeb-workbench.mesh-4.core
  (:require [scad-clj.scad :refer :all])
  (:require [scad-clj.model :refer :all])
  (:use [keeb-workbench.constants])
  (:use [keeb-workbench.util])
  )

;;
;; parameters
;;

(def shell-depth 73)
(def thumb-depth-offset 49)

(def ncols 6) ; normally 6
(def nrows 4) ; normally 4

(def cols (range (- 0 (/ ncols 2)) (+ 0 (/ ncols 2))))
(def rows (reverse (range (- 1 (/ nrows 2)) (+ 1 (/ nrows 2)))))

(def c0 (first cols))
(def cn (last cols))
(def r0 (first rows))
(def rn (last rows))

(def cps (partition 2 1 cols))
(def rps (partition 2 1 rows))

; TODO: maybe inline as constant?!?
(defn- c2f-radius [c]
  ; column 2 finger radius
  (cond
   :else 55)) ; normal fallback is 55

(def row-angle (/ Math/PI 7.5)) ; the amount by which to rotate non-middle rows

;;
;; primitives
;;

(defn- pp-colorize [c r shape]
  (let [alpha 0.8
        c (case [c r]
               [c0 r0] [0.5 1 0.5 alpha] ; c0 r0 is green
               [0 0]   [1 1 0.5 alpha]   ; 0 0 is yellow
               [cn rn] [1 0.5 0.5 alpha] ; cn rn is red
               [0.8 0.5 0.8 alpha])]
    (color c shape)))

(defn- col-offset-x [c]
  (* c keyswitch-pitch))

(defn- col-offset-y [c]
  (cond (< c -2) (* -1 keyswitch-pitch)
        (= c -2) (* -0.75 keyswitch-pitch)
        (= c -1) (* -0.25 keyswitch-pitch)
        (= c 0) 0
        (= c 1) (* -0.5 keyswitch-pitch)
        (> c 1) (* -0.75 keyswitch-pitch)))

(defn- col-offset-z [c]
  (cond (< c -2) 3.5
        (= c -2) 2
        (= c -1) -0.25
        (= c 0) 0
        (= c 1) 0
        (> c 1) 2.5))

(defn- col-place [c shape]
  (translate [(col-offset-x c) 0 0] shape))

; NOTE: this is kind of a hack. life's too short etc...
(defn- pp-first-row [shape]
  (->> shape
       (rotate [row-angle 0 0])
       (translate [0 6 5])))

(defn- radius-place [c r shape]
  (let [shape-angle (+ (/ Math/PI -2) (* r row-angle))
        rotation-angle (+ (/ Math/PI 2) shape-angle)
        finger-radius (c2f-radius c)]
    (->> shape
         (rotate [rotation-angle 0 0 ])
         (translate [(col-offset-x c)
                     (* finger-radius (Math/cos shape-angle))
                     (* finger-radius (Math/sin shape-angle))]))))

(defn- above-surface [c r shape]
  (->> shape
       (translate [0 0 shell-depth])))

;;
;; joining primitives
;;

(def float-z -10)
(def float-y -6)
(def mesh-thumb-wall-z -10)

(def floater-left
  (->> (switch-joiner [:vertical :thin] :left) (translate [0 0 mesh-thumb-wall-z])))

(def floater-front
  (->> (switch-joiner :horizontal :front) (translate [0 (- float-y) 0])))

(def floater-front-left-out
  (->> (switch-corner :front-left-out) (translate [0 (- float-y) 0])))

(def floater-front-right-out
  (->> (switch-corner :front-right-out) (translate [0 (- float-y) 0])))

(def floater-back-left-out
  (->> (switch-corner :back-left-out) (translate [0 float-y 0])))

(def floater-back-right-out
  (->> (switch-corner :back-right-out) (translate [0 float-y 0])))

(def floater-back
  (->> (switch-joiner :horizontal :back) (translate [0 float-y 0])))

; TODO: (defn- joiner-n [translation] (...))
(def joiner-1 (->> (switch-corner :back-right-out) (translate [-22 -72 25])))
(def joiner-2 (->> (switch-corner :back-right-out) (translate [-18 -55 30])))
(def joiner-3 (->> (switch-corner :back-right-out) (translate [  0 -43 33])))
(def joiner-4 (->> (switch-corner :back-right-out) (translate [ 24 -44 20])))
(def joiner-5 (->> (switch-corner :back-right-out) (translate [ 38 -46 20])))

;;
;; assembly pipeline steps
;;

(defn- maybe [p f shape]
  (if p (f shape) shape))

(defn- pre-process [c r shape]
  (->> shape
       (pp-colorize c r)
       ;(maybe (= r r0) pp-first-row)
       ))

(defn- key-place [c r shape]
  (->> shape
       (radius-place c r)
       (above-surface c r)))

(defn- pp-col-offset [c r shape]
  (let [offset-y (col-offset-y c)
        offset-z (col-offset-z c)]
    (translate [0 offset-y offset-z] shape)))

(defn- post-process [c r shape]
  (->> shape
       (pp-col-offset c r)))

(defn- assemble [c r shape]
  (->> shape
       (pre-process c r)
       (key-place c r)
       (post-process c r)))

;;
;; supports
;;

(def support-nub-width 2.5)

(defn- support-nub-direction [position]
  (case position
    :left -1
    :right 1
    (throw (new IllegalArgumentException
                (str "bad support nub arguments: " position)))))

(defn- support-nub [position]
  (let [direction (support-nub-direction position)]
    (->> (cylinder (/ plate-thickness 2) support-nub-width)
         (translate [0 0 (/ support-nub-width 2)])
         (with-fn 40)
         (rotate [0 (/ Math/PI (* direction 2)) 0])
         (translate [(/ keyswitch-pitch (* direction 2)) 0 0]))))

(def mesh-support
  (union
   ; support mount points
   ;(for [r rows] (->> (support-nub :left) (assemble c0 r)))
   ;(for [r rows] (->> (support-nub :right) (assemble cn r)))
   ; front/back joining
   (for [c cols
         [a b] rps]
     (hull (->> (switch-joiner [:horizontal :thin] :back) (assemble c a))
           (->> (switch-joiner [:horizontal :thin] :front) (assemble c b))))
   ; side joining
   (for [[a b] cps
         r rows]
     (hull (->> (switch-joiner [:vertical :thin] :right) (assemble a r))
           (->> (switch-joiner [:vertical :thin] :left) (assemble b r))))
   ; gap filling
   (for [[c1 c2] cps
         [r1 r2] rps]
     (hull (->> (switch-corner :back-right-out) (assemble c1 r1))
           (->> (switch-corner :back-left-out) (assemble c2 r1))
           (->> (switch-corner :front-right-out) (assemble c1 r2))
           (->> (switch-corner :front-left-out) (assemble c2 r2))))
   ))

;;
;; housing
;;

(def pad-offset 1.15)
(def z-offset -6)
(def lateral-offset 2)

(defn- mesh-housing-front [float-z]
  (for [c cols]
    (let [floater (->> (switch-joiner :horizontal :front)
                       (translate [0 0 float-z]))]
      (union
       (->> (hull (->> (switch-joiner :horizontal :front) (assemble c r0))
                  (->> floater (assemble c r0))))
       (->> floater (assemble c r0) make-pillar)))))

(defn- mesh-housing-front-joiners [float-z]
  (for [[a b] cps]
    (let [base-a (->> (union (switch-corner :front-right)
                             (switch-corner :front-right-out))
                      (assemble a r0))
          base-b (->> (union (switch-corner :front-left)
                             (switch-corner :front-left-out))
                      (assemble b r0))
          floater-a (->> (union (switch-corner :front-right)
                                (switch-corner :front-right-out))
                         (translate [0 0 float-z])
                         (assemble a r0))
          floater-b (->> (union (switch-corner :front-left)
                                (switch-corner :front-left-out))
                         (translate [0 0 float-z])
                         (assemble b r0))]
      (union (hull (hull base-a floater-a) (hull base-b floater-b))
             (->> (hull floater-a floater-b)
                  (make-pillar))))))

(defn mesh-housing-sides-fl [float-z front-right]
  ; TODO extract param picking function
  ; TODO make this take params instead of having magic numbers
  (let [piece (hull (->> front-right (assemble (- c0 1) r0))
                    (->> front-right
                         (translate [0 0 float-z])
                         (assemble (- c0 1) r0)))
        gap-piece (->> front-right
                       (translate [0 0 float-z])
                       (assemble (- c0 1) r0))
        to-floater #(translate [(- lateral-offset) 0 z-offset] %)
        template #(->> (hull % (->> % to-floater)))]
    (union (->> piece template)
           (->> gap-piece template make-pillar)
           (->> piece to-floater make-pillar))))

(defn mesh-housing-sides-fr [float-z front-left]
  ; TODO see above
  (let [piece (hull (->> front-left (assemble (+ cn 1) r0))
                    (->> front-left
                         (translate [0 0 float-z])
                         (assemble (+ cn 1) r0)))
        gap-piece (->> front-left
                       (translate [0 0 float-z])
                       (assemble (+ cn 1) r0))
        to-floater #(translate [lateral-offset 0 z-offset] %)
        template #(->> (hull % (->> % to-floater)))]
    (union (->> piece template)
           (->> gap-piece template make-pillar)
           (->> piece to-floater make-pillar))))

(defn mesh-housing-sides-lr [float-z]
  (let [piece-l #(->> (switch-joiner :vertical :right) (assemble (- c0 1) %))
        piece-r #(->> (switch-joiner :vertical :left) (assemble (+ cn 1) %))
        to-floater-l #(translate [(- lateral-offset) 0 z-offset] %)
        to-floater-r #(translate [lateral-offset 0 z-offset] %)
        template #(hull %1 (->> %1 %2))]
    (for [r rows]
      [(template (piece-l r) to-floater-l)
       (template (piece-r r) to-floater-r)
       (->> (piece-l r) to-floater-l make-pillar)
       (->> (piece-r r) to-floater-r make-pillar)])
    ))

(defn mesh-housing-sides-lr-fill [float-z
                                  front-right back-right
                                  front-left back-left]
  (for [[a b] rps]
    ; TODO tidy up...
    [; c0
     (let [piece (hull (->> back-right (assemble (- c0 1) a))
                       (->> front-right (assemble (- c0 1) b)))
           to-floater #(translate [(- lateral-offset) 0 z-offset] %)]
       (union (hull piece (->> piece to-floater))
              (->> piece to-floater make-pillar)))
     ; cn
     (let [piece (hull (->> back-left (assemble (+ cn 1) a))
                       (->> front-left (assemble (+ cn 1) b)))
           to-floater #(translate [lateral-offset 0 z-offset] %)]
       (union (hull piece (->> piece to-floater))
              (->> piece to-floater make-pillar)))]))

(defn mesh-housing-sides [float-z]
  (let [front-right (union (switch-corner :front-right-out)
                           (->> (switch-corner :front-right-out)
                                (translate [(- pad-offset) 0 0])))
        back-right (union (switch-corner :back-right-out)
                          (->> (switch-corner :back-right-out)
                               (translate [(- pad-offset) 0 0])))
        front-left (union (switch-corner :front-left-out)
                          (->> (switch-corner :front-left-out)
                               (translate [pad-offset 0 0])))
        back-left (union (switch-corner :back-left-out)
                         (->> (switch-corner :back-left-out)
                              (translate [pad-offset 0 0])))]
    (union
     (mesh-housing-sides-fl float-z front-right)
     (mesh-housing-sides-fr float-z front-left)
     (mesh-housing-sides-lr float-z)
     (mesh-housing-sides-lr-fill float-z
                                 front-right back-right
                                 front-left back-left)
     )))

(defn mesh-housing-back [float-z]
  (let
    [x-offset (- 0 lateral-offset (/ (- keyswitch-pitch keyswitch-side) 8))
     corner-waller (->> (switch-corner :back-right)
                        (assemble (- c0 1) rn)
                        (translate [x-offset 0 z-offset]))
     corner (->> (switch-corner :back-right-out)
                 (translate [(- pad-offset) 0 0])
                 (assemble (- c0 1) rn))
     corner-float (->> (switch-corner :back-right-out)
                       (translate [0 float-y 0])
                       (translate [(- pad-offset) 0 0])
                       (assemble (- c0 1) rn))
     corner-float-waller (->> (switch-corner :back-right-out)
                              (translate [0 float-y 0])
                              (assemble (- c0 1) rn)
                              (translate [(- 0 lateral-offset pad-offset)
                                          0
                                          z-offset]))
     floater-back-first (->> floater-back (assemble c0 rn))
     template (comp make-pillar hull)]
    [;(->> (union
     ;      (color [1 0 0 1] corner)
     ;      (color [0 0 1 1] corner-waller)
     ;      (color [0 1 0 1] corner-float)
     ;      (color [0 1 1 1] corner-float-waller))
     ;     (translate [0 0 10]))
     (hull (hull corner corner-waller)
           (hull corner-float corner-float-waller))
     (hull floater-back-first corner corner-float)
     (template corner-waller
               corner-float-waller
               (->> corner-waller (translate [1 0 0]))
               (->> corner-float-waller (translate [1 0 0])))
     (template floater-back-first
               corner-float
               corner-float-waller)
     (for [[a b] (drop-last 1 cps)]
       (template (->> floater-back-right-out (assemble a rn))
                 (->> floater-back-left-out (assemble b rn))))
     (for [c (drop-last 1 cols)]
       (union (hull (->> (switch-joiner [:horizontal :thin] :back) (assemble c rn))
                    (->> floater-back (assemble c rn)))
              (->> floater-back (assemble c rn) make-pillar)))]))

(def mesh-housing
  (let [float-z float-z]
    (union (mesh-housing-front float-z)
           (mesh-housing-front-joiners float-z)
           (mesh-housing-sides float-z)
           (mesh-housing-back float-z))))

;;
;; mesh thumbs
;;

(def mesh-thumb-rotation (/ Math/PI 16))

(defn- mesh-thumb-elevate [shape]
  (->> shape
       (translate [0 0 (- shell-depth
                          thumb-depth-offset)])))

(defn- mesh-thumb-place-main [shape]
  (->> shape
       (rotate [0 (* mesh-thumb-rotation 5) 0])
       mesh-thumb-elevate))

(defn- mesh-thumb-alt-1 [shape]
  (->> shape
       (rotate [(* mesh-thumb-rotation 2) 0 0])
       (translate [0 (* keyswitch-pitch -1) -3.5])
       mesh-thumb-place-main))

(defn- mesh-thumb-alt-2 [shape]
  (->> shape
       (rotate [(* mesh-thumb-rotation -1) 0 0])
       (translate [0 (* keyswitch-pitch 1) -2])
       mesh-thumb-place-main))

(defn- mesh-thumb-extra-1 [shape]
  (->> shape
       (translate [(+ keyswitch-pitch 7) (* keyswitch-pitch 0.5) -13])
       mesh-thumb-elevate
       (rotate [0 0 (* mesh-thumb-rotation 1)])))

(defn- mesh-thumb-extra-2 [shape]
  (->> shape
       (translate [0 (* keyswitch-pitch -1.5) 0])
       (mesh-thumb-extra-1)))

(defn- mesh-thumbs [shape]
  (for [f [mesh-thumb-place-main
           mesh-thumb-alt-1
           mesh-thumb-alt-2
           mesh-thumb-extra-1
           mesh-thumb-extra-2]]
    (->> shape f)))

(defn- assemble-thumbs [shape]
  (->> shape
       (rotate [0 0 (/ (* Math/PI 1) -12)]) ; original
       ;(rotate [0 0 (/ (* Math/PI -2) -16)])
       (translate [(* keyswitch-pitch 2.25) -80 0]) ; original
       ;(translate [0 0 0])
       ))

;;
;; mesh thumb hulling
;;

(def mesh-thumbs-hull
  (let [f (switch-joiner [:horizontal :thin] :front)
        b (switch-joiner [:horizontal :thin] :back)
        l (switch-joiner [:vertical :thin] :left)
        r (switch-joiner [:vertical :thin] :right)
        offset-x (/ keyswitch-pitch 4)
        offset-y (/ keyswitch-pitch 2)
        join-inner-f (->> r (translate [offset-x offset-y 0]) mesh-thumb-place-main)
        join-inner-b (->> r (translate [offset-x (- offset-y) 0]) mesh-thumb-place-main)
        join-inner-fc (->> (switch-corner :front-right-out)
                           (translate [offset-x offset-y 0])
                           mesh-thumb-place-main)
        join-inner-bc (->> (switch-corner :back-right-out)
                           (translate [offset-x (- offset-y) 0])
                           mesh-thumb-place-main)
        ]
    (union
     (hull (->> b mesh-thumb-place-main) (->> f mesh-thumb-alt-1))
     (hull (->> f mesh-thumb-place-main) (->> b mesh-thumb-alt-2))
     (hull (->> b mesh-thumb-extra-1) (->> f mesh-thumb-extra-2))
     (hull join-inner-f (->> (switch-joiner [:vertical :thin] :right) mesh-thumb-alt-2))
     (hull join-inner-b (->> (switch-joiner [:vertical :thin] :right) mesh-thumb-alt-1))
     (hull join-inner-fc
           (->> (switch-corner :front-right-out) mesh-thumb-alt-2)
           (->> (switch-corner :front-left-out) mesh-thumb-extra-1))
     (hull join-inner-bc
           (->> (switch-corner :back-right-out) mesh-thumb-alt-1)
           (->> (switch-corner :back-left-out) mesh-thumb-extra-2))
     (hull join-inner-f
           join-inner-b
           (->> (switch-joiner [:vertical :thin] :right) mesh-thumb-place-main))
     (->> (switch-joiner [:vertical :thin] :right) mesh-thumb-alt-1)
     (hull join-inner-f
           join-inner-b
           (->> (switch-joiner [:vertical :thin] :left) mesh-thumb-extra-1)
           (->> (switch-joiner [:vertical :thin] :left) mesh-thumb-extra-2))
     )))

;;
;; thumb housing
;;

(def thumb-housing-hp
  [[(->> (switch-corner :front-right-out) mesh-thumb-alt-2)
    (->> (switch-corner :front-left-out) mesh-thumb-extra-1)]
   (->> (switch-joiner [:horizontal :thin] :front) mesh-thumb-extra-1)
   (->> (switch-joiner [:vertical :thin] :right)
        (#(union (mesh-thumb-extra-1 %)
                 (mesh-thumb-extra-2 %))))
   [(->> (switch-corner :back-right-out) mesh-thumb-extra-2)
    (->> (switch-corner :back-right-out)
         (translate [0 float-y 0])
         mesh-thumb-extra-2)]
   (->> (switch-joiner [:horizontal :thin] :back)
        (translate [0 float-y 0])
        mesh-thumb-extra-2)
   [(->> (switch-corner :back-left-out) (translate [0 float-y 0]) mesh-thumb-extra-2)
    (->> (switch-corner :back-right-out) mesh-thumb-alt-1)]
   (->> (switch-joiner [:horizontal :thin] :back) mesh-thumb-alt-1)
   [(->> (switch-corner :back-left-out) mesh-thumb-alt-1)
    (->> (switch-corner :back-left-out)
         (#(translate [0 0 z-offset] %))
         mesh-thumb-alt-1)]
   (->> floater-left mesh-thumb-place-main)
   (->> floater-left mesh-thumb-alt-1)
   (->> floater-left mesh-thumb-alt-2)
   ])

(def mesh-thumbs-housing
  (let [template (comp make-pillar hull)
        j-l (switch-joiner [:vertical :thin] :left)]
    [; gap filling
     (hull (->> (switch-joiner [:horizontal :thin] :back) mesh-thumb-extra-2)
           (->> floater-back mesh-thumb-extra-2))
     (hull (->> floater-back-left-out mesh-thumb-extra-2)
           (->> (switch-corner :back-left-out) mesh-thumb-extra-2)
           (->> (switch-corner :back-right-out) mesh-thumb-alt-1))
     ; normal walls
     (for [shape thumb-housing-hp] (template shape))
     ; z walls
     (for [f [mesh-thumb-place-main
              mesh-thumb-alt-1
              mesh-thumb-alt-2]]
       (hull (->> j-l f)
             (->> j-l (#(translate [0 0 z-offset] %)) f)))]))

;;
;; assembly joining
;;

(def mesh-joining
  (let [lat-off (+ lateral-offset (/ (- keyswitch-pitch keyswitch-side) 4))
        floater-lat (->> (switch-corner :back-left-out)
                         (assemble (+ cn 1) rn)
                         (translate [lat-off 0 z-offset]))]
    [(hull (->> floater-back-right-out (assemble (- cn 1) rn))
           (->> (switch-corner :back-left-out) (assemble cn rn))
           (->> (switch-corner :back-left-out) (assemble (+ cn 1) rn))
           (->> (switch-corner :front-left-out) mesh-thumb-alt-2 assemble-thumbs)
           (->> (switch-corner :front-left-out)
                (translate [0 0 float-z])
                mesh-thumb-alt-2
                assemble-thumbs))
     (hull floater-lat
           (->> (switch-corner :back-left-out)
                (translate [(/ (- keyswitch-pitch keyswitch-side) 4) 0 0])
                (assemble (+ cn 1) rn))
           (->> (switch-corner :front-left-out)
                (mesh-thumb-alt-2)
                assemble-thumbs))
     (hull floater-lat
           (->> (switch-corner :front-left-out)
                mesh-thumb-alt-2
                assemble-thumbs)
           (->> (switch-corner :front-right-out)
                mesh-thumb-alt-2
                assemble-thumbs))
     (map (comp make-pillar hull)
          [[(->> (switch-corner :front-right-out)
                 mesh-thumb-alt-2
                 assemble-thumbs
                 make-pillar)
            floater-lat]
           [(->> floater-back-right-out (assemble (- cn 1) rn))
            (->> (switch-corner :back-left-out) (assemble cn rn))
            (->> (switch-corner :front-left-out)
                 (translate [0 0 float-z])
                 mesh-thumb-alt-2
                 assemble-thumbs)]
           ])
     ]))


;;
;; stl
;;

(def bob-w 21.59)
(def bob-h 20.32)
(def bob-d 1.6)

(def bob-x 28)
(def bob-y 31)
(def bob-z 4)

(def teensy-w 18)
(def teensy-h 35)
(def teensy-d 1.6)

(def teensy-x -10)
(def teensy-y -10)
(def teensy-z 4)

(def teensy-usb-w 8)
(def teensy-usb-h 8)
(def teensy-usb-d 2)

;;
;; stl imports
;;

(def stl-bob
  (->>
   (union
    (->> (union (->> (import "rj11.stl")
                     (rotate [(/ Math/PI 2) 0 0])
                     (translate [(- 0 1.4 2.4892) (- 7.874 3.5) -3.01])
                     (rotate [0 0 Math/PI]))
                (difference
                 (cube bob-w bob-h bob-d)
                 (let [d 3.3
                       hole-mount (->> (cylinder (/ d 2) 2) (with-fn 40))
                       hole-mount-x (- (/ bob-w 2) (/ d 2) 1.2)
                       hole-mount-y1 (- (+ (/ d 2) 1.2) (/ bob-h 2))
                       hole-mount-y2 (- (/ bob-h 2) (/ d 2) 1.2)]
                   (union
                    (->> hole-mount (translate [hole-mount-x hole-mount-y1 0]))
                    (->> hole-mount (translate [hole-mount-x hole-mount-y2 0]))))))
         (translate [(/ bob-w 2) (/ bob-h 2) (/ bob-d 2)])))
   (rotate [0 0 (/ Math/PI -2)])
   (translate [bob-x bob-y bob-z])))

; TODO remove this once bob-support is reworked
(def stl-bob2
  (->>
   (union
    (->> (union (->> (import "rj11.stl")
                     (rotate [(/ Math/PI 2) 0 0])
                     (translate [(- 0 1.4 2.4892) (- 7.874 3.5) -3.01])
                     (rotate [0 0 Math/PI]))
                (difference
                 (cube bob-w bob-h bob-d)
                 (let [d 3.3
                       hole-mount (->> (cylinder (/ d 2) 2) (with-fn 40))
                       hole-mount-x (- (/ bob-w 2) (/ d 2) 1.2)
                       hole-mount-y1 (- (+ (/ d 2) 1.2) (/ bob-h 2))
                       hole-mount-y2 (- (/ bob-h 2) (/ d 2) 1.2)]
                   (union
                    (->> hole-mount (translate [hole-mount-x hole-mount-y1 0]))
                    (->> hole-mount (translate [hole-mount-x hole-mount-y2 0]))))))
         (translate [(- 0 1.4 1.4 2.4892) (/ bob-h -2) (/ bob-d 2)])))
   (rotate [0 0 (/ Math/PI 2)])
   (translate [bob-x bob-y bob-z])))

(def stl-teensy32
  (->> (union
        (cube teensy-w teensy-h teensy-d)
        (->> (cube teensy-usb-w teensy-usb-h teensy-usb-d)
             (translate [0
                         (+ (/ teensy-h -2) 1.25)
                         (/ (+ teensy-d teensy-usb-d) 2)])))
       (translate [teensy-x teensy-y (+ (/ teensy-d 2) teensy-z)])))

;;
;; supports
;;

(def bob-support
  (->>
   (->>
    (union
     (let [d 3.3
           peg-depth 7
           grip-w 5
           grip-h 24
           grip-d 4
           bob-grip (cube grip-w grip-h grip-d)
           hole-mount (->> (cylinder (/ d 2) (+ peg-depth 0.01)) (with-fn 40))
           ; TODO countersunk holes
           hole-mount-x (- (/ bob-w 2) (/ d 2) 1.2)
           hole-mount-x2 (- (/ bob-w -2) (/ d 2) 1.2)
           hole-mount-y1 (- (+ (/ d 2) 1.2) (/ bob-h 2))
           hole-mount-y2 (- (/ bob-h 2) (/ d 2) 1.2)
           ]
       (difference
        (union (->> bob-grip (translate [8 0 (/ grip-d 2)]))
               (->> bob-grip (translate [-13.5 0 (/ grip-d 2)]))
               (hull
                (->> (cube 5 6 grip-d)
                     (translate [hole-mount-x (+ hole-mount-y1 -1) (/ grip-d 2)]))
                (->> (cube 5 6 grip-d)
                     (translate [hole-mount-x2 (+ hole-mount-y1 -1) (/ grip-d 2)])))
               )
        (->> hole-mount (translate [hole-mount-x hole-mount-y1 (/ peg-depth 2)]))
        (->> hole-mount (translate [hole-mount-x hole-mount-y2 (/ peg-depth 2)]))
        (->> hole-mount (translate [hole-mount-x2 hole-mount-y1 (/ peg-depth 2)]))
        (->> hole-mount (translate [hole-mount-x2 hole-mount-y2 (/ peg-depth 2)]))
        ))
     )
    (translate [(/ bob-w 2) (/ bob-h 2)
                ;(/ bob-d 2)
                ]))
   (rotate [0 0 (/ Math/PI -2)])
   (translate [bob-x bob-y 0])))

(def teensy-support
  (->> (union (let []
                (->> (cube (+ teensy-w 10) teensy-h teensy-d)
                     ))
              (let [peg-d 8
                    hole-d 3]
                ; TODO break out cylinder into peg
                ; TODO diff out hole from cylinder
                ; TODO factor cylinder 3.3 stuff into util for peg-screw-m3
                (->> (difference (cylinder (/ 5 2)  peg-d)
                                 (->> (cylinder (/ 2.6 2) hole-d)
                                      (translate [0 0 (/ peg-d 2)])))
                     (with-fn 40)
                     (translate [0 0 (/ (+ teensy-d peg-d) 2)]))))
       (translate [teensy-x teensy-y (/ teensy-d 2)])))

;;
;; holes
;;

(def mesh-rj-hole
  (let [h-pad 0.2
        hole-w 5
        hole-h (+ 13.3 h-pad)
        hole-d 12.75
        hole-z (+ bob-z bob-d -0.1)]
    (->> (cube hole-w hole-h hole-d)
         (translate [(+ bob-x bob-w)
                     (- bob-y 1.4 (/ hole-h 2) (/ h-pad -2))
                     (+ (/ hole-d 2) hole-z)]))))

(def mesh-usb-hole
  (let [hole-w 5
        hole-h 5
        hole-d 5
        hole-z (/ hole-d 2)]
    (->> (cube hole-w hole-h hole-d)
         ; TODO use bob-x, bob-y to place usb... also pad with offset
         (translate [0 0 hole-z]))))

;;
;; assembly
;;

(def assembly-switch-debug
  (let [render-hint false
        render-cap false
        render-collar false
        render-switch true
        cap-height 9.39
        cap-side (* 0.8 keyswitch-pitch)
        switch-depth 6.9
        switch-s1 13.9
        switch-s2 12.8]
    (union
     single-plate
     (if render-hint (->> (cube 5 5 2.5) (translate [0 4 1.25])))
     (if render-cap (->> (difference (cube cap-side cap-side cap-height)
                                     (->> (sphere 50) (translate [0 0 50])))
                         (translate [0 0 (+ (/ cap-height 2) 6.75)])))
     (if render-collar (difference
                        (hull single-plate
                              (->> single-plate
                                   (translate [0 0 (- 0
                                                      (/ plate-thickness 0.5))])))
                        single-plate))
     (if render-switch (->> (import "switch_mx.stl")
                            (translate [0 0 15.12])))
     )))

(def mesh-grid
  (for [c cols
        r rows]
    (assemble c r assembly-switch-debug)))

(def assembly
  (let [shape assembly-switch-debug
        thumbs (union (mesh-thumbs shape)
                      mesh-thumbs-hull
                      mesh-thumbs-housing
                      )]
    (union mesh-grid
           mesh-support
           (difference
            mesh-housing
            mesh-rj-hole
            mesh-usb-hole
            )
           ; stl and pcb supports
           (->> stl-bob (color [1 1 0 0.8]))
           ;(->> stl-bob2 (color [1 0 1 0.8]))
           bob-support
           (->> stl-teensy32 (color [0 1 1 0.8]))
           teensy-support
           (->> thumbs assemble-thumbs)
           mesh-joining
           )))

;;
;; BONUS: mesh assembly side
;;

(defn- assembly-side-opts [side]
  (case side
    :left [:back-right :front-right :right c0 -1]
    :right [:back-left :front-left :left cn 1]))

(defn- assembly-side-support-joiner [[corner-a corner-b inverse col-out offset]]
  (for [[a b] rps]
    (->> (hull (->> (switch-corner [corner-a :thick])
                    (assemble (+ col-out offset) a))
               (->> (switch-corner [corner-b :thick])
                    (assemble (+ col-out offset) b)))
         (make-pillar))))

(defn- assembly-side-support [side [corner-a corner-b inverse col-out offset]]
  (for [r rows]
    (union
     (->> (union (switch-joiner :vertical inverse)
                 (->> (switch-joiner :vertical inverse)
                      (translate [0 0 (/ plate-thickness 8)])))
          (assemble (+ col-out offset) r)
          (make-pillar)))))

(defn- assembly-side-support-tenting [side [corner-a corner-b inverse col-out offset]]
  (let [tenting-mount-depth (+ plate-thickness 5)]
    (->> (difference (->> (cube 12 12 tenting-mount-depth))
                     (->> (cylinder 4.05 (/ tenting-mount-depth 3))
                          (translate [0 0 (+ (/ tenting-mount-depth 3 -2)
                                             (/ tenting-mount-depth 2))])
                          (with-fn 6))
                     (->> (cylinder 2 (+ plate-thickness 5))
                          (with-fn 40)))
         (translate [1.25 0 0])
         (col-place (- c0 1))
         (translate [0 0 (/ tenting-mount-depth 2)])
         )))

(defn- assembly-side-mount [side]
  (let [parsed-opts (assembly-side-opts side)]
    (union (assembly-side-support side parsed-opts)
           (assembly-side-support-joiner parsed-opts)
           ;(assembly-side-support-tenting side parsed-opts)
           )))

(def assembly-side
  (union (assembly-side-mount :left)
         (assembly-side-mount :right)
         ))

;;
;; things
;;

;(->> assembly
;     (write-scad)
;     (spit "things/mesh4.scad"))
;
;(->> assembly-side
;     (write-scad)
;     (spit "things/side4.scad"))
;
;(->> assembly
;     (union assembly-side)
;     (write-scad)
;     (spit "things/assembly4.scad"))

