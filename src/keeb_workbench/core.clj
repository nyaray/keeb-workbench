(ns keeb-workbench.core
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  (:use [keeb-workbench.mesh-4.core])
  (:use clojure.pprint)
  (:gen-class))

(defn -main
  "spits scad into things/BENCH.scad, where bench is the first arg in `lein run`"
  [& args]
  (let* [bench (first args)
         s (symbol (str "keeb-workbench." bench ".core"))]
    (in-ns s)
    (println (str "spitting assembly from " *ns*))
    (->> assembly
         (write-scad)
         (spit (str "things/" bench ".scad")))))

