(ns keeb-workbench.core
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  (:use [keeb-workbench.mesh-4.core])
  (:use clojure.pprint)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
   (let [s (symbol (str "keeb-workbench." (first args) ".core"))]
     (println (str "pre " *ns*))
     (in-ns s)
     (println (str "post " *ns*))
     ;(use s)
     ;(println (ns-map s))
     ;(pprint (str "var-get: " (var-get 'assembly)))
     ;(pprint (str assembly))
     (->> assembly
          (write-scad)
          (spit "things/mesh-4.scad"))
     )
   (comment case (first args)
            "mesh-4" (pprint keeb-workbench.mesh-4.core/assembly)
            (println "bad arg"))))

