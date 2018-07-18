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
   (let* [bench (first args)
         s (symbol (str "keeb-workbench." bench ".core"))]
     (println (str "pre " *ns*))
     (in-ns s)
     (println (str "post " *ns*))
     ;(use s)
     ;(println (ns-map s))
     ;(pprint (str "var-get: " (var-get 'assembly)))
     ;(pprint (str assembly))
     (->> assembly
          (write-scad)
          (spit (str "things/" bench ".scad"))
     )
   (comment case (first args)
            "mesh-4" (pprint keeb-workbench.mesh-4.core/assembly)
            (println "bad arg"))))

