#!/usr/bin/env bb
;; Extracts profiles from a Leiningen's project.clj

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper

(defn- project-definition->map
  [[_defproject-macro name version & key-val-pairs :as _project-clj]]
  (->> key-val-pairs
       (concat [:name name :version version])
       (apply hash-map)))

(defn- lein-project-profiles
  "Returns a list with all profile names from `project-map`."
  [project-map]
  (->> project-map
       :profiles
       (keys)
       (map name)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defn parse-project-clj
  "Parses a leiningen project.clj to a map."
  [project-clj-filepath]
  (->> project-clj-filepath
       (slurp)
       (read-string)
       (project-definition->map)))

(defn extract-project-profiles
  "Main function, that pretty prints all profile names from project.clj."
  [[project-clj-filepath :as _command-args]]
  (->> project-clj-filepath
       (parse-project-clj)
       (lein-project-profiles)
       (clojure.pprint/pprint)))

(extract-project-profiles *command-line-args*)
