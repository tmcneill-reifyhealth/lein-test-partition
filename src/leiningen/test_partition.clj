(ns leiningen.test-partition
  (:require [leiningen.core.main :as lmain]
            [leiningen.core.eval :as eval]
            [leiningen.core.project :as project]
            [bultitude.core :as b]
            [clojure.java.io :as io]))

(defn test?-form
  [project-selectors selectors]
  `(fn [v#]
     (let [m# (meta (val v#))]
       ((apply every-pred
               :test
               ~(vec (vals (select-keys project-selectors selectors))))
        m#))))

(defn all-namespaces
  [project]
  (b/namespaces-on-classpath
    :classpath (map io/file (distinct (:test-paths project)))
    :ignore-unreadable? false))

(defn test-partition
  [project _ & args]
  (let [{:keys [target] :as parsed-args} (lmain/parse-options args)
        target (or target "tests.txt")
        project (project/merge-profiles project [:leiningen/test :test])
        all-namespaces (all-namespaces project)
        selectors (keys (dissoc parsed-args target))
        project-selectors (:test-selectors project)
        _ (eval/eval-in-project
           project
           `(->> (reduce
                  (fn [tests# namespace#]
                    (concat
                     tests#
                     (->>
                      (ns-publics (last namespace#))
                      (filter ~(test?-form project-selectors selectors))
                      (map (comp (partial symbol (name (last namespace#)))
                                 name
                                 key)))))
                  [] '~(map (fn [ns] `'~ns) all-namespaces))
                 (map str)
                 (sort)
                 (clojure.string/join "\n")
                 (spit ~target))
           (apply list
                  'do
                  (map (fn [ns] (list 'require `'~ns))
                       all-namespaces)))]))
