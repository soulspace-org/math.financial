(defproject org.soulspace.clj/math.financial "0.8.0"
  :description "The math.financial library contains financial math functions in Clojure."
  :url "https://github.com/soulspace-org/math.financial"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  ; use deps.edn dependencies
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

;  :dependencies [[org.clojure/clojure "1.11.3"]]

  :test-paths ["test"]
  :scm {:name "git" :url "https://github.com/soulspace-org/math.financial"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
