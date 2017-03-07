(defproject defcomponent "0.2.2"
  :description "wrapper for com.stuartsierra/component"
  :url "https://github.com/prepor/defcomponent"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.stuartsierra/component "0.2.3"]]
  :profiles {:repl
             {:dependencies [[org.clojure/tools.namespace "0.2.10"]
                             [org.clojure/clojure "1.7.0"]]
              :repl-options {:init-ns defcomponent}
              :injections [(require 'clojure.tools.namespace.repl)
                           (require '[clojure.tools.namespace.repl :refer [refresh]])
                           (require 'defcomponent)]}}
  :deploy-repositories [["releases" {:url "https://clojars.org/repo"
                                     :sign-releases false}]])
