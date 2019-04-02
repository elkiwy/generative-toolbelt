(defproject elkiwy/generative-toolbelt "0.1.1-SNAPSHOT"
    
  :description "A handy set of function to ease out the creative process of drawing with Quil."

  :url "https://github.com/elkiwy/generative-toolbelt"

  :license {:name "GNU General Public License v3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}

  :plugins [[lein-codox "0.10.6"]]


  :target-path "docs/"

  :codox   {:project {:name "generative-toolbelt", :version "0.1.1"}
            :namespaces :all}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "2.8.0"]])
