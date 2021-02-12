(defproject programming-bitcoin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[commons-codec/commons-codec "1.15"]
                 [org.clojure/clojure "1.10.1"]
                 [pandect "0.6.1"]
                 [org.bouncycastle/bcprov-jdk15on "1.68"]]
  ; :main ^:skip-aot programming-bitcoin.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "1.1.0"]]}})
