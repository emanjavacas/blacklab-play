(ns blacklab-play.core
  (:import [nl.inl.blacklab.search Searcher TextPattern Hit Hits HitsWindow Concordance]
           [nl.inl.blacklab.queryParser.corpusql CorpusQueryLanguageParser]
           [nl.inl.util XmlUtil]
           [org.apache.lucene.document Document]))

(defn corpus-query [^String s]
  (let [parsed-str (apply str (replace {\' \"} s))]
    (CorpusQueryLanguageParser/parse parsed-str)))

(defn create-searcher ^Searcher [^String path] ;closing
  (Searcher/open (java.io.File. path)))

(defn run-query ^Hits [^Searcher searcher ^String s]
  (let [^Hits hits (.find searcher (corpus-query s))]
    hits))

(defn hits->cpos [^Hits hits]
  (map (juxt #(.start %) #(.end %) #(.doc %)) (seq hits)))

(defn hits->txt [^Hits hits]
  (let [->txt (fn [^String conc] (XmlUtil/xmlToPlainText conc))]
    (for [hit hits
          :let [^Concordance conc (.getConcordance hits hit)
                ^Integer doc (.doc hit)
                ^Integer start (.start hit)
                ^Integer end (.end hit)
                ^String left  (->txt (.left conc))
                ^String match (->txt (.match conc))
                ^String right (->txt (.right conc))]]
;      [doc start end left match right]
      (format "[%05d %06d:%06d] %50s[%s]%s\n" doc start end left match right)
      )))

(defprotocol PaginateHits
  (^Integer get-current [this])
  (^Integer page-size! [this n])
  (^HitsWindow current-page [this])
  (^HitsWindow context-size! [this context-size])
  (^HitsWindow next-page! [this])
  (^HitsWindow prev-page! [this])
  (^HitsWindow nth-page! [this n]))

(defrecord Paginator [hits init page-size] ;private record
  PaginateHits
  (get-current [this] @init)
  (page-size! [this n] (ref-set page-size n))
  (current-page [this] (.window hits @init @page-size))
  (context-size! [this context-size] 
    (do (.setContextSize hits context-size))
    (current-page this))
  (next-page! [this]
    (println @init)
    (dosync (let [to-add (+ @init @page-size)]
              (if (< to-add (.size hits))
                (.window hits (alter init (partial +) @page-size) @page-size)
                (.window hits (ref-set init 0) @page-size)))))
  (prev-page! [this]
    (dosync (let [to-rem (- @init @page-size)
                  num-hits (.size hits)
                  prev (- num-hits (mod num-hits @page-size))]
              (if (pos? to-rem)
                (.window hits (alter init (partial -) @page-size) @page-size)   
                (.window hits (ref-set init prev) @page-size)))))
  (nth-page! [this n]
    (dosync (.window hits (ref-set init (* n @page-size)) @page-size))))

(defn paginate [^Hits hits page-size] ;record constructor
  (let [current (ref 0) page-size (ref page-size)]
    (Paginator. hits current page-size)))

(def test-searcher 
  (create-searcher "/Users/quique/code/java/BlackLab/target/test-index"))

(def brown-searcher 
  (create-searcher "resources/brown-index"))

(def hits (run-query brown-searcher "'be' 'going' 'to' [pos='v.*']"))
(def hits (run-query-safe brown-searcher "\"scheme\" \",\" \"add\""))
(def hits (run-query brown-searcher "[punct=\" \\. \"]"))
(def paginator (paginate hits 10))
(doseq [hit (hits->txt (next-page! paginator))]
  (println hit))

(.getTokens (.getKwic hits (first (seq hits))) "word")
;;; get doc info
;; (.stringValue (.getField (.document brown-searcher 499) "base"))
;; (doseq [field (.getFields (.document brown-searcher 499))]
;;   (println (.stringValue field) (.name field)))
