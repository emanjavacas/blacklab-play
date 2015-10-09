(ns blacklab-play.core
  (:require [blacklab-play.paginator :refer [paginate next-page]])
  (:import [nl.inl.blacklab.search Searcher Hit Hits Concordance Kwic TextPatternRegex]
           [nl.inl.blacklab.queryParser.corpusql CorpusQueryLanguageParser]
           [nl.inl.util XmlUtil]
           [org.apache.lucene.document Document]
           [org.apache.lucene.index IndexableField]))

(set! *warn-on-reflection* true)

(defn corpus-query 
  "Basic query handler that translates single to double quotes"
  ^TextPatternRegex [s]
  (let [parsed-str (apply str (replace {\' \"} s))]
    (CorpusQueryLanguageParser/parse parsed-str)))

(defn create-searcher 
  "Creates a searcher given index path. It does not close the searcher"
  ^Searcher  [^String path]
  (Searcher/open (java.io.File. path)))

(defn run-query 
  "Runs the query and returns the general Hits object"
  ^Hits [^Searcher searcher ^String s]
  (let [hits (.find searcher (corpus-query s))]
    hits))

(defn hits->concs 
  "Returns a vector with basic concordance info.
  Less useful as metadata cannot be recovered from
  the Concordance object - better use Kwic objects" 
  [^Hits hits]
  (let [->txt (fn [conc] (XmlUtil/xmlToPlainText conc))
        hit->vec
        (fn [^Hit hit]
          (let [^Concordance conc (.getConcordance hits hit)
                doc (.doc hit)
                start (.start hit)
                end (.end hit)
                left  (->txt (.left conc))
                match (->txt (.match conc))
                right (->txt (.right conc))]
            [doc start end left match right]))]
    (map hit->vec hits)))

(defn format-conc 
  "Simple string formatting for concordance vec output"
  [[doc start end left match right]]
  (format "[%05d %06d:%06d] %50s[%s]%s\n" doc start end left match right))

(defn hits->txt 
  "Applies string formatting to a hit seq"
  [^Hits hits]
  (map format-conc (hits->concs hits)))

(defmacro update-vec
  "Updates a vec by idx. bindings are in the form
  [idx form] where form can be a new value or a fn
  to be applied on the idx item of v"
  [v & bindings]
  `(-> ~v
       ~@(map (fn [[idx form]]
                `(assoc ~idx (if (clojure.test/function? ~form)
                               (~form (get ~v ~idx))
                               ~form)))
              bindings)))

(defmacro update-range
  "Updates v applying function f to the items at the positions
  by a range. See #'range for its function signature"
  [v f & args]
  `(-> ~v 
       ~@(map (fn [idx]
                `(assoc ~idx (~f (get ~v ~idx)))) 
              `~(apply range args))))

(defn update-range
  "Updates v applying function f to the items at the positions
  by a range. See #'range for its function signature"
  [v f & args]
  (if args
    (let [arange (apply range args)]
      (loop [cur (first arange)
             todo (next arange)
             res v]
        (if todo
          (recur
           (first todo)
           (next todo)
           (assoc res cur (f (get res cur))))
          res)))
    v))

(defn create-hit
  "Base handler that takes a Hit and gives a clojure data struct"
  [^Hit hit ^Hits hits]
  (let [kwic (.getKwic hits hit)
        props (map keyword (.getProperties kwic))
        tokens (.getTokens kwic)
        hit-vec (mapv (partial zipmap props) (partition (count props) tokens))]
    {:hit hit :hits hits :hit-vec hit-vec :meta {}}))

(defn basic-handler 
  "Basic handler that removes the hit key from hit-map"
  [hit-map]
  (-> hit-map
   (dissoc :hit)
   (dissoc :hits)))

(defn create-doc-wrapper
  "Handler for extracting doc metadata"
  [^Searcher searcher field-name]
  (fn [handler]
    (fn [hit-map]
      (let [^Hit hit (:hit hit-map)
            ^Document doc (.document searcher (.doc hit))
            ^String field (.stringValue (.getField doc field-name))
            new-map (assoc-in hit-map [:meta (keyword field-name)] field)]
        (handler new-map)))))

(defn create-doc-wrappers
  "Extract multiple fields at once"
  [^Searcher searcher & field-names]
  (fn [handler]
    (fn [hit-map]
      (let [^Hit hit (:hit hit-map)
            ^Document doc (.document searcher (.doc hit))
            get-value (fn [field-name] (.stringValue (.getField doc field-name)))
            fields (zipmap (map keyword field-names) (map get-value field-names))
            new-map (assoc hit-map :meta fields)]
        (handler new-map)))))

(defn create-doc-wrappers
  "Extract all doc fields"
  [^Searcher searcher]
  (fn [handler]
    (fn [hit-map]
      (let [^Hit hit (:hit hit-map)
            ^Document doc (.document searcher (.doc hit))
            field-tokens (map (fn [^IndexableField field]
                                [(.name field)
                                 (.stringValue field)])
                              (.getFields doc))
            fields (interleave (map keyword (map first field-tokens))
                               (map second field-tokens))]
        (handler (apply update-in hit-map [:meta] assoc fields))))))

(defn wrap-match
  "Add match annotation to match tokens"
  [handler]
  (fn [hit-map]
    (let [^Hit hit (:hit hit-map)
          ^Hits hits (:hits hit-map)
          ^Kwic kwic (.getKwic hits hit)
          start (.getHitStart kwic)
          end (.getHitEnd kwic)
          hit-vec (:hit-vec hit-map)
          hit-match (update-range hit-vec #(assoc % :match true) start (inc end))
          new-map (assoc hit-map :hit-vec hit-match)]
      (handler new-map))))

(defmacro create-handler
  "Insert wrappers into a handler"
  [handler & wrappers]
  `(-> ~handler ~@wrappers))

;;; test
(def brown-searcher (create-searcher "resources/brown-index/"))
(def wrap-base (create-doc-wrapper brown-searcher "base"))
(def wrap-inputfile (create-doc-wrapper brown-searcher "fromInputFile"))
(def wrap-doc (create-doc-wrappers brown-searcher))
(def handle-hit   
  (create-handler basic-handler wrap-match wrap-doc))
;wrap-base wrap-inputfile
(defn handle-hits
  "applies handler to a hit-map"
  [^Hits hits]
  (for [^Hit hit hits]
    (handle-hit (create-hit hit hits))))
(def hits (run-query brown-searcher "'be' 'going' 'to' [pos='v.*']"))
(def hits (run-query brown-searcher "[pos=\"v.*\"] [punct=\" \\. \"]"))
(def paginator (paginate hits))
;(handle-hits (next-page paginator))





