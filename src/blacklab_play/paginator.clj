(ns blacklab-play.paginator
    (:import [nl.inl.blacklab.search Hits HitsWindow]))

(defprotocol PaginateHits
  (get-pos [this])
  (set-context-size! [this context-size])
  (^HitsWindow current-page [this] [this page-size])
  (^HitsWindow next-page [this] [this page-size])
  (^HitsWindow prev-page [this] [this page-size])
  (^HitsWindow nth-page [this n] [this n page-size]))

(defrecord Paginator [hits init] ; private
  PaginateHits
  (get-pos [this] @init)
  (current-page [this] (.window hits @init 20))
  (current-page [this page-size] (.window hits @init page-size))
  (set-context-size! [this context-size] (.setContextSize hits context-size))
  (next-page [this] (next-page this 20))
  (next-page [this page-size]
    (dosync (let [current @init
                  to-add (+ current page-size)
                  _ (ref-set init (if (> to-add (.size hits)) 0 to-add))] 
              (.window hits current page-size))))
  (prev-page [this] (prev-page this 20))
  (prev-page [this page-size]
    (dosync (let [to-rem (- @init page-size)
                  _ (ref-set init (if (pos? to-rem) to-rem (.size hits)))]
              (.window hits to-rem page-size))))
  (nth-page [this n] (nth-page this n 20))
  (nth-page [this n page-size]
    (dosync (.window hits (ref-set init (* n page-size)) page-size))))

(defn paginate [^Hits hits] ;record constructor
  (let [current (ref 0)]
    (Paginator. hits current)))
