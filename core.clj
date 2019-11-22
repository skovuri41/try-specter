(ns try-specter.core
  (:require [com.rpl.specter :refer :all]
            [clj-time.coerce :as c]))

;; Initial Problem
;; Complex data structure needs to be cleaned up for GraphQL query.
;;
;; All instances of #uuid need to be turned into strings
;; All #inst need to be converted into milliseconds.
;;
;; These can appear at various levels in the data structure.


(def data
  [[:subscription [:property {:from    #inst "2019-01-10T08:00:00.000-00:00"
                              :id      #uuid "1ed2bdf6-664d-4766-9600-669d3f405a20"
                              :options {:foo "hello"}
                              :other   [{:bar "baz" :quux "hello"}
                                        {:hello "ff"
                                         :id    #uuid "37eb64d1-b282-4314-803e-c3c50cb8c330"}]
                              :amount  10}
                   [:id]]]])

(def path-to-hello [0 1 1 :other 1 :hello])

(comment


  ;; Use select to find something
  (select [0] data)
  (select [0 1] data)
  (select [0 1 1 :other] data)
  (select [0 1 1 :other 1 :hello] data)

  ;; this works very much like get-in but with the parameters reversed
  (get-in data [0 1])

  ;; Also notice that select returns a vector of results. If we only want one then use select-one
  (select-one [0 1] data)

  ;; Once the path is correct it may be better to define it (see above)
  (select path-to-hello data)

  ;; Use setval to change something to a fixed value
  (setval path-to-hello "world" data)

  ;; Use transform to change something using a function
  (transform path-to-hello #(str "**" % "**") data)

  ;; But selecting individual entries is not enough so we need to walk the data structure:

  ;; Find all the strings
  (select [(walker string?)] data)

  ;; And all the #uuid s
  (select [(walker uuid?)] data)

  ;; Now the task is easy:
  (->> data
       (transform [(walker uuid?)] str)
       (transform [(walker inst?)] to-unix-time)))

;; Inspired by Misophistful's screencast https://www.youtube.com/watch?v=rh5J4vacG98
;; Further investigation: zippers & parser
;;
;; zippers allow for changes to be made on items in the data structure depending on items around it.
;; Here is an example of its use: https://github.com/soulflyer/hinh-anh/blob/master/src_front/anh_front/tree/tree.cljs
;; There it is being used to implement a tree navigation structure for a clojurescript UI
;;
;; parser alows us to specify 2 functions, parse and unparse. The first is applied as specter does its search,
;; the second is applied as specter rebuilds the datastructure to be returned.
;; The Misophistful screencast has an example of this at around 34:00.

(defn to-unix-time
  "Return the number of seconds after the Unix time
  Arity 1: returns the number of milliseconds after Unix time.
  Arity 2: returns the time in `unit` after Unix time: #{:millis :seconds}"
  ([dt]
   (to-unix-time dt :millis))
  ([dt unit]
   (when-some [date-long (c/to-long dt)]
     (case unit
       :seconds (quot date-long 1000)
       :millis  date-long))))


(defn sanitize-queries
  [data]
  (->> data
       (transform [(walker uuid?)] str)
       (transform [(walker inst?)] to-unix-time)))
