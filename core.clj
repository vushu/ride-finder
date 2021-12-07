;; Copyright (c) 2021 Vushu <danvu.hustle@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(use '[clojure.string :refer [blank? split join]])
(use '[clojure.pprint :refer [pprint]])

(def rides (atom []))

(def test-rides
  [{ :from-city "Odense" :to-city "Copenhagen" :date "2018-10-01" :number-of-seats "4"}
   { :from-city "Copenhagen" :to-city "Aarhus" :date "2018-10-01" :number-of-seats "2"}
   { :from-city "Odense" :to-city "Copenhagen" :date "2018-10-02" :number-of-seats "1"}])

;; Using java date time to compare dates
(defn has-value[k v arr]
  (= (k arr) v))

(defn get-set-where [key-sym search-value data]
  (if (blank? search-value) data
    (filterv #(has-value key-sym search-value %) data)))

(defn to-java-date [value]
  (.parse (java.text.SimpleDateFormat. "yyyy-mm-dd") value))

(defn equal-to [from date]
  (= (.compareTo (to-java-date from) (to-java-date date)) 0))

(defn greater-than-or-equal-to [from date]
  (>= (.compareTo (to-java-date from) (to-java-date date)) 0))

(defn less-than-or-equal-to [date to]
  (<= (.compareTo (to-java-date date) (to-java-date to)) 0))
;;

(defn date-is-between [date from to]
  (cond (and (not (blank? from)) (not(blank? to))) (and (greater-than-or-equal-to date from) (less-than-or-equal-to date to))
        (and (blank? from) (not (blank? to))) (less-than-or-equal-to date to)
        (and (not (blank? from)) (blank? to)) (equal-to date from)
        (and (blank? from) (blank? to)) true))

(defn get-rides-date-between
  ([from-date to-date the-rides]
   (filterv #(date-is-between (:date %) from-date to-date ) the-rides)))

(defn get-rides-with-minimum-seats [minimum-seats data]
  (if (blank? minimum-seats) data
    (filterv #(<= (Long/parseLong minimum-seats) (Long/parseLong (:number-of-seats %))) data)))

(defn get-rides-by-cities
  ([from-city to-city data]
   (->> data
        (get-set-where :from-city from-city)
        (get-set-where :to-city to-city))))


(defn print-rides [the-rides]
  (do
    (println "===========================")
    (pprint (map #(vals %) the-rides))
    (println "===========================")
    ))



(defn search-ride [[from-city [to-city]] [from-date [to-date]] [minimum-seats]]
  (->> @rides
       (get-rides-by-cities from-city to-city)
       (get-rides-date-between from-date to-date)
       (get-rides-with-minimum-seats minimum-seats)))

(defn create-ride [[ from-city to-city date seats] & args]
  (swap! rides conj {:from-city from-city :to-city to-city :date date :number-of-seats seats}))

(defn create-return-ride-from-last [[date] & args]
  (let [last-ride (last @rides)
        updated-ride (assoc last-ride :from-city (:to-city last-ride) :to-city (:from-city last-ride) :date date)]
    (swap! rides conj updated-ride)))

(defn handle-command [command & args]
  (cond
    (= "S" command) (print-rides
                      (let [[from-city to-city  from-date to-date minimum-seats ] args ]
                        (search-ride [from-city [to-city]] [from-date [to-date]] [minimum-seats])))
    (= "C" command) (create-ride args)
    (= "R" command) (create-return-ride-from-last args)
    (= "t" command) (do
                      (println "---------------------------")
                      (println "Reset to Test!")
                      (reset! rides test-rides)
                      (println "---------------------------")
                      )
    (= "c" command) (do
                      (println "---------------------------")
                      (println "Clearing data!")
                      (reset! rides [])
                      (println "---------------------------")
                      )


    (= "l" command)
    (if (> ( count @rides) 0)
      (do
        (println "Listing rides:")
        (print-rides @rides))
      (do
        (println "---------------------------")
        (println "No rides registered")
        (println "---------------------------")))
    :else []))

(defn -main []
  (println "---------------------------")
  (println "Welcome to go more or less!")
  (println "---------------------------")
  (println "q to quit")
  (println "t to create test data")
  (println "c to clearing rides")
  (println "l to list rides")
  (println "---------------------------")
  (loop []
    (println "Insert your command: ")
    (let [command (read-line)]
      (when (not= command "q")
        (let [splitted (split command #" ")]
          (apply handle-command splitted))
        (recur))))
  (println "Bye bye!")
  0)

(-main)

