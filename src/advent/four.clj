(ns advent.four
  (:require [clojure.test :refer :all]
            [advent.utils.input :as io]
            [clojure.string :as str]))

(defn falls-asleep-str? [s]
  (= s "falls asleep"))

(defn wakes-up-str? [s]
  (= s "wakes up"))

(defn str->schedule [s]
  (let [[time event] (str/split s #"]")
        event (apply str (rest event))
        [date clock] (str/split (apply str (rest time)) #" ")
        [year month day] (str/split date #"-")
        [hours minutes] (str/split clock #":")
        schedule {:year (Integer. year)
                  :month (Integer. month)
                  :day (Integer. day)
                  :hours (Integer. hours)
                  :minutes (Integer. minutes)}]
    (rest time)
    (cond
      (wakes-up-str? event)
      (assoc schedule :type :wakes-up)

      (falls-asleep-str? event)
      (assoc schedule :type :falls-asleep)

      :else
      (let [[_ id] (str/split event #" ")]
        (assoc schedule :type :shift-start
                        :id (Integer. (apply str (rest id))))))))

(deftest str->schedule-test
  (is (= (str->schedule "[1518-11-01 00:00] Guard #10 begins shift")
         {:year 1518, :month 11, :day 1, :hours 0, :minutes 0, :type :shift-start, :id 10}))
  (is (= (str->schedule "[1518-11-01 00:05] falls asleep")
         {:year 1518, :month 11, :day 1, :hours 0, :minutes 5, :type :falls-asleep}))
  (is (= (str->schedule "[1518-11-01 00:25] wakes up")
         {:year 1518, :month 11, :day 1, :hours 0, :minutes 25, :type :wakes-up})))

(defn sort-events [events]
  (sort-by (juxt :year :month :day :hours :minutes (complement :id)) events))

(deftest sort-events-test
  (is (= (sort-events [{:year 1 :month 1 :day 1 :hours 1 :minutes 1 :id 10}
                       {:year 1 :month 1 :day 1 :hours 1 :minutes 1}])
         [{:year 1 :month 1 :day 1 :hours 1 :minutes 1 :id 10}
          {:year 1 :month 1 :day 1 :hours 1 :minutes 1}]))
  (is (= (sort-events [{:year 2 :month 1 :day 1 :hours 1 :minutes 1}
                       {:year 1 :month 1 :day 1 :hours 1 :minutes 1}
                       {:year 1 :month 2 :day 3 :hours 1 :minutes 1}
                       {:year 1 :month 2 :day 3 :hours 10 :minutes 1}])
         [{:year 1 :month 1 :day 1 :hours 1 :minutes 1}
          {:year 1 :month 2 :day 3 :hours 1 :minutes 1}
          {:year 1 :month 2 :day 3 :hours 10 :minutes 1}
          {:year 2 :month 1 :day 1 :hours 1 :minutes 1}])))

(defn parse-input [input]
  (->> input
       (map str->schedule)
       sort-events))

(def input (parse-input (io/read-input "four.input")))

(def test-input
  (parse-input
    ["[1518-11-01 00:00] Guard #10 begins shift"
     "[1518-11-01 00:05] falls asleep"
     "[1518-11-01 00:25] wakes up"
     "[1518-11-01 00:30] falls asleep"
     "[1518-11-01 00:55] wakes up"
     "[1518-11-01 23:58] Guard #99 begins shift"
     "[1518-11-02 00:40] falls asleep"
     "[1518-11-02 00:50] wakes up"
     "[1518-11-03 00:05] Guard #10 begins shift"
     "[1518-11-03 00:24] falls asleep"
     "[1518-11-03 00:29] wakes up"
     "[1518-11-04 00:02] Guard #99 begins shift"
     "[1518-11-04 00:36] falls asleep"
     "[1518-11-04 00:46] wakes up"
     "[1518-11-05 00:03] Guard #99 begins shift"
     "[1518-11-05 00:45] falls asleep"
     "[1518-11-05 00:55] wakes up"]))

(defn with-minutes-in-sleep-cycle [sleeplog]
  (map
    (fn [cycle]
      (if (and (:start cycle) (:end cycle))
        (let [minutes (- (:end cycle) (:start cycle))]
          (assoc cycle :total minutes))
        cycle))
    sleeplog))

(deftest with-minutes-in-sleep-cycle-test
  (is (= (with-minutes-in-sleep-cycle [{:id 1 :start 0 :end 10}
                                       {:id 1 :start 15 :end 25}
                                       {:id 2}])
         [{:id 1 :start 0 :end 10 :total 10}
          {:id 1 :start 15 :end 25 :total 10}
          {:id 2}])))

(defn sleep-minutes-by-guard [events]
  (->>
    events
    (reduce
      (fn [log event]
        (case (:type event)
          :shift-start
          (conj log {:id (:id event)})

          :falls-asleep
          (if (nil? (:start (first log)))
            (conj
              (rest log)
              (assoc (first log) :start (:minutes event)))

            (conj
              log
              {:id (:id (first log))
               :start (:minutes event)}))

          :wakes-up
          (conj
            (rest log)
            (assoc (first log)
              :end (:minutes event)))

          log))
      '())
    with-minutes-in-sleep-cycle))

(deftest sleep-minutes-by-guard-test
  (is (= (sleep-minutes-by-guard
           [{:year 1518, :month 11, :day 1, :hours 0, :minutes 0, :type :shift-start, :id 10}
            {:year 1518, :month 11, :day 1, :hours 0, :minutes 5, :type :falls-asleep}
            {:year 1518, :month 11, :day 1, :hours 0, :minutes 25, :type :wakes-up}
            {:year 1518, :month 11, :day 1, :hours 0, :minutes 30, :type :falls-asleep}
            {:year 1518, :month 11, :day 1, :hours 0, :minutes 55, :type :wakes-up}])
         '({:id 10, :start [0 30], :end [0 55] :total 25}
            {:id 10, :start [0 5], :end [0 25] :total 20}))))

(defn sleepiest-guard [events]
  (->> events
       (map
         (fn [[id events]]
           [id (reduce + 0 (keep :total events))]))
       (reduce
         (fn [prev this]
           (if (> (second prev) (second this))
             prev
             this)))
       first))

(defn sleep-minutes [events]
  (mapcat
    (fn [e]
      (when (and (:start e) (:end e))
        (range (:start e) (:end e))))
    events))

(defn step-one [input]
  (let [events (->> input
                    sleep-minutes-by-guard
                    (group-by :id))
        guard-id (sleepiest-guard events)
        guard-events (get events guard-id)]
    (->> guard-events
         sleep-minutes
         frequencies
         (apply max-key second)
         first
         (* guard-id))))

(deftest step-one-test
  (is (= (step-one test-input) 240)))

(defn step-two [input]
  (let [events (->> input
                    sleep-minutes-by-guard
                    (group-by :id))
        sleepiest-minutes (keep
                            (fn [[id guard-events]]
                              (let [sleepy-minutes (->> guard-events
                                                        sleep-minutes
                                                        frequencies)]
                                (when (not-empty sleepy-minutes)
                                  [id (apply max-key second sleepy-minutes)])))
                            events)]
    (->> sleepiest-minutes
         (apply max-key (comp second second))
         ((juxt first (comp first second)))
         (apply *))))

(deftest step-two-test
  (is (= (step-two test-input) 4455)))