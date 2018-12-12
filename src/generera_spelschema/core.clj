(ns generera-spelschema.core
  (:require [ysera.test :refer [is is-not]]))

(defn beräkna-antal-tresnurrar
  [spelschema]
  (map (fn [lag-spelschema]
         (->> lag-spelschema
              (filter (fn [omgång]
                        (-> (clojure.string/split omgång #"-")
                            (count)
                            (= 2))))
              (count)))
       spelschema))

(defn balanserad-tresnurr?
  {:test (fn []
           (is (balanserad-tresnurr? [["1-2" "3-4"]
                                      ["0-2" "2"]
                                      ["0-1" "1"]
                                      ["4" "0-4"]
                                      ["3" "0-3"]]))
           (is-not (balanserad-tresnurr? [["1" "1"]
                                          ["0" "0"]
                                          ["3-4" "3-4"]
                                          ["2-4" "2-4"]
                                          ["2-3" "2-3"]])))}
  [spelschema]
  (let [antal-tresnurrar (beräkna-antal-tresnurrar spelschema)]
    (<= (- (apply max antal-tresnurrar) (apply min antal-tresnurrar))
        1)))

(defn beräkna-antal-möten
  [lag-spelschema antal-lag]
  (->> lag-spelschema
       (reduce (fn [antal-möten omgång]
                 (reduce (fn [antal-möten motståndare]
                           (update antal-möten (read-string motståndare) inc))
                         antal-möten
                         (clojure.string/split omgång #"-")))
               (into [] (repeat antal-lag 0)))
       (sort)
       (drop 1)))

(defn balanserat-motstånd-lag-spelschema?
  {:test (fn []
           (is (balanserat-motstånd-lag-spelschema? ["1" "2" "3"] 4))
           (is (balanserat-motstånd-lag-spelschema? ["1-2" "2" "3"] 4))
           (is-not (balanserat-motstånd-lag-spelschema? ["1-2" "2" "2-3"] 4))
           (is-not (balanserat-motstånd-lag-spelschema? ["2" "2" "3"] 4)))}
  [lag-spelschema antal-lag]
  (let [sorterat-antal-möten (beräkna-antal-möten lag-spelschema antal-lag)
        högsta (last sorterat-antal-möten)
        lägsta (first sorterat-antal-möten)]
    (<= (- högsta lägsta)
        1)))

(defn balanserat-motstånd?
  {:test (fn []
           (is (balanserat-motstånd? [["1-2" "1-3" "1-4" "2-3" "2-4" "3-4"]
                                      ["0-2" "0-3" "0-4" "4" "3" "2"]
                                      ["0-1" "4" "3" "3" "4"]
                                      ["4" "1-0" "2" "0-2" "1" "0-4"]
                                      ["3" "2" "0-1" "1" "0-2" "0-3"]]))
           (is-not (balanserat-motstånd? [["1-2" "1-2"]
                                          ["0-2" "0-2"]
                                          ["0-1" "0-1"]
                                          ["4" "4"]
                                          ["3" "3"]])))}
  [spelschema]
  (let [antal-lag (count spelschema)]
    (reduce (fn [balanserat? lag-spelschema]
              (and balanserat? (balanserat-motstånd-lag-spelschema? lag-spelschema antal-lag)))
            true
            spelschema)))

(defn giltigt-spelschema?
  "Ett spelschema är giltigt om det är balanserat. Det är balanserat om man möter alla ungefär lika många gånger och alla har ungefär lika många tresnurrar. (Ungefär := skillnaden mellan högsta och minsta värde är max 1)"
  [spelschema]
  (and (balanserat-motstånd? spelschema)
       (balanserad-tresnurr? spelschema)))

(defn lägg-till-tresnurr
  [antal-lag]
  (let [blandad-lista (shuffle (range antal-lag))
        offren (take 3 blandad-lista)
        återstående-lag (drop 3 blandad-lista)]
    [(reduce (fn [omgång offer-lista-index]
               (condp = offer-lista-index
                 0
                 (assoc omgång (nth offren 0) (str (nth offren 1) "-" (nth offren 2)))

                 1
                 (assoc omgång (nth offren 1) (str (nth offren 0) "-" (nth offren 2)))

                 2
                 (assoc omgång (nth offren 2) (str (nth offren 1) "-" (nth offren 0)))))
             (into [] (repeat antal-lag nil))
             (range 3))
     återstående-lag]))

(defn slumpa-omgång
  [antal-lag]
  (loop [[omgång återstående-lag] (lägg-till-tresnurr antal-lag)]
    (if (= (count återstående-lag) 0)
      omgång
      (recur [(-> (assoc omgång (first återstående-lag) (str (second återstående-lag)))
                  (assoc (second återstående-lag) (str (first återstående-lag))))
              (drop 2 återstående-lag)]))))


(defn poängsätta-spelschema
  [spelschema]
  (let [antal-tresnurrar (beräkna-antal-tresnurrar spelschema)
        poäng-tresnurrar (- (apply max antal-tresnurrar) (apply min antal-tresnurrar))
        antal-lag (count spelschema)
        poäng-antal-möten (reduce (fn [summa lag-spelschema]
                                    (let [sorterat-antal-möten (beräkna-antal-möten lag-spelschema antal-lag)]
                                      (+ summa
                                         (- (last sorterat-antal-möten) (first sorterat-antal-möten)))))
                                  0
                                  spelschema)]
    (+ poäng-tresnurrar poäng-antal-möten)))

(defn slumpa-spelschema
  [antal-lag antal-omgångar]
  (reduce (fn [spelschema _]
            (let [omgång (slumpa-omgång antal-lag)]
              (reduce (fn [spelschema lag-index]
                        (update spelschema lag-index (fn [lag-spelschema]
                                                       (conj lag-spelschema (nth omgång lag-index)))))
                      spelschema
                      (range antal-lag))))
          (into [] (repeat antal-lag []))
          (range antal-omgångar)))

(defn generera-spelschema
  [antal-lag antal-omgångar]
  (loop [iter 1]
    (let [spelschema (slumpa-spelschema antal-lag antal-omgångar)]
      (if (giltigt-spelschema? spelschema)
        (do (println "Färdig!")
            (println spelschema)
            spelschema)
        (do (println iter)
            (recur (inc iter)))))))

(defn slumpa-poängsatt-spelschema
  [antal-lag antal-omgångar]
  (reduce (fn [spelschema _]
            ;; Hur många slumpningar man ska göra per omgång
            (->> (range 100)
              (reduce (fn [möjliga-spelscheman _]
                           (let [omgång (slumpa-omgång antal-lag)
                                 möjligt-spelschema (reduce (fn [spelschema lag-index]
                                                              (update spelschema lag-index (fn [lag-spelschema]
                                                                                             (conj lag-spelschema (nth omgång lag-index)))))
                                                            spelschema
                                                            (range antal-lag))]
                             (conj möjliga-spelscheman möjligt-spelschema)))
                         [])
                 (sort-by poängsätta-spelschema)
                 (first)))
          (into [] (repeat antal-lag []))
          (range antal-omgångar)))

(defn generera-poängsatt-spelschema
  [antal-lag antal-omgångar]
  (loop [iter 1]
    (let [spelschema (slumpa-poängsatt-spelschema antal-lag antal-omgångar)]
      (if (giltigt-spelschema? spelschema)
        (do (println "Färdig!")
            (println spelschema)
            (println (str "Poäng: " (poängsätta-spelschema spelschema)))
            spelschema)
        (do (println (str "Iter: " iter))
            (println (str "Poäng: " (poängsätta-spelschema spelschema)))
            (recur (inc iter)))))))

(comment

  (let [spelschema (slumpa-spelschema 7 7)]
    (println spelschema)
    (println (poängsätta-spelschema spelschema)))
  (let [spelschema (slumpa-poängsatt-spelschema 7 7)]
    (println spelschema)
    (println (poängsätta-spelschema spelschema)))
  (def spelschema (generera-spelschema 7 7))
  spelschema
  (giltigt-spelschema? spelschema)

  (def poängsatt-spelschema (generera-poängsatt-spelschema 17 11))
  )