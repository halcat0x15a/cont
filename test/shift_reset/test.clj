(ns shift-reset.test
  (:require [clojure.test :refer :all]
            [shift-reset.core :refer [shift reset]]))

(with-test
  (defn amb [& xs]
    (shift k (mapcat k xs)))
  (= (reset
      (let [x (amb 1 2 3)
            y (amb 2 4 6)]
        (if (zero? (mod (+ x y) 3))
          [[x y]])))
     [[1 2] [2 4] [3 6]]))
