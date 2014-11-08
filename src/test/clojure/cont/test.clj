(ns cont.test
  (:require [clojure.test :refer :all]
            [cont.core :refer [shift reset]]))

(with-test
  (defn amb [& xs]
    (shift k (mapcat k xs)))
  (is (= (reset
           (let [x (amb 1 2 3)
                 y (amb 2 4 6)]
             (if (zero? (mod (+ x y) 3))
               [[x y]])))
         [[1 2] [2 4] [3 6]])))

(with-test
  (defn yield [x]
    (shift k (cons x (k nil))))
  (is (= (reset
           (yield 0)
           (yield 1)
           (yield 2))
         [0 1 2])))

(defmacro stack [& args]
  `((reset (let* [x# (do ~@args)] (fn* [_#] x#))) []))

(set-test stack
  (letfn [(push [x] (shift k (fn [s] ((k (cons x s)) (cons x s)))))
          (pop [] (shift k (fn [s] ((k (first s)) (next s)))))]
    (is (= (stack
             (push 0)
             (push 1)
             (push 2))
           [2 1 0]))
    (is (= (stack
             (push 0)
             (push 1)
             (pop)
             (push 2))
            [2 0]))
    (is (= (stack
             (push 0)
             (push 1)
             (push 2)
             (pop)
             (pop))
           1))))

(deftest transformation
  (testing "loop"
    (is (= (reset (loop [x 0] x)) 0)))
  (testing "letfn"
    (is (= (reset (letfn [(f [x] x)] (f 0))) 0)))
  (testing "throw"
    (is (thrown? Exception (reset (throw (Exception.))))))
  (testing "."
    (is (= (reset (Integer/parseInt "0")) 0))
    (is (= (reset (.toString 0)) "0")))
  (testing "new"
    (is (= (reset (new String "0")) "0"))))
