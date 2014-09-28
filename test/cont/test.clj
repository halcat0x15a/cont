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
          (do
            (yield 1)
            (yield 2)
            (yield 3)
            nil))
         [1 2 3])))

(deftest transformation
  (testing "if"
    (is (= (reset (if true :foo :bar)) :foo))
    (is (= (reset (if false :foo :bar)) :bar)))
  (testing "do"
    (is (= (reset (do)) nil))
    (is (= (reset (do 0)) 0))
    (is (= (reset (do :foo :bar)) :bar)))
  (testing "let"
    (is (= (reset (let [] 0)) 0))
    (is (= (reset (let [foo :foo] foo)) :foo))
    (is (= (reset (let [foo :foo bar :bar] foo bar)) :bar)))
  (testing "try"
    (is (= (reset (try (throw (Exception.)) (catch Exception e :foo))) :foo))
    (is (= (reset (try :foo (finally :bar))) :foo))
    (is (= (reset (try (throw (Exception.)) (catch Exception e :foo) (finally :bar))) :foo)))
  (testing "."
    (is (= (reset (Integer/parseInt "0")) 0))
    (is (= (reset (.toString 0)) "0")))
  (testing "new"
    (is (= (reset (new String "foo")) "foo"))))
