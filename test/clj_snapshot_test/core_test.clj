(ns clj-snapshot-test.core-test
  (:require [clojure.test :refer :all]
            [clj-snapshot-test.core :refer :all]))

(deftest snapshot-test
  (testing "snapshot assertion plugs into is"
    (is (matches-snapshot "test snapshot" (str "aaaaaaaa")))))

(deftest snapshot-test-2
  (testing "snapshot assertion plugs into is"
    (is (matches-snapshot "test snapshot 2" (str "aaaaaaaa")))))