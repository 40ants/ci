(uiop:define-package #:40ants-ci-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest))
(in-package 40ants-ci-tests/core)


(deftest test-hello-world
    (testing "Just example"
             (ok (equal (length "Foo")
                        3))))
