(uiop:define-package #:40ants-ci-test/core
  (:use #:cl)
  (:import-from #:hamcrest/rove
                #:contains
                #:assert-that)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest))
(in-package 40ants-ci-test/core)


(deftest test-hello-world
    (testing "Just example"
             (ok (equal (length "Foo")
                        3))))
