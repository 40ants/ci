(defpackage #:40ants-ci/jobs/run-tests
  (:use #:cl)
  (:export
   #:run-tests))
(in-package 40ants-ci/jobs/run-tests)


(defclass run-tests (40ants-ci/jobs/job:job)
  ((coverage :initarg :coverage)
   (qlfile :initarg :qlfile)))


(defun run-tests (&rest rest &key coverage qlfile os quicklisp lisp)
  (declare (ignore coverage qlfile os quicklisp lisp))
  (apply #'make-instance 'run-tests
         rest))
