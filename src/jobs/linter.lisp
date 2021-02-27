(defpackage #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job)
  (:export
   #:linter))
(in-package 40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ())


(defun linter ()
  (make-instance 'linter))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Install SBLint"
        "qlot exec ros install cxxxr/sblint")
    (sh "Run Linter"
        "qlot exec sblint 40ants-ci.asd"))))
