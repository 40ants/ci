(defpackage #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asd-system)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export
   #:linter))
(in-package 40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ())


(defun linter (&key asd-system)
  "Creates a job which will run SBLint for given ASDF systems.

   If no ASD files given, it will use all ASD files from
   the current ASDF system."
  (make-instance 'linter
                 :asd-system asd-system))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Install SBLint"
        "qlot exec ros install cxxxr/sblint")
    (sh "Run Linter"
        (format nil "qlot exec sblint ~A.asd"
                (asd-system job))))))
