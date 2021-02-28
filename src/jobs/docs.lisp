(defpackage #:40ants-ci/jobs/docs
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asd-system)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export #:build-docs))
(in-package 40ants-ci/jobs/docs)


(defclass build-docs (40ants-ci/jobs/lisp-job:lisp-job)
  ())


(defun build-docs ()
  (make-instance 'build-docs))


(defmethod 40ants-ci/jobs/job:steps ((job build-docs))
  (append
   (call-next-method)
   (list
    (action "Build Docs"
            "40ants/build-docs@v1"
            :asdf-system (or (asd-system job)
                             (current-system-name))))))
