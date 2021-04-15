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
  ((error-on-warnings :initform t
                      :initarg :error-on-warnings
                      :reader error-on-warnings)))


(defun build-docs (&key asdf-system
                        (error-on-warnings t))
  (make-instance 'build-docs
                 :asdf-system asdf-system
                 :error-on-warnings error-on-warnings))


(defmethod 40ants-ci/jobs/job:steps ((job build-docs))
  (append
   (call-next-method)
   (list
    (action "Build Docs"
            "40ants/build-docs@v1"
            :asdf-system (or (asdf-system job)
                             (current-system-name))
            :error-on-warnings (if (error-on-warnings job)
                                   :true
                                   :false)))))
