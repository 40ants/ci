(defpackage #:40ants-ci/jobs/docs
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export #:build-docs))
(in-package 40ants-ci/jobs/docs)


(defclass build-docs (40ants-ci/jobs/lisp-job:lisp-job)
  ((error-on-warnings :initform t
                      :initarg :error-on-warnings
                      :reader error-on-warnings))
  (:documentation "Builds documentation and uploads it to GitHub using [\"40ants/build-docs\" github action](https://40ants.com/build-docs/)."))


(defun build-docs (&key asdf-system
                        (error-on-warnings t))
  "Creates a job of class BUILD-DOCS."
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
