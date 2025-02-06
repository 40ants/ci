(uiop:define-package #:40ants-ci/jobs/docs
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export #:build-docs
           #:error-on-warnings))
(in-package #:40ants-ci/jobs/docs)


(defclass build-docs (40ants-ci/jobs/lisp-job:lisp-job)
  ((error-on-warnings :initform t
                      :initarg :error-on-warnings
                      :reader error-on-warnings))
  (:documentation "Builds documentation and uploads it to GitHub using [\"40ants/build-docs\" github action](https://40ants.com/build-docs/)."))


(defun build-docs (&rest args
                   &key
                   (error-on-warnings t)
                   ;; Settings from base JOB class
                   os
                   permissions
                   exclude
                   env
                   steps
                   ;; Settings from base LISP-JOB class
                   roswell-version
                   asdf-version
                   qlot-version
                   quicklisp
                   lisp
                   asdf-system
                   qlfile
                   dynamic-space-size)
  "Creates a job of class BUILD-DOCS."
  (declare (ignore asdf-system error-on-warnings
                   os permissions exclude env steps
                   roswell-version asdf-version qlot-version
                   quicklisp lisp qlfile dynamic-space-size))
  (apply #'make-instance
         'build-docs args))


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
