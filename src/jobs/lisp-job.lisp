(defpackage #:40ants-ci/jobs/lisp-job
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:export
   #:lisp-job))
(in-package 40ants-ci/jobs/lisp-job)


(defclass lisp-job (40ants-ci/jobs/job:job)
  ()
  (:documentation "This job checkouts the sources, installs Roswell and Qlot. Also, it caches results between runs."))


(defmethod 40ants-ci/jobs/job:steps ((lisp-job lisp-job))
  (list* (action "actions/checkout@v1")
         (action "40ants/setup-lisp@v1"
                 :asdf-system (asdf:component-name 40ants-ci/github:*current-system*))
         (call-next-method)))
