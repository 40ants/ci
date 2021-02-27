(defpackage #:40ants-ci/jobs/lisp-job
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/utils
                #:dedent
                #:current-system-name)
  (:export
   #:lisp-job
   #:asd-system))
(in-package 40ants-ci/jobs/lisp-job)


(defclass lisp-job (40ants-ci/jobs/job:job)
  ((qlfile :initarg :qlfile
           :initform nil
           :reader qlfile)
   (asd-system :initarg :asd-system
               :initform nil
               :reader asd-system))
  (:documentation "This job checkouts the sources, installs Roswell and Qlot. Also, it caches results between runs."))


(defmethod asd-system :around ((job lisp-job))
  (or (call-next-method)
      (current-system-name)))


(defmethod 40ants-ci/jobs/job:steps ((job lisp-job))
  (list* (action "actions/checkout@v1")
         (action "40ants/setup-lisp@v1"
                 :asdf-system (asd-system job)
                 :qlfile-template
                 (when (qlfile job)
                   (dedent (qlfile job))))
         (call-next-method)))
