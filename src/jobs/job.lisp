(uiop:define-package #:40ants-ci/jobs/job
  (:use #:cl)
  (:import-from #:40ants-ci/utils
                #:ensure-list-of-plists)
  (:import-from #:40ants-ci/github)
  (:import-from #:serapeum
                #:length<)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:40ants-ci/steps/step
                #:ensure-step)
  (:export #:job
           #:use-matrix-p
           #:steps
           #:os
           #:name
           #:make-matrix
           #:make-env
           #:permissions
           #:make-permissions
           #:explicit-steps))
(in-package #:40ants-ci/jobs/job)


(defclass job ()
  ((name :initarg :name
         :reader name)
   (os :initform "ubuntu-latest"
       :initarg :os
       :reader os)
   (exclude :initform nil
            :initarg :exclude
            :reader exclude
            :documentation "A list of plists denoting matrix combinations to be excluded.")
   (steps :initform nil
          :initarg :steps
          :documentation "This slot holds steps given as a STEPS argument to a job constructor. Depending on a job class, it might add additional steps around these explicit steps."
          :reader explicit-steps)
   (permissions :initform nil
                :initarg :permissions
                :documentation "A plist of permissions need for running the job.

                                These permissions will be bound to `secrets.GITHUB_TOKEN` variable.
                                Use default-initargs to override permissions in subclasses:

                                ```lisp
                                (:default-initargs
                                 :permissions '(:content \"write\"))
                                ```"
                :reader permissions)))


(defmethod initialize-instance :after ((job job) &rest initargs)
  (declare (ignore initargs))
  
  (unless (slot-boundp job 'name)
    (setf (slot-value job 'name)
          (string-downcase
           (class-name (class-of job)))))
  
  (setf (slot-value job 'steps)
        (mapcar #'ensure-step
                (slot-value job 'steps))))


(defmethod os :around ((job job))
  (uiop:ensure-list
   (call-next-method)))


(defgeneric steps (job)
  (:method ((job job))
    (explicit-steps job))
  
  (:method :around ((job job))
    (uiop:ensure-list
     (call-next-method))))


(defmethod exclude :around ((job job))
  (ensure-list-of-plists
   (call-next-method)))


(defgeneric use-matrix-p (job)
  (:method ((job job))
    (length< 1 (os job))))


(defgeneric make-matrix (job)
  (:method ((job job))
    (append
     (when (length< 1 (os job))
       `(("os" . ,(os job))))
     (when (exclude job)
       `(("exclude" .
                    ,(mapcar #'40ants-ci/utils:plist-to-alist
                             (exclude job))))))))


(defgeneric make-env (job)
  (:method ((job job))
    (append
     (cond
       ((length< 1 (os job))
        `(("OS" . "${{ matrix.os }}")))
       ((length= 1 (os job))
        `(("OS" . ,(first (os job)))))
       (t
        nil)))))


(defgeneric make-steps (job)
  (:method ((job job))
    (let ((steps (steps job)))
      (mapcar #'40ants-ci/github:prepare-data
              steps))))


(defgeneric make-runs-on (job)
  (:method ((job job))
    (if (length< 1 (os job))
        "${{ matrix.os }}"
        (first (os job)))))


(defgeneric make-permissions (job)
  (:documentation "Should return an alist with mapping from string to string where keys are scopes and values are permission names. Default method generates this alist from the plist of job's \"permissions\" slot.")
  (:method ((job job))
    (loop for (key value) on (permissions job) by #'cddr
          for key-as-str = (string-downcase key)
          for value-as-str = (string-downcase value)
          collect (cons key-as-str
                        value-as-str))))


(defmethod 40ants-ci/github:prepare-data ((job job))
  (append 
   (when (use-matrix-p job)
     `(("strategy" . (("fail-fast" . :false)
                      ("matrix" . ,(make-matrix job))))))

   (when (permissions job)
     (list (cons "permissions"
                 (make-permissions job))))

   `(("runs-on" . ,(make-runs-on job))
     ("env" . ,(make-env job))
     ("steps" . ,(make-steps job)))))
