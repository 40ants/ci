(uiop:define-package #:40ants-ci/jobs/job
  (:use #:cl)
  (:import-from #:40ants-ci/utils
                #:to-env-alist
                #:ensure-list-of-plists)
  (:import-from #:40ants-ci/github)
  (:import-from #:serapeum
                #:soft-alist-of
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
           #:explicit-steps
           #:exclude
           #:job-env
           #:steps-before
           #:steps-after))
(in-package #:40ants-ci/jobs/job)


(defclass job ()
  ((name :initarg :name
         :reader name
         :documentation "If this name was not given in constructor, then name will be lowercased name of the job class.")
   (os :initform "ubuntu-latest"
       :initarg :os
       :reader os)
   (exclude :initform nil
            :initarg :exclude
            :reader exclude
            :documentation "A list of plists denoting matrix combinations to be excluded.")
   (env :initform nil
        :type (soft-alist-of string string)
        :initarg :env
        :documentation "An alist of environment variables and their values to be added on job level. Values are evaluated in runtime."
        :reader job-env)
   (steps :initform nil
          :initarg :steps
          :documentation "This slot holds steps given as a STEPS argument to a job constructor. Depending on a job class, it might add additional steps around these explicit steps."
          :reader explicit-steps)
   (steps-before :initform nil
                 :initarg :steps-before
                 :documentation "This slot holds steps given as a STEPS-BEFORE argument to a job constructor. These steps will be prepended to steps returned by the JOB class."
                 :reader steps-before)
   (steps-after :initform nil
                :initarg :steps-after
                :documentation "This slot holds steps given as a STEPS-AFTER argument to a job constructor. These steps will be appended to steps returned by the JOB class."
                :reader steps-after)
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


(defmethod initialize-instance :around ((job job) &rest initargs)
  (let* ((initargs (copy-list initargs))
         (env (getf initargs :env)))
    (when env
      (setf (getf initargs :env)
            (to-env-alist env)))

    (unless (getf initargs :name)
      (setf (getf initargs :name)
            (string-downcase
             (class-name (class-of job)))))

    
    (setf (getf initargs :steps)
          (mapcar #'ensure-step
                  (getf initargs :steps)))
    
    (apply #'call-next-method
           job
           initargs)))


(defmethod os :around ((job job))
  (uiop:ensure-list
   (call-next-method)))


(defgeneric steps (job)
  (:method ((job job))
    (explicit-steps job))
  
  (:method :around ((job job))
    (append
     (steps-before job)
     (uiop:ensure-list
      (call-next-method))
     (steps-after job))))


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
     (when (job-env job)
       (job-env job))
     
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
