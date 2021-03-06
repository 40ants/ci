(defpackage #:40ants-ci/workflow
  (:use #:cl)
  (:import-from #:40ants-ci/github
                #:*current-system*)
  (:import-from #:40ants-ci/utils
                #:ensure-primary-system)
  (:export
   #:defworkflow))
(in-package 40ants-ci/workflow)


(defvar *registered-workflows*
  (make-hash-table :test 'equal)
  "This hash maps `package -> workflows` where \"workflows\" is another hash mapping workflow names to workflow objects.")


(defclass workflow ()
  ((name :initarg :name
         :reader name)
   (on-push-to :initform "master"
               :initarg :on-push-to
               :reader on-push-to)
   (on-pull-request :initform t
                    :initarg :on-pull-request
                    :reader on-pull-request)
   (by-cron :initform "0 10 * * 1"
            :initarg :by-cron
            :reader by-cron)
   (cache :initform t
          :initarg :cache
          :reader cache-p)
   (jobs :initform nil
         :initarg :jobs
         :reader jobs)))

(defmethod on-push-to :around ((workflow workflow))
  (uiop:ensure-list
   (call-next-method)))

(defmethod by-cron :around ((workflow workflow))
  (uiop:ensure-list
   (call-next-method)))

(defmethod jobs :around ((workflow workflow))
  (uiop:ensure-list
   (call-next-method)))


(defun make-job (name)
  (let* ((name (uiop:ensure-list name))
         (symbol (car name))
         (args (cdr name)))
    (if (fboundp symbol)
        (apply symbol args)
        (apply #'make-instance symbol args))))


(defun register-workflow (workflow &key (package *package*))
  (unless (gethash (package-name package)
                   *registered-workflows*)
    (setf (gethash (package-name package)
                   *registered-workflows*)
          (make-hash-table :test 'equal)))
  
  (setf (gethash (slot-value workflow 'name)
                 (gethash (package-name package)
                          *registered-workflows*))
        workflow)
  (values))


(defun package-workflows (&optional (package *package*))
  (let ((workflows-hash (gethash (package-name package)
                                 *registered-workflows*)))
    (when workflows-hash
      (loop for workflow being the hash-values of workflows-hash
            collect workflow))))


(defmacro defworkflow (name &key
                              on-push-to
                              by-cron
                              on-pull-request
                              cache
                              jobs)
  `(progn
     (defclass ,name (workflow)
       ())
     (let* ((jobs (mapcar #'make-job ',jobs))
            (workflow (make-instance ',name
                                     :name ',name
                                     :jobs jobs
                                     :on-push-to ',(uiop:ensure-list on-push-to)
                                     :by-cron ',(uiop:ensure-list by-cron)
                                     :on-pull-request ,on-pull-request
                                     :cache ,cache)))
       (register-workflow workflow)
       (on-workflow-redefinition workflow)
       workflow)))


(defgeneric make-triggers (workflow)
  (:method ((workflow workflow))
    (append
     (when (on-push-to workflow)
       `(("push" . (("branches" . ,(on-push-to workflow))))))
     (when (on-pull-request workflow)
       '(("pull_request" . :null)))
     
     (when (by-cron workflow)
       `(("schedule" .
                     ,(loop for schedule in (by-cron workflow)
                            collect `(("cron" . ,schedule)))))))))


(defmethod 40ants-ci/github:prepare-data ((workflow workflow))
  (let* ((40ants-ci/vars:*use-cache* (cache-p workflow))
         (triggers (make-triggers workflow))
         (jobs (uiop:ensure-list
                (jobs workflow))))

    (append
     `(("name" . ,(symbol-name
                   (name workflow))))
     
     (when triggers
       `(("on" . ,triggers)))

     (when jobs
       `(("jobs" .
                 ,(loop for job in jobs
                        for job-name = (string-downcase
                                        (class-name (class-of job)))
                        for job-data = (40ants-ci/github:prepare-data job)
                        collect `(,job-name . ,job-data))))))))


(defun make-workflow-path (base-path workflow)
  (uiop:merge-pathnames*
   (make-pathname :name (string-downcase
                         (symbol-name
                          (name workflow)))
                  :type "yml")
   base-path))


(defmethod 40ants-ci/github:generate ((package package) path)
  ;; For system, we try to find all system packages
  ;; and execute generate for each of them:
  (loop for workflow in (package-workflows package)
        for workflow-path = (make-workflow-path path workflow)
        appending (40ants-ci/github:generate workflow
                                             workflow-path)))


(defmethod 40ants-ci/github:generate ((workflow workflow) path)
  (let* ((data (40ants-ci/github:prepare-data workflow))
         (json (40ants-ci/utils:to-json data)))
    (unless path
      (error "Please, provide a path to save JSON data to."))

    (ensure-directories-exist path)
    (alexandria:with-output-to-file (output path :if-exists :supersede)
      (write-string json output)
      (list path))))


(defgeneric on-workflow-redefinition (workflow)
  (:documentation "This hook can be redefine by user.
                   It will be called each time when
                   you eval defworkflow either because file compilation
                   or manually by hitting C-c C-c in the Emacs.

                   Default behaviour is to generate a new
                   output for this workflow. Current system is set
                   to the primary system with same package name
                   as a package where defworkflow is used.")
  (:method ((workflow workflow))
    (let* ((system-name (string-downcase
                         (package-name *package*)))
           (*current-system*
             (ensure-primary-system
              (asdf:find-system system-name)))
           (system-path (40ants-ci/utils:make-github-workflows-path *current-system*))
           (workflow-path (make-workflow-path system-path
                                              workflow)))
      (40ants-ci/github:generate workflow workflow-path))))
