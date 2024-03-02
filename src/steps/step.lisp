(defpackage #:40ants-ci/steps/step
  (:use #:cl)
  (:shadow #:step)
  (:import-from #:40ants-ci/github)
  (:import-from #:40ants-ci/utils
                #:to-env-alist
                #:alistp
                #:plistp)
  (:import-from #:serapeum
                #:soft-alist-of)
  (:export #:step
           #:step-id
           #:step-name
           #:env
           #:step-if))
(in-package 40ants-ci/steps/step)


(defclass step ()
  ((id :initarg :id
       :initform nil
       :reader step-id)
   (name :initarg :name
         :initform nil
         :reader step-name)
   (env :initarg :env
        :initform nil
        :type (soft-alist-of string string)
        :documentation "An alist of environment variables."
        :reader env)
   (if :initarg :if
       :initform nil
       :reader step-if)))


(defmethod initialize-instance :around ((step step) &rest initargs)
  (let* ((initargs (copy-list initargs))
         (env (getf initargs :env)))
    (when env
      (setf (getf initargs :env)
            (to-env-alist env)))
    
    (apply #'call-next-method
           step
           initargs)))


(defgeneric make-env (step)
  (:method ((step step))
    (env step)))


(defmethod 40ants-ci/github:prepare-data ((step step))
  nil)


(defmethod 40ants-ci/github:prepare-data :around ((step step))
  (append
   (when (step-name step)
     `(("name" . ,(step-name step))))
   (when (step-id step)
     `(("id" . ,(step-id step))))

   (call-next-method)
   
   (when (env step)
     `(("env" . ,(make-env step))))
   
   (when (step-if step)
     `(("if" . ,(step-if step))))))


(defun ensure-step (step-or-step-definition)
  (check-type step-or-step-definition (or step list))
  (etypecase step-or-step-definition
    (step step-or-step-definition)
    (list
     (let ((head (car step-or-step-definition))
           (args (cdr step-or-step-definition)))
       
       (unless (symbolp head)
         (error "~A is not a correct step definition."
                step-or-step-definition))
       
       (if (fboundp head)
           (apply head args)
           (apply #'make-instance head args))))))
