(defpackage #:40ants-ci/steps/step
  (:use #:cl)
  (:shadow #:step)
  (:import-from #:40ants-ci/github)
  (:import-from #:40ants-ci/utils
                #:alistp
                #:plistp)
  (:export #:step
           #:step-id
           #:step-name))
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
        :reader env)
   (if :initarg :if
       :initform nil
       :reader step-if)))


(defgeneric make-env (step)
  (:method ((step step))
    (let ((env (env step)))
      (cond
        ((plistp env)
         (loop for (key value) on env by #'cddr
               collect (cons (symbol-name key)
                             value)))
        ((alistp env)
         env)
        (t
         (error "~A is not alisp or plist"
                env))))))


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
