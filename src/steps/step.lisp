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
