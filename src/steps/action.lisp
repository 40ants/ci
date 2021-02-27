(defpackage #:40ants-ci/steps/action
  (:use #:cl)
  (:import-from #:40ants-ci/steps/step
                #:step-name
                #:step-id)
  (:import-from #:40ants-ci/github)
  (:export #:action))
(in-package 40ants-ci/steps/action)


(defclass action (40ants-ci/steps/step:step)
  ((action-args :initarg :args
                :reader action-args
                :documentation "A plist to be passed as \"with\" dictionary to the action.")))


(defun action (name &rest args)
  (make-instance 'action
                 :name name
                 :args args))


(defmethod 40ants-ci/github:prepare-data ((action action))
  (append
   `(("uses" . ,(step-name action)))
   (when (action-args action)
     `(("with" . ,(loop for (name value) on (action-args action) by #'cddr
                        collect (cons (string-downcase
                                       (symbol-name name))
                                      value)))))))
