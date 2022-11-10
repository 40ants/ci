(defpackage #:40ants-ci/steps/action
  (:use #:cl)
  (:import-from #:40ants-ci/steps/step)
  (:import-from #:40ants-ci/github)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:export #:action))
(in-package 40ants-ci/steps/action)


(defclass action (40ants-ci/steps/step:step)
  ((uses :initarg :uses
         :reader uses)
   (action-args :initarg :args
                :reader action-args
                :documentation "A plist to be passed as \"with\" dictionary to the action.")))


;; ignore-critiques: if-no-else
(defun action (name uses &rest args &key id if env &allow-other-keys)
  (remove-from-plistf args :id :if :env)
  (make-instance 'action
                 :id id
                 :name name
                 :uses uses
                 :args args
                 :env env
                 :if if))


(defmethod 40ants-ci/github:prepare-data ((action action))
  (append
   `(("uses" . ,(uses action)))
   (when (action-args action)
     `(("with" . ,(loop for (name value) on (action-args action) by #'cddr
                        when value
                          collect (cons (string-downcase
                                         (symbol-name name))
                                        value)))))))
