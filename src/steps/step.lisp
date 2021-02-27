(defpackage #:40ants-ci/steps/step
  (:use #:cl)
  (:shadow #:step)
  (:export
   #:step
   #:step-id
   #:step-name))
(in-package 40ants-ci/steps/step)


(defclass step ()
  ((id :initarg :id
       :initform nil
       :reader step-id)
   (name :initarg :name
         :initform nil
         :reader step-name)))
