(defpackage #:40ants-ci/vars
  (:use #:cl)
  (:export
   #:*current-system*))
(in-package 40ants-ci/vars)


(defvar *current-system*)
(setf (documentation '*current-system* 'variable)
      "When workflow is generated for ASDF system, this variable will contain a primary ASDF system.")


