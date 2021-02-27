(defpackage #:40ants-ci/vars
  (:use #:cl)
  (:export
   #:*current-system*))
(in-package 40ants-ci/vars)


(defvar *current-system*)
(setf (documentation '*current-system* 'variable)
      "When workflow is generated for ASDF system, this variable will contain a primary ASDF system.")


;; (defvar *written-files*)
;; (setf (documentation '*written-files* 'variable)
;;       "For collecting filenames and output them from 40ants-ci:generate.")
