(defpackage #:40ants-ci/steps/sh
  (:use #:cl)
  (:import-from #:40ants-ci/steps/step
                #:step-name
                #:step-id)
  (:export
   #:sh))
(in-package 40ants-ci/steps/sh)


(defvar *default-shell* "bash")


(defclass sh (40ants-ci/steps/step:step)
  ((command :initarg :command
            :reader command)
   (shell :initarg :shell
          :initform *default-shell*
          :reader shell)))


(defun sh (name command &key
                          id
                          (shell *default-shell*))
  (make-instance 'sh
                 :name name
                 :command command
                 :id id
                 :shell shell))


(defmethod 40ants-ci/github:prepare-data ((sh sh))
  (append
   (when (step-name sh)
     `(("name" . ,(step-name sh))))
   (when (step-id sh)
     `(("id" . ,(step-id sh))))
   `(("run" . ,(command sh))
     ("shell" . , (shell sh)))))
