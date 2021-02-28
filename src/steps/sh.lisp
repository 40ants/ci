(defpackage #:40ants-ci/steps/sh
  (:use #:cl)
  (:import-from #:40ants-ci/steps/step)
  (:import-from #:alexandria
                #:remove-from-plistf)
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


(defun sh (name command &rest env &key
                                    id
                                    if
                                    (shell *default-shell*)
           &allow-other-keys)
  (remove-from-plistf env :id :if :shell)
  (make-instance 'sh
                 :name name
                 :command command
                 :id id
                 :if if
                 :shell shell
                 :env env))



(defmethod 40ants-ci/github:prepare-data ((sh sh))
  `(("run" . ,(command sh))
    ("shell" . , (shell sh))))
