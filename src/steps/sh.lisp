(defpackage #:40ants-ci/steps/sh
  (:use #:cl)
  (:import-from #:40ants-ci/steps/step)
  (:import-from #:40ants-ci/github)
  (:import-from #:40ants-ci/utils
                #:dedent)
  (:export #:sh
           #:sections
           #:command
           #:shell))
(in-package #:40ants-ci/steps/sh)


(defvar *default-shell* "bash")


(defclass sh (40ants-ci/steps/step:step)
  ((command :initarg :command
            :reader command)
   (shell :initarg :shell
          :initform *default-shell*
          :reader shell)))


;; ignore-critiques: if-no-else
(defun sh (name command &key id if (shell *default-shell*) env)
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


(defmacro sections (&body body)
  "Returns a string with a bash script where some parts are grouped.

   In this example we have 3 sections:

   ```lisp
   (sections
         (\"Help Argument\"
          \"qlot exec cl-info --help\")
         (\"Version Argument\"
          \"qlot exec cl-info --version\")
         (\"Lisp Systems Info\"
          \"qlot exec cl-info\"
          \"qlot exec cl-info cl-info defmain\"))
   ```

   It will be compiled into:

   ```bash
   echo ::group::Help Argument
   qlot exec cl-info --help
   echo ::endgroup::
   echo ::group::Version Argument
   qlot exec cl-info --version
   echo ::endgroup::
   echo ::group::Lisp Systems Info
   qlot exec cl-info
   qlot exec cl-info cl-info defmain
   echo ::endgroup::
   ```


"
  (with-output-to-string (s)
    (loop for (group-name . commands) in body
          do (format s "echo ::group::~A~%" group-name)
             (loop for command in commands
                   do (format s "~A~%"
                              (dedent command)))
             (format s "echo ::endgroup::~%"))))
