(defpackage #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export
   #:linter))
(in-package 40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ((asdf-systems :initarg :asdf-systems
                 :documentation "Linter can validate more than one system, but for the base class we need provide only one."
                 :reader asdf-systems)))


(defun linter (&key asdf-systems)
  "Creates a job which will run SBLint for given ASDF systems.

   If no ASD files given, it will use all ASD files from
   the current ASDF system."
  (setf asdf-systems
        (uiop:ensure-list asdf-systems))
  
  (make-instance 'linter
                 :asdf-system (first asdf-systems)
                 :asdf-systems asdf-systems))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Install SBLint"
        "
echo 'dist ultralisp http://dist.ultralisp.org' > qlfile
qlot update
qlot exec ros install 40ants-linter
")
    (sh "Run Linter"
        (format nil "qlot exec 40ants-linter --system \"~{~A~^, ~}\""
                (asdf-systems job))))))
