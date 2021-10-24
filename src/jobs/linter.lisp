(defpackage #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export #:linter))
(in-package 40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ((asdf-systems :initarg :asdf-systems
                 :documentation "Linter can validate more than one system, but for the base class we need provide only one."
                 :reader asdf-systems)))


(defun linter (&key asdf-systems asdf-version)
  "Creates a job which will run SBLint for given ASDF systems.

   If no ASD files given, it will use all ASD files from
   the current ASDF system."
  (setf asdf-systems
        (uiop:ensure-list asdf-systems))
  
  (make-instance 'linter
                 :asdf-system (first asdf-systems)
                 :asdf-systems asdf-systems
                 :asdf-version asdf-version))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Change dist to Ultralisp"
        "echo 'dist ultralisp http://dist.ultralisp.org' > qlfile")
    (sh "Update Qlot"
        ;; Here we update qlot twice in case if the first
        ;; time will fail. I wasn't able to figure out and
        ;; reproduce this issue on my own machine, but inside
        ;; GitHub action container qlot update sometimes
        ;; fails on updating dependencies. The second run
        ;; fixes this issue. :(((
        "qlot update || qlot update")
    (sh "Install SBLint wrapper"
        "qlot exec ros install 40ants-linter")
    (sh "Run Linter"
        (format nil "qlot exec 40ants-linter --system \"~{~A~^, ~}\""
                (or (asdf-systems job)
                    (list (asdf-system job))))))))
