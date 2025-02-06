(uiop:define-package #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export #:linter
           #:asdf-systems
           #:check-imports))
(in-package #:40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ((asdf-systems :initarg :asdf-systems
                 :initform nil
                 :type (or null list)
                 :documentation "Linter can validate more than one system, but for the base class we need provide only one."
                 :reader asdf-systems)
   (check-imports :initarg :check-imports
                  :initform nil
                  :type boolean
                  :documentation "Linter will check for missing or unused imports of package-inferred systems."
                  :reader check-imports)))


(defmethod asdf-systems :around ((job linter))
  (uiop:ensure-list
   (or (call-next-method)
       (current-system-name))))


(defun linter (&rest args
               &key
               asdf-systems
               check-imports
               ;; Settings from base JOB class
               os
               permissions
               exclude
               env
               steps
               steps-before
               steps-after
               permission
               ;; Settings from base LISP-JOB class
               roswell-version
               asdf-version
               qlot-version
               quicklisp
               lisp
               qlfile
               dynamic-space-size)
  "Creates a job which will run SBLint for given ASDF systems.

   If no ASD files given, it will use all ASD files from
   the current ASDF system."
  (declare (ignore check-imports os permissions exclude env
                   steps steps-before steps-after permissions
                   roswell-version asdf-version qlot-version
                   quicklisp lisp qlfile dynamic-space-size))
  
  (apply #'make-instance
         'linter
         :asdf-system (first asdf-systems)
         args))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Change dist to Ultralisp if qlfile does not exist"
        "if [[ ! -e qlfile ]]; then echo 'dist ultralisp http://dist.ultralisp.org' > qlfile; fi")
    (sh "Update Qlot"
        ;; Here we update qlot twice in case if the first
        ;; time will fail. I wasn't able to figure out and
        ;; reproduce this issue on my own machine, but inside
        ;; GitHub action container qlot update sometimes
        ;; fails on updating dependencies. The second run
        ;; fixes this issue. :(((
        "qlot update --no-deps")
    (sh "Install SBLint wrapper"
        ;; Linter has a defsystem dependency on 40ants-asdf-system
        ;; and roswell breaks if it is not installed, thus we need
        ;; to install both of them:
        "qlot exec ros install 40ants-asdf-system 40ants-linter")
    (sh "Run Linter"
        (format nil "qlot exec 40ants-linter --system \"~{~A~^, ~}\"~:[~; --imports~]"
                (or (asdf-systems job)
                    (list (asdf-system job)))
                (check-imports job))))))
