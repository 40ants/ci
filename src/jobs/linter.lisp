(defpackage #:40ants-ci/jobs/linter
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/lisp-job)
  (:import-from #:40ants-ci/utils
                #:current-system-name)
  (:export
   #:linter))
(in-package 40ants-ci/jobs/linter)


(defclass linter (40ants-ci/jobs/lisp-job:lisp-job)
  ((asd-files :initarg :asd-files
              :initform nil
              :reader asd-files)))


(defun linter (&key asd-files)
  "Creates a job which will run SBLint for given ASDF systems.

   If no ASD files given, it will use all ASD files from
   the current ASDF system."
  (make-instance 'linter
                 :asd-files asd-files))


(defmethod asd-files :around ((job linter))
  (let ((files (uiop:ensure-list (call-next-method))))
    (or files
        (mapcar #'file-namestring
                (directory
                 (asdf:system-relative-pathname (current-system-name)
                                                #P"*.asd"))))))


(defmethod 40ants-ci/jobs/job:steps ((job linter))
  (append
   (call-next-method)
   (list
    (sh "Install SBLint"
        "qlot exec ros install cxxxr/sblint")
    (let ((files (asd-files job)))
      (sh "Run Linter"
          (format nil "qlot exec sblint ~{~A~^ ~}"
                  files))))))
