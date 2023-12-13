(uiop:define-package #:40ants-ci/jobs/lisp-job
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/job
                #:os)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/utils
                #:single
                #:dedent
                #:current-system-name)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/vars
                #:*use-cache*)
  (:import-from #:serapeum
                #:length<)
  (:import-from #:alexandria
                #:length=)
  (:export #:lisp-job
           #:asdf-system
           #:lisp
           #:quicklisp))
(in-package 40ants-ci/jobs/lisp-job)


(defclass lisp-job (40ants-ci/jobs/job:job)
  ((quicklisp :initform "quicklisp"
                 :initarg :quicklisp
                 :reader quicklisp)
   (lisp :initform "sbcl-bin"
         :initarg :lisp
         :reader lisp)
   (qlfile :initarg :qlfile
           :initform nil
           :reader qlfile)
   (asdf-system :initarg :asdf-system
                :initform nil
                :type (or null string)
                :reader asdf-system)
   (asdf-version :initarg :asdf-version
                 :initform nil
                 :type (or null string)
                 :documentation "ASDF version to use when setting up Lisp environment. If NIL, then the latest will be used."
                 :reader asdf-version)
   (roswell-version :initarg :roswell-version
                    :initform nil
                    :type (or null string)
                    :documentation "Roswell version to use when setting up Lisp environment. If NIL, then will be used version, pinned in SETUP-LISP github action."
                    :reader roswell-version)
   (qlot-version :initarg :qlot-version
                 :initform nil
                 :type (or null string)
                 :documentation "Qlot version to use when setting up Lisp environment. If NIL, then will be used version, pinned in SETUP-LISP github action."
                 :reader qlot-version))
  (:documentation "This job checkouts the sources, installs Roswell and Qlot. Also, it caches results between runs."))


(defmethod lisp :around ((job lisp-job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod quicklisp :around ((job lisp-job))
  (uiop:ensure-list
   (call-next-method)))


(defmethod 40ants-ci/jobs/job:use-matrix-p ((job lisp-job))
  (or (call-next-method)
      (length< 1 (lisp job))
      (length< 1 (quicklisp job))))


(defmethod 40ants-ci/jobs/job:make-matrix ((job lisp-job))
  (append
   (call-next-method)
   
   (when (length< 1 (quicklisp job))
     `(("quicklisp" . ,(quicklisp job))))
   (when (length< 1 (lisp job))
     `(("lisp" . ,(lisp job))))))


(defmethod 40ants-ci/jobs/job:make-env ((job lisp-job))
  (append
   (call-next-method)
   (cond
     ((length< 1 (quicklisp job))
      `(("QUICKLISP_DIST" . "${{ matrix.quicklisp }}")))
     ((length= 1 (quicklisp job))
      `(("QUICKLISP_DIST" . ,(first (quicklisp job)))))
     (t
      nil))
   (cond
     ((length< 1 (lisp job))
      `(("LISP" . "${{ matrix.lisp }}")))
     ((length= 1 (lisp job))
      `(("LISP" . ,(first (lisp job)))))
     (t
      nil))))


(defmethod asdf-system :around ((job lisp-job))
  (or (call-next-method)
      (current-system-name)))


(defgeneric make-cache-key (job)
  (:method ((job lisp-job))
    (with-output-to-string (s)
      (write-string "a-${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-" s)
      (let ((os (os job)))
        (if (single os)
            (format s "~A-" (first os))
            (write-string "${{ matrix.os }}-" s)))
      (let ((quicklisp (quicklisp job)))
        (if (single quicklisp)
            (format s "~A-" (first quicklisp))
            (write-string "${{ matrix.quicklisp }}-" s)))
      (let ((lisp (lisp job)))
        (if (single lisp)
            (format s "~A-" (first lisp))
            (write-string "${{ matrix.lisp }}-" s)))
      ;; Here we need to hash *.asd files to make cache different
      ;; for each project. Cache content will depend not only
      ;; on qlfile, but also on ASD systems installed during
      ;; the build.
      (write-string "${{ hashFiles('qlfile.lock', '*.asd') }}" s))))


(defgeneric make-cache-steps (job)
  (:method ((job lisp-job))
    (when 40ants-ci/vars:*use-cache*
      (let ((paths-to-cache
              (list "qlfile"
                    "qlfile.lock"
                    "~/.cache/common-lisp/"
                    "~/.roswell"
                    "/usr/local/etc/roswell"
                    "/usr/local/bin/ros"
                    ;; On OSX Roswell is installed
                    ;; using Homebrew and /usr/local/bin/ros
                    ;; is a symlink into a Cellar directory:
                    "/usr/local/Cellar/roswell"
                    ".qlot")))
        (list (sh "Grant All Perms to Make Cache Restoring Possible"
                  "sudo mkdir -p /usr/local/etc/roswell
                 sudo chown \"${USER}\" /usr/local/etc/roswell
                 # Here the ros binary will be restored:
                 sudo chown \"${USER}\" /usr/local/bin")
              (sh "Get Current Month"
                  "echo \"value=$(date -u \"+%Y-%m\")\" >> $GITHUB_OUTPUT"
                  :id "current-month")
              (action "Cache Roswell Setup"
                      "actions/cache@v3"
                      :id "cache"
                      :path (format nil "~{~A~^~%~}" paths-to-cache)
                      :key (make-cache-key job))
              (sh "Restore Path To Cached Files"
                  "echo $HOME/.roswell/bin >> $GITHUB_PATH
                 echo .qlot/bin >> $GITHUB_PATH"
                  :if "steps.cache.outputs.cache-hit == 'true'"))))))


(defmethod 40ants-ci/jobs/job:steps ((job lisp-job))
  (append (list
           (action "Checkout Code"
                   "actions/checkout@v3"))
          (make-cache-steps job)
          (list
           (action "Setup Common Lisp Environment"
                   "40ants/setup-lisp@v3"
                   :asdf-system (asdf-system job)
                   :asdf-version (asdf-version job)
                   :roswell-version (roswell-version job)
                   :qlot-version (qlot-version job)
                   :qlfile-template (when (qlfile job)
                                      (dedent (qlfile job)))
                   :if (when *use-cache*
                         "steps.cache.outputs.cache-hit != 'true'")))
          (call-next-method)))
