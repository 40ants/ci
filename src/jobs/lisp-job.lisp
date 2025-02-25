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
  (:import-from #:40ants-ci/vars
                #:*use-cache*)
  (:import-from #:serapeum
                #:length<)
  (:import-from #:alexandria
                #:length=)
  (:export #:lisp-job
           #:asdf-system
           #:lisp
           #:quicklisp
           #:qlfile
           #:asdf-version
           #:roswell-version
           #:qlot-version
           #:dynamic-space-size))
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
                    :documentation "Roswell version to use when setting up Lisp environment. If NIL, then will be used version, pinned in `setup-lisp` github action."
                    :reader roswell-version)
   (dynamic-space-size :initarg :dynamic-space-size
                       :initform nil
                       :type (or null string)
                       :documentation "Dynamic space size for SBCL."
                       :reader dynamic-space-size)
   (qlot-version :initarg :qlot-version
                 :initform nil
                 :type (or null string)
                 :documentation "Qlot version to use when setting up Lisp environment. If NIL, then will be used version, pinned in `setup-lisp` github action."
                 :reader qlot-version)
   (checkout-submodules :initarg :checkout-submodules
                        :initform nil
                        :type (or null boolean)
                        :documentation "If this flag is true, then we will command actions/checkout action to checkout submodules."
                        :reader checkout-submodules))
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


(defmethod 40ants-ci/jobs/job:steps ((job lisp-job))
  (append (list
           (action "Checkout Code"
                   "actions/checkout@v4"
                   :submodules (when (checkout-submodules job)
                                 t)))
          (list
           (action "Setup Common Lisp Environment"
                   "40ants/setup-lisp@v4"
                   :asdf-system (asdf-system job)
                   :asdf-version (asdf-version job)
                   :roswell-version (roswell-version job)
                   :qlot-version (qlot-version job)
                   :qlfile-template (when (qlfile job)
                                      (dedent (qlfile job)))
                   :dynamic-space-size (dynamic-space-size job)
                   :cache (if *use-cache*
                            "true"
                            "false")))
          (call-next-method)))
