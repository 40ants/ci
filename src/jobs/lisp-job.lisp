(defpackage #:40ants-ci/jobs/lisp-job
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/job
                #:os
                #:lisp
                #:quicklisp)
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
  (:export
   #:lisp-job
   #:asd-system))
(in-package 40ants-ci/jobs/lisp-job)


(defclass lisp-job (40ants-ci/jobs/job:job)
  ((qlfile :initarg :qlfile
           :initform nil
           :reader qlfile)
   (asd-system :initarg :asd-system
               :initform nil
               :reader asd-system))
  (:documentation "This job checkouts the sources, installs Roswell and Qlot. Also, it caches results between runs."))


(defmethod asd-system :around ((job lisp-job))
  (or (call-next-method)
      (current-system-name)))


;; ${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-${{ matrix.os }}-${{ matrix.quicklisp-dist }}-${{ matrix.lisp }}-${{ hashFiles('qlfile.lock') }}


(defgeneric make-cache-key (job)
  (:method ((job lisp-job))
    (with-output-to-string (s)
      (write-string "${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-" s)
      (let ((os (os job)))
        (if (single os)
            (format s "~A-" (first os))
            (write-string "${{ matrix.os }}-" s)))
      (let ((quicklisp (quicklisp job)))
        (if (single quicklisp)
            (format s "~A-" (first quicklisp))
            (write-string "${{ matrix.quicklisp-dist }}-" s)))
      (let ((lisp (lisp job)))
        (if (single lisp)
            (format s "~A-" (first lisp))
            (write-string "${{ matrix.lisp }}-" s)))
      (write-string "${{ hashFiles('qlfile.lock') }}" s))))


(defgeneric make-cache-steps (job)
  (:method ((job lisp-job))
    (when 40ants-ci/vars:*use-cache*
      (list (sh "Grant All Perms to Make Cache Restoring Possible"
                "sudo mkdir -p /usr/local/etc/roswell
                 sudo chown \"${USER}\" /usr/local/etc/roswell
                 # Here the ros binary will be restored:
                 sudo chown \"${USER}\" /usr/local/bin")
            (sh "Get Current Month"
                "echo \"::set-output name=value::$(date -u \"+%Y-%m\")\""
                :id "current-month")
            (action "Cache Roswell Setup"
                    "actions/cache@v2"
                    :id "cache"
                    :path "qlfile
                           qlfile.lock
                           /usr/local/bin/ros
                           ~/.cache/common-lisp/
                           ~/.roswell
                           /usr/local/etc/roswell
                           .qlot"
                    :key (make-cache-key job))
            (sh "Restore Path To Cached Files"
                "echo $HOME/.roswell/bin >> $GITHUB_PATH
                 echo .qlot/bin >> $GITHUB_PATH"
                :if "steps.cache.outputs.cache-hit == 'true'")))))


(defmethod 40ants-ci/jobs/job:steps ((job lisp-job))
  (append (list
           (action "Checkout Code"
                   "actions/checkout@v1"))
          (make-cache-steps job)
          (list
           (action "Setup Common Lisp Environment"
                   "40ants/setup-lisp@v1"
                   :asdf-system (asd-system job)
                   :qlfile-template (when (qlfile job)
                                      (dedent (qlfile job)))
                   :if (when *use-cache*
                         "if: steps.cache.outputs.cache-hit != 'true'")))
          (call-next-method)))
