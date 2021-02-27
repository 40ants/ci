(defpackage #:40ants-ci/jobs/job
  (:use #:cl)
  (:export
   #:job
   #:use-matrix-p
   #:steps))
(in-package 40ants-ci/jobs/job)


(defclass job ()
  ((os :initform "ubuntu-latest"
       :initarg :os
       :reader os)
   (lisp :initform "sbcl-bin"
         :initarg :lisp
         :reader lisp)
   (quicklisp :initform "quicklisp"
              :initarg :quicklisp
              :reader quicklisp)
   (steps :initform nil
          :initarg :steps
          :reader steps)))


(defmethod os :around ((job job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod lisp :around ((job job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod quicklisp :around ((job job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod steps :around ((job job))
  (uiop:ensure-list
   (call-next-method)))


(defgeneric use-matrix-p (job)
  (:method ((job job))
    (or (> (length (os job)) 1)
        (> (length (lisp job)) 1)
        (> (length (quicklisp job)) 1))))


(defgeneric make-matrix (job)
  (:method ((job job))
    (append
     (when (> (length (os job)) 1)
       `(("os" . ,(os job))))
     (when (> (length (quicklisp job)) 1)
       `(("quicklisp-dist" . ,(quicklisp job))))
     (when (> (length (lisp job)) 1)
       `(("lisp" . ,(lisp job)))))))


(defgeneric make-env (job)
  (:method ((job job))
    (append
     (if (= (length (os job)) 1)
         `(("OS" . ,(first (os job))))
         `(("OS" . "${{ matrix.os }}")))
     (if (= (length (quicklisp job)) 1)
         `(("QUICKLISP_DIST" . ,(first (quicklisp job))))
         `(("QUICKLISP_DIST" . "${{ matrix.quicklisp-dist}}")))
     (if (= (length (lisp job)) 1)
         `(("LISP" . ,(first (lisp job))))
         `(("LISP" . "${{ matrix.lisp }}}"))))))


(defgeneric make-steps (job)
  (:method ((job job))
    (let ((steps (steps job)))
      (mapcar #'40ants-ci/github:prepare-data
              steps))))


(defgeneric make-runs-on (job)
  (:method ((job job))
    (if (> (length (os job)) 1)
        "${{ matrix.os }}"
        (first (os job)))))


(defmethod 40ants-ci/github:prepare-data ((job job))
  (append 
   (when (use-matrix-p job)
     `(("strategy" . (("fail-fast" . :false)
                      ("matrix" . ,(make-matrix job))))))

   `(("runs-on" . ,(make-runs-on job))
     ("env" . ,(make-env job))
     ("steps" . ,(make-steps job)))))
