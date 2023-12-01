(uiop:define-package #:40ants-ci/jobs/job
  (:use #:cl)
  (:import-from #:40ants-ci/utils
                #:ensure-list-of-plists)
  (:import-from #:40ants-ci/github)
  (:export #:job
           #:use-matrix-p
           #:steps
           #:os
           #:name
           #:make-matrix
           #:make-env))
(in-package 40ants-ci/jobs/job)


(defclass job ()
  ((name :initarg :name
         :reader name)
   (os :initform "ubuntu-latest"
       :initarg :os
       :reader os)
   (exclude :initform nil
            :initarg :exclude
            :reader exclude
            :documentation "A list of plists denoting matrix combinations to be excluded.")
   (steps :initform nil
          :initarg :steps
          :reader steps)))


(defmethod initialize-instance :after ((job job) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp job 'name)
    (setf (slot-value job 'name)
          (string-downcase
           (class-name (class-of job))))))

(defmethod os :around ((job job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod steps :around ((job job))
  (uiop:ensure-list
   (call-next-method)))

(defmethod exclude :around ((job job))
  (ensure-list-of-plists
   (call-next-method)))


;; ignore-critiques: length=num
(defgeneric use-matrix-p (job)
  (:method ((job job))
    (or (> (length (os job)) 1))))


;; ignore-critiques: length=num
(defgeneric make-matrix (job)
  (:method ((job job))
    (append
     (when (> (length (os job)) 1)
       `(("os" . ,(os job))))
     (when (exclude job)
       `(("exclude" .
                    ,(mapcar #'40ants-ci/utils:plist-to-alist
                             (exclude job))))))))


;; ignore-critiques: length=num
(defgeneric make-env (job)
  (:method ((job job))
    (append
     (if (= (length (os job)) 1)
         `(("OS" . ,(first (os job))))
         `(("OS" . "${{ matrix.os }}"))))))


(defgeneric make-steps (job)
  (:method ((job job))
    (let ((steps (steps job)))
      (mapcar #'40ants-ci/github:prepare-data
              steps))))


;; ignore-critiques: length=num
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
