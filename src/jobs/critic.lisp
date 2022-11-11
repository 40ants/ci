(defpackage #:40ants-ci/jobs/critic
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asdf-system)
  (:export #:critic))
(in-package 40ants-ci/jobs/critic)


(defclass critic (40ants-ci/jobs/lisp-job:lisp-job)
  ;; TODO: add ability to ignore some critiques
  ((asdf-systems :initarg :asdf-systems
                 :documentation "Critic can validate more than one system, but for the base class we need provide only one."
                 :reader asdf-systems)
   (ignore-critiques :initarg :ignore-critiques
                     :documentation "A list strigns with names of critiques to ignore."
                     :reader ignore-critiques)))


(defun critic (&key asdf-systems asdf-version ignore-critiques)
  "Creates a job which will run Lisp Critic for given ASDF systems.

   If argument ASDF-SYSTEMS is NIL, it will use ASDF system
   to which current lisp file is belong.

   You may also provide ASDF-VERSION argument. It should be
   a string. By default, the latest ASDF version will be used."
  (check-type asdf-version (or null string))
  
  (let ((asdf-systems (uiop:ensure-list asdf-systems)))
    (make-instance 'critic
                   :asdf-system (first asdf-systems)
                   :asdf-systems asdf-systems
                   :asdf-version asdf-version
                   :ignore-critiques ignore-critiques)))


(defmethod 40ants-ci/jobs/job:steps ((job critic))
  (append
   (call-next-method)
   (list*
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
    (sh "Install LISP-CRITIC wrapper"
        "qlot exec ros install 40ants-critic")
    (loop for system in (or (asdf-systems job)
                            (list (asdf-system job)))
          collect (sh (format nil "Run Critic for ~S system" system)
                      (format nil "qlot exec lisp-critic~@[ --ignore ~{~A~^, ~}~] ~A"
                              (ignore-critiques job)
                              system))))))
