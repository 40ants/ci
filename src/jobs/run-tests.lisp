(defpackage #:40ants-ci/jobs/run-tests
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:asd-system)
  (:import-from #:40ants-ci/utils
                #:dedent
                #:current-system-name)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:run-tests))
(in-package 40ants-ci/jobs/run-tests)


(defclass run-tests (40ants-ci/jobs/lisp-job:lisp-job)
  ((coverage :initarg :coverage
             :initform nil
             :reader coverage)
   (custom :initarg :custom
           :initform nil
           :reader custom)))


(defun run-tests (&rest rest &key coverage qlfile asd-system
                               os quicklisp lisp
                               exclude
                               custom)
  (declare (ignore coverage qlfile os quicklisp lisp asd-system exclude))
  (check-type custom
              (or null string list))
  (apply #'make-instance 'run-tests
         rest))


(defmethod 40ants-ci/jobs/job:steps ((job run-tests))
  (append
   (call-next-method)
   (list
    (action "Run Tests"
            "40ants/run-tests@v2"
            :asdf-system (or (asd-system job)
                             (current-system-name))
            :coveralls-token (when (coverage job)
                               (dedent
                                (if (40ants-ci/jobs/job:use-matrix-p job)
                                    "
                                    ${{ matrix.lisp == 'sbcl-bin' &&
                                        matrix.os == 'ubuntu-latest' &&
                                        matrix.quicklisp == 'ultralisp' &&
                                        secrets.github_token }}"
                                    "${{ secrets.github_token }}")))
            :run-tests (when-let ((custom-code (custom job)))
                         (etypecase custom-code
                           (string custom-code)
                           (list (with-output-to-string (s)
                                   (write custom-code :stream s)))))))))
