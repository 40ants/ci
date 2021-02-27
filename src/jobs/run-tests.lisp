(defpackage #:40ants-ci/jobs/run-tests
  (:use #:cl)
  (:import-from #:40ants-ci/steps/sh
                #:sh)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:import-from #:40ants-ci/jobs/lisp-job)
  (:import-from #:40ants-ci/utils
                #:dedent
                #:current-system-name)
  (:export
   #:run-tests))
(in-package 40ants-ci/jobs/run-tests)


(defclass run-tests (40ants-ci/jobs/lisp-job:lisp-job)
  ((coverage :initarg :coverage
             :reader coverage)))


(defun run-tests (&rest rest &key coverage qlfile os quicklisp lisp)
  (declare (ignore coverage qlfile os quicklisp lisp))
  (apply #'make-instance 'run-tests
         rest))


(defmethod 40ants-ci/jobs/job:steps ((job run-tests))
  (append
   (call-next-method)
   (list
    (action "40ants/run-tests@v2"
            :asdf-system (current-system-name)
            :coveralls-token (when (coverage job)
                               (dedent
                                (if (40ants-ci/jobs/job:use-matrix-p job)
                                    "
                                    ${{ matrix.lisp == 'sbcl-bin' &&
                                        matrix.os == 'ubuntu-latest' &&
                                        matrix.quicklisp-dist == 'ultralisp' &&
                                        secrets.github_token }}"
                                    "${{ secrets.github_token }}")))))))
