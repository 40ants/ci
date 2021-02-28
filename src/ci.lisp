(defpackage #:40ants-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter
                #:linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package 40ants-ci/ci)


;; (40ants-ci/workflow:defworkflow docs-2
;;   :on-push-to "master"
;;   :by-cron "0 10 * * 1"
;;   :cache t
;;   :jobs ((40ants-ci/jobs/docs:build-docs)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((linter)
         (run-tests
          :os ("ubuntu-latest"
               "macos-latest")
          :quicklisp ("quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 "ccl-bin"
                 "allegro")
          ;; :exclude-from-matrix (;; Seems allegro is does not support 64bit OSX.
          ;;                       ;; Unable to install it using Roswell:
          ;;                       ;; alisp is not executable. Missing 32bit glibc?
          ;;                       (:os "macos-latest"
          ;;                        :lisp "allegro"))
          
          :coverage t
          :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}

                   github mgl-pax svetlyak40wt/mgl-pax :branch mgl-pax-minimal")))

