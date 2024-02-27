(defpackage #:40ants-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/critic)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/autotag)
  (:import-from #:40ants-ci/jobs/lisp-job
                #:lisp-job))
(in-package 40ants-ci/ci)


(defworkflow release
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/autotag:autotag)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :by-cron "0 10 * * 1"
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("40ants-ci"
                         "40ants-ci-tests")
          :check-imports t)
         (40ants-ci/jobs/critic:critic
          :ignore-critiques ("function-too-long"
                             "check-prefix"))
         (run-tests
          :os ("ubuntu-latest"
               "macos-latest")
          :quicklisp ("quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 "ccl-bin")
          :coverage t
          :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}")
         (40ants-ci/jobs/lisp-job:lisp-job :name "check-ros-config"
                                           :lisp "ccl-bin"
                                           :steps ((40ants-ci/steps/sh:sh "Show Roswell Config"
                                                                          "ros config")))))


