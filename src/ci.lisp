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
  :env ((:custom-env-hello . "Hello"))
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "40ants-ci-docs"
          :env ((:custom-env-world . "World!")))))


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
          ;; CCL does not work on arm64 architecture yet
          :exclude ((:os "macos-latest" :lisp "ccl-bin"))
          :coverage t
          :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}")
         ;; This is an example of a job with a custom steps:
         (40ants-ci/jobs/lisp-job:lisp-job :name "custom-steps"
                                           :lisp "ccl-bin"
                                           :steps ((40ants-ci/steps/sh:sh "Show Roswell Config"
                                                                          "ros config")
                                                   (40ants-ci/steps/sh:sh "Custom ENV"
                                                                          "echo $CUSTOM_ENV"
                                                                          :env (:custom-env "Hello world!"))))))

