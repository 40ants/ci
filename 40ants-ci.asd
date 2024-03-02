#-asdf3.1 (error "40ants-ci requires ASDF 3.1")
(defsystem "40ants-ci"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("40ants-ci/core"
               "40ants-ci/jobs/job"
               "40ants-ci/jobs/docs"
               "40ants-ci/jobs/linter"
               "40ants-ci/jobs/critic"
               "40ants-ci/jobs/run-tests"
               "40ants-ci/jobs/autotag")
  :description "A tool simplify continuous deployment for Common Lisp projects."
  :homepage "https://40ants.com/ci/"
  :source-control (:git "https://github.com/40ants/ci")
  :path-to-changelog "src/changelog.lisp"
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain")))
  :in-order-to ((test-op (test-op 40ants-ci-test))))

