(defsystem 40ants-ci-test
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               "40ants-ci-test/core")
  :description "Test system for 40ants-ci"

  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
