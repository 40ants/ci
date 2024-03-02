(defsystem "40ants-ci-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides documentation for 40ants-ci."
  :source-control (:git "https://github.com/40ants/ci")
  :bug-tracker "https://github.com/40ants/ci/issues"
  :homepage "https://40ants.com/ci/"
  :pathname "docs"
  :depends-on ("40ants-ci"
               "40ants-ci-docs/index"))
