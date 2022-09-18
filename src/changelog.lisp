(defpackage #:40ants-ci/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package 40ants-ci/changelog)


(defchangelog (:ignore-words ("40ANTS-DOC"
                              "ASDF"
                              "OSX"))
  (0.8.1 2022-09-18
         "- Fixed default value of asdf-systems slot of 40ANTS-CI/JOBS/LINTER:LINTER class.")
  (0.8.0 2022-03-21
         "- Fixed caching on OSX. Previously, job failed with
            `/Users/runner/.roswell/bin/qlot: line 4: exec: ros: not found` error
            if `:cache t` was given to a job running on OSX and Roswell was restored from a cache.")
  (0.7.0 2022-03-13
         "- 40ANTS-CI/JOBS/CRITIC:CRITIC function's argument IGNORE-CRITICUES was
            renames to the IGNORE-CRITIQUES argument.")
  (0.6.0 2022-02-21
         "- New job type \"critic\" was added. It advices how to make you Lisp code better.
            Learn more about this job type at 40ANTS-CI::@CRITIC section.")
  (0.5.0 2022-01-28
         "- Move the actions/checkout action from v1 to v2.")
  (0.4.0 2022-01-28
         "
- Now multiple jobs of the same type can be listed in the same workflow.
- Also, you can change a job's name using :NAME argument.")
  (0.3.0 2021-10-24
         "- Now jobs 40ANTS-CI/JOBS/LINTER:LINTER, 40ANTS-CI/JOBS/RUN-TESTS:RUN-TESTS and 40ANTS-CI/JOBS/DOCS:BUILD-DOCS
            support ASDF-VERSION argument.")
  (0.2.2 2021-06-18
         "- Fixed an occasional failure on `qlot update` inside linter workflow.
            Usually it happed when quicklisp distribution was updated and `qlfile.lock`
            changed.")
  (0.2.1 2021-04-22
         "- Linter step was fixed to use default
            ASDF system if it wasn't specified explicitly.")
  (0.2.0 2021-04-15
         "- Supported ERROR-ON-WARNINGS argument for documentation builder.
          - Argument ASD-SYSTEM was renamed to [ASDF-SYSTEM][argument].
          - Moved this project's documentation to 40ANTS-DOC system.")
  (0.1.1 2021-03-08
         "- Fixed the cache key to use `*.asd` files.")
  (0.1.0 2021-02-26
         "- Initial version."))
