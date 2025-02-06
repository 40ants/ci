(uiop:define-package #:40ants-ci-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-ci-docs/changelog)


(defchangelog (:ignore-words ("40ANTS-DOC"
                              "ASDF"
                              "CI"
                              "secrets.DEPLOY_TRIGGER_TOKEN"
			      "DEPLOY_TRIGGER_TOKEN"
                              "secrets.GITHUB_TOKEN"
			      "GITHUB_TOKEN"
                              "OSX")
               :external-docs ("https://40ants.com/40ants-asdf-system/"))
  (0.17.0 2025-02-06
          "
Added
=====

Functions for creation jobs now accept two new arguments:

- STEPS-BEFORE argument allows to specify a list of steps to be performed before the job. For example, this can be used to install some system packages required for loading ASDF systems during the job execution.
- STEPS-AFTER argument is the same as previous one, but executes steps after the job.
")
  (0.16.0 2024-12-14
          "
Added
=====

Now dynamic space size can be given for lisp steps.

There are two ways to set it:

```
(build-docs
  :asdf-system \"cl-telegram-bot-docs\"
  :env ((\"DYNAMIC_SPACE_SIZE\" . \"4Gb\")))
```

This way it will be applied only to the step of the documentation building,
because [docs-builder script](https://github.com/40ants/docs-builder) allows to use
such environment variable.

But if you CI process fails to compile the ASDF system because of the memory limit,
then you need to set dynamic space size on the earlier state - during \"Setup Lisp\"
step. For this case an argument DYNAMIC-SPACE-SIZE can be given:

```
(build-docs
 :asdf-system \"cl-telegram-bot-docs\"
 :dynamic-space-size \"4gb\")
```

")
  (0.15.0 2024-03-02
          "
New
===

* Now you can specify ENV argument to 40ANTS-CI/WORKFLOW:DEFWORKFLOW and any job. This should be an alist where keys are strings and values are evaluated during GitHub workflow generation phase. Read more in 40ANTS-CI-DOCS/INDEX::@ENV section.
* Also, 40ANTS-CI/JOBS/AUTOTAG:AUTOTAG function now ignores TOKEN-PATTERN argument if ENV argument was given and has `GITHUB_TOKEN` value for whole job.

Backward incompatible changes
=============================

* When additional keyword arguments to 40ANTS-CI/STEPS/SH:SH function are given, they are transformed into env variables. Previously, their names were taken as is. Now they are uppercased and dash symbols are replaced with underscores.

")
  (0.14.0 2024-02-25
          "
Changed
=======

All jobs now use setup-lisp@v4 where internal caching was implemented.

Also all jobs were switched to from `actions/checkout@v3` to `actions/checkout@v4` action.
")
  (0.13.0 2023-12-14
          "
Changed
=======

Jobs now use setup-lisp@v3 action where a fix to quicklisp-client is applied.
This fix makes ql:quickload work with package-inferred systems.

If you want to use this fix in your own environments, you can find
it [here](https://github.com/40ants/quicklisp-client-fix).
")
  (0.12.0 2023-12-11
          "
Changed
=======

Use `secrets.GITHUB_TOKEN` instead of `secrets.DEPLOY_TRIGGER_TOKEN` and set required scopes for the token.
This way you don't have to setup a special secret for each repository or an organization.

")
  (0.11.0 2023-12-01
          "
Added
=====

New job class 40ANTS-CI/JOBS/AUTOTAG:AUTOTAG was added.

Use it like this:

```lisp
(defworkflow release
  :on-push-to \"master\"
  :jobs ((40ants-ci/jobs/autotag:autotag)))
```

and it will search for new semver tags in the ChangeLog.md file and push them to the git.

Changed
=======

Slots `quicklisp` and `lisp` were moved from class 40ANTS-CI/JOBS/JOB:JOB to 40ANTS-CI/JOBS/LISP-JOB:LISP-JOB.

Fixed
=====

Class 40ANTS-CI/JOBS/CRITIC:CRITIC was fixed for case when there are multiple critiques to ignore.
")
  (0.10.1 2023-03-08
          "- Fixed installation of the Linter. Now it depends on 40ANTS-ASDF-SYSTEM system.")
  (0.10.0 2022-11-10
          "- Now Linter does `qlot install --no-deps` and quickloads only those systems, which should be linted.
           - Also, 40ANTS-CI system now inherits from 40ANTS-ASDF-SYSTEM system.")
  (0.9.0 2022-11-10
         "- Fixed warnings about `set-output` and [outdated Node.js versions](https://github.blog/changelog/2022-09-22-github-actions-all-actions-will-begin-running-on-node16-instead-of-node12/) in checkout and cache actions.")
  (0.8.1 2022-09-18
         "- Fixed default value of asdf-systems slot of 40ANTS-CI/JOBS/LINTER:LINTER class.
          - Also, now linter accepts CHECK-IMPORTS argument and is able to warn on unused or missing imports in package-inferred systems.")
  (0.8.0 2022-03-21
         "- Fixed caching on OSX. Previously, job failed with
            `/Users/runner/.roswell/bin/qlot: line 4: exec: ros: not found` error
            if `:cache t` was given to a job running on OSX and Roswell was restored from a cache.")
  (0.7.0 2022-03-13
         "- 40ANTS-CI/JOBS/CRITIC:CRITIC function's argument IGNORE-CRITICUES was
            renames to the IGNORE-CRITIQUES argument.")
  (0.6.0 2022-02-21
         "- New job type \"critic\" was added. It advices how to make you Lisp code better.
            Learn more about this job type at 40ANTS-CI-DOCS/INDEX::@CRITIC section.")
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
