(uiop:define-package #:40ants-ci
  (:nicknames #:40ants-ci/core)
  (:use #:cl)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:reader)
  (:import-from #:40ants-ci/github)
  (:export #:generate))
(in-package 40ants-ci)


(defsection @index (:title "40Ants-CI - Github Workflow Generator")
  "
[![](https://github-actions.40ants.com/40ants/ci/matrix.svg)](https://github.com/40ants/ci/actions)

This is a small utility, which can generate GitHub workflows for Common Lisp
projects.

It generates workflow for running tests and building docs. These workflows
use [40ants/run-tests](https://40ants.com/run-tests) and [40ants/build-docs](https://40ants.com/build-doc)
actions and [SBLint](https://github.com/cxxxr/sblint) to check code for compilation errors.

Usage from Common Lisp
======================

This will generate two files, `.github/workflows/ci.yml` and
`.github/workflows/docs.yml`:

```
(ql:quickload :40ants-ci)

(40ants-ci:generate)
```

By default, utility searches for `*.asd` files in the current directory
and chooses one with shortest name. This will be the system to test.

API Reference
=============
"
  (generate function))


(defun generate (system &key path)
  (40ants-ci/github:generate system path))
