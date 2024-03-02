(uiop:define-package #:40ants-ci
  (:nicknames #:40ants-ci/core)
  (:use #:cl)
  (:import-from #:40ants-ci/github)
  (:export #:generate))
(in-package #:40ants-ci)


(defun generate (system &key path)
  "Generates GitHub workflow for given ASDF system.

   This function searches workflow definitions in all packages
   of the given ASDF system.

   If PATH argument is not given, workflow files will be written
   to .github/workflow/ relarive to the SYSTEM."
  (40ants-ci/github:generate system path))

