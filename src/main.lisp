(defpackage #:40ants-ci/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain))
(in-package #:40ants-ci/main)


(defmain main ((version "Show program version and exit."
                        :flag t)
               &subcommand)
  "Show information about Lisp implementation and given systems. Useful when collecting information for bugreports."

  (when version
    (let* ((system (asdf:find-system :40ants-ci))
           (version (asdf:component-version system)))
      (format t "Version: ~A~%" version)
      (uiop:quit 0))))


(defmain:defcommand (main generate) ()
  (format t "Generating~%"))
