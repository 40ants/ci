(defpackage #:40ants-ci/jobs/docs
  (:use #:cl)
  (:export
   #:build-docs))
(in-package 40ants-ci/jobs/docs)


(defclass build-docs (40ants-ci/jobs/job:job)
  ())
