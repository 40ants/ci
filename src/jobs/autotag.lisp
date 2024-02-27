(uiop:define-package #:40ants-ci/jobs/autotag
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/job)
  (:import-from #:40ants-ci/steps/action
                #:action)
  (:export #:autotag
           #:filename
           #:regex
           #:tag-prefix
           #:token-pattern))
(in-package 40ants-ci/jobs/autotag)

(defparameter *default-filename* "ChangeLog.md")

(defparameter *default-regex* "^## (?<version>\\d+\\.\\d+\\.\\d+.*?)( |\\n).*$")

(defparameter *default-tag-prefix* "v")

(defparameter *default-token-pattern* "${{ secrets.GITHUB_TOKEN }}")


(defclass autotag (40ants-ci/jobs/job:job)
  ((filename :initarg :filename
             :initform *default-filename*
             :type string
             :documentation "File where to search for version numbers."
             :reader filename)
   (regex :initarg :regex
          :initform *default-regex*
          :type string
          :documentation "Regexp used to extract version numbers."
          :reader regex)
   (tag-prefix :initarg :tag-prefix
               :initform *default-tag-prefix*
               :type string
               :documentation "Tag prefix."
               :reader tag-prefix)
   (token-pattern :initarg :token-pattern
                  :initform *default-token-pattern*
                  :type string
                  :documentation "Auth token pattern."
                  :reader token-pattern))
  (:default-initargs
   :permissions '(:contents "write"))
  (:documentation "This type of the job created a git tag when finds a new tag in specified file."))


(defun autotag (&key (filename *default-filename*)
                     (regex *default-regex*)
                     (tag-prefix *default-tag-prefix*)
                     (token-pattern *default-token-pattern*))
  "Creates a job which will run autotagger to create a new git tag for release."
  (make-instance 'autotag
                 :filename filename
                 :regex regex
                 :tag-prefix tag-prefix
                 :token-pattern token-pattern))


(defmethod 40ants-ci/jobs/job:steps ((job autotag))
  (append (list
           (action "Checkout Code"
                   "actions/checkout@v4"))
          (list
           (action "Create release tag"
                   "butlerlogic/action-autotag@8bc1ad456dcdee34e8c6ffbce991cc31793578c2"
                   :root (filename job)
                   :regex_pattern (regex job)
                   :tag_prefix (tag-prefix job)
                   :env (list :github_token
                              (token-pattern job))))
          (call-next-method)))
