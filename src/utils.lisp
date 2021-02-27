(defpackage #:40ants-ci/utils
  (:use #:cl)
  (:export
   #:to-json
   #:ensure-primary-system
   #:system-packages))
(in-package 40ants-ci/utils)


(defvar *jq-available* 'not-checked)


(defun jq-available-p ()
  (when (eql *jq-available* 'not-checked)
    (setf *jq-available*
          ;; zero status code means the utility is available
          (zerop
           (nth-value
            2
            (uiop:run-program "which jq"
                              :ignore-error-status t)))))
  *jq-available*)


(defun to-json (data)
  (let ((json (jonathan:to-json data
                                :from :alist)))
    (if (jq-available-p)
        (with-output-to-string (jq-stdout)
          (with-input-from-string (jq-stdin json)
            (uiop:run-program "jq" :input jq-stdin
                                   :output jq-stdout)))
        json)))


(defun ensure-primary-system (system)
  (check-type system asdf:system)
  (asdf:find-system
   (asdf:primary-system-name system)))


(defgeneric system-packages (system)
  (:documentation "Returns a list of packages created by ASDF system.

Default implementation returns a package having the same name as a system
and all packages matched to package-inferred subsystems:

```
CL-USER> (docs-builder/utils:system-packages :docs-builder)
(#<PACKAGE \"DOCS-BUILDER\">
 #<PACKAGE \"DOCS-BUILDER/UTILS\">
 #<PACKAGE \"DOCS-BUILDER/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/GENEVA/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDER\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER\">
 #<PACKAGE \"DOCS-BUILDER/DOCS\">
 #<PACKAGE \"DOCS-BUILDER/BUILDERS/MGL-PAX/BUILDER\">)
```

This function can be used by builder to find pieces of documentation.
For example, DOCS-BUILDER/BUILDERS/MGL-PAX/GUESSER:@INDEX
builder uses it to find documentation sections.
")
  (:method ((system string))
    (system-packages (asdf:find-system system)))
  (:method ((system symbol))
    (system-packages (asdf:find-system system)))
  (:method ((system asdf:system))

    (let* ((package-name (string-upcase
                          (asdf:component-name system))))
      (append (list (find-package package-name))
              (loop with prefix-name = (format nil "~A/" package-name)
                    with prefix-name-length = (length prefix-name)
                    for package in (list-all-packages)
                    for package-name = (package-name package)
                    when (and (> (length package-name)
                                 prefix-name-length)
                              (string= (subseq package-name
                                               0 prefix-name-length)
                                       prefix-name))
                      collect package)))))
