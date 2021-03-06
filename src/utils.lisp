(defpackage #:40ants-ci/utils
  (:use #:cl)
  (:import-from #:40ants-ci/vars)
  (:import-from #:str
                #:join
                #:split)
  (:import-from #:yason)
  (:export
   #:to-json
   #:ensure-primary-system
   #:system-packages
   #:current-system-name
   #:dedent
   #:single
   #:plistp
   #:alistp
   #:plist-to-alist
   #:ensure-list-of-plists
   #:make-github-workflows-path))
(in-package 40ants-ci/utils)


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


(defun current-system-name ()
  (asdf:component-name
   40ants-ci/vars:*current-system*))


(defun count-leading-spaces (line)
  (loop for char across line
        while (char-equal char #\Space)
        summing 1))

(defun empty-line (line)
  (loop for char across line
        always (char-equal char #\Space)))


(defun dedent (text)
  "Removes common leading whitespace from each string.

A few examples:

```
(dedent \"Hello
          World
          and all Lispers!\")

\"Hello
World
and all Lispers!\"
```

```
(dedent \"
    Hello
    World
    and all Lispers!\")

\"Hello
World
and all Lispers!\"
```

```
(dedent \"This is a code:

              (symbol-name :hello-world)

          it will output HELLO-WORLD.\")

\"This is a code:

    (symbol-name :hello-world)

it will output HELLO-WORLD.\"
```


"
  (let* ((lines (split #\Newline text))
         (common-ident
           (loop for line in lines
                 for line-idx upfrom 0
                 for leading-spaces = (count-leading-spaces line)
                 unless (or (empty-line line)
                            ;; If first line has no leading spaces
                            ;; spaces we should treat it specially
                            (and (zerop line-idx)
                                 (zerop leading-spaces)))
                   minimizing leading-spaces))
         (trimmed-lines
           (loop for line in lines
                 for line-idx upfrom 0
                 for ident = (if (and (zerop line-idx)
                                      (zerop (count-leading-spaces line)))
                                 0
                                 common-ident)
                 collect
                 (subseq line
                         (min ident
                              (length line))
                         (length line)))))
    (join #\Newline
          trimmed-lines)))


(defun single (list)
  "Test wheather LIST contains exactly 1 element."
  (and (consp list) (not (cdr list))))


(defun plistp (list)
  "Test wheather LIST is a properly formed plist."
  (when (listp list)
    (loop for rest on list by #'cddr
          unless (and (keywordp (car rest))
                      (cdr rest))
            do (return nil)
          finally (return list))))


(defun alistp (list)
  "Test wheather LIST is a properly formed alist.

   In this library, ALIST has always a string as a key.
   Because we need them to have this form to serialize
   to JSON propertly.

   (alistp '((\"cron\" . \"0 10 * * 1\"))) -> T
   (alistp '(((\"cron\" . \"0 10 * * 1\")))) -> NIL
"
  (and (listp list)
       (loop for item in list
             always (and (consp item)
                         (typep (car item)
                                'string)))))


(defun ensure-list-of-plists (data)
  (cond
    ((plistp data)
     (list data))
    ((and (listp data)
          (every #'plistp data))
     data)
    (t
     (error "~A is not a plist a list of plists"
            data))))


(defun plist-to-alist (plist &key (string-keys t)
                               (lowercase t))
  "Make an alist from a plist PLIST.

   By default, transforms keys to lowercased strings"
  (flet ((transform-key (key)
           (let* ((result (if string-keys
                              (symbol-name key)
                              key))
                  (result (if lowercase
                              (string-downcase key)
                              result)))
             result)))
    (loop for (key value) on plist by #'cddr
          collect (cons (transform-key key)
                        value))))


(defun make-github-workflows-path (system)
  (asdf:system-relative-pathname
   system
   (make-pathname :directory
                  '(:relative
                    ".github"
                    "workflows"))))


(defun list-to-json (obj stream)
  (cond
    ((alistp obj)
     (yason:encode-alist obj stream))
    (t
     (yason:encode-plain-list-to-array obj stream))))


(defun to-json (data)
  ;; I've tried all JSON libraries (with permissive licenses)
  ;; from this list:
  ;; https://sabracrolleton.github.io/json-review
  ;; and only YASON is able to produce indented results.
  ;; However it requires a few hacks to make it work with
  ;; nested alists.
  ;; In the first version of this library, I've generated
  ;; JSON with Jonathan and indented it using JQ command-line
  ;; utility if it is available.
  (yason:with-output-to-string* (:stream-symbol s :indent 2)
    (let ((yason:*list-encoder* #'list-to-json)
          (yason:*parse-json-booleans-as-symbols* t))
      (yason:encode data))))


;; I don't like these hacks :(
(defmethod yason:encode ((object (eql :false)) &optional (stream yason::*json-output*))
  (write-string "false" stream))


(defmethod yason:encode ((object (eql :true)) &optional (stream yason::*json-output*))
  (write-string "true" stream))


(defmethod yason:encode ((object (eql :null)) &optional (stream yason::*json-output*))
  (write-string "null" stream))


(defmethod yason:encode ((object (eql nil)) &optional (stream yason::*json-output*))
  (write-string "[]" stream))
