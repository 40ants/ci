(defpackage #:40ants-ci/github
  (:use #:cl)
  (:import-from #:40ants-ci/utils
                #:make-github-workflows-path
                #:system-packages
                #:ensure-primary-system)
  (:import-from #:40ants-ci/vars
                #:*current-system*)
  (:export
   #:generate
   #:prepare-data
   #:*current-system*))
(in-package 40ants-ci/github)


(defgeneric generate (obj path))


(defmethod generate ((symbol symbol) path)
  (generate (ensure-primary-system
             (asdf:find-system symbol))
            path))


(defmethod generate ((system asdf:system) path)
  ;; For system, we try to find all system packages
  ;; and execute generate for each of them: 
  (let* ((*current-system* system)
         (path (or path
                   (make-github-workflows-path system))))
    (loop for package in (system-packages system)
          appending (generate package
                              path))))


(defgeneric prepare-data (obj))

