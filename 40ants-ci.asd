#-asdf3.1 (error "40ants-ci requires ASDF 3.1")
(defsystem "40ants-ci"
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("40ants-ci/core"
               "40ants-ci/ci"
               "40ants-ci/changelog")
  :description "A helper to an answer a question about OS, Lisp and Everything."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain")))
  :in-order-to ((test-op (test-op 40ants-ci-test))))

