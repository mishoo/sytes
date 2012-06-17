(defpackage #:sytes.template
  (:use #:cl #:iterate #:anaphora)
  (:shadow #:compile #:compile-file)
  (:nicknames #:tmpl)
  (:export #:make-context
           #:compile-file
           #:exec-template
           #:exec-template-request
           #:def-primitive
           #:template-context
           #:template-filename
           #:template-function
           #:template-timestamp
           #:my-symbol-p
           #:my-symbol-name
           #:clear-cache

           #:*current-template*))

(in-package #:sytes.template)
