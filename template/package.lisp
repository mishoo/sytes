(defpackage #:sytes.template
  (:use #:cl #:iterate #:anaphora)
  (:shadow #:compile #:compile-file)
  (:nicknames #:tmpl)
  (:export #:make-context
           #:compile-file
           #:exec-template
           #:def-primitive))

(in-package #:sytes.template)
