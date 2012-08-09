(defpackage #:sytes.template
  (:use #:cl #:anaphora)
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
           #:clear-cache
           #:tops
           #:make-keyval
           #:defglobal-context
           #:fetch-property
           #:define-fetcher

           #:*current-template*
           #:*request-template*))

(defpackage #:sytes.%runtime%
  (:import-from :cl #:t #:nil))

(in-package #:sytes.template)
