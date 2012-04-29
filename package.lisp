;;;; package.lisp

(defpackage #:sytes
  (:use #:cl #:split-sequence #:anaphora)
  (:export #:syte
           #:syte-request-handler
           #:register-syte
           #:unregister-syte
           #:syte-names
           #:syte-root
           #:syte-context
           #:def-syte-primitive))

