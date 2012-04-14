;;;; package.lisp

(defpackage #:sytes
  (:use #:cl #:split-sequence)
  (:export #:syte
           #:syte-request-handler
           #:register-syte
           #:unregister-syte
           #:syte-names
           #:syte-root
           #:syte-context))

