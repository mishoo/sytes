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
           #:def-syte-primitive
           #:start-server
           #:stop-server)
  #+sbcl
  (:import-from #:sb-ext
                #:run-program
                #:process-input
                #:process-output
                #:process-exit-code)
  #+ccl
  (:shadow CCL::RUN-PROGRAM))

