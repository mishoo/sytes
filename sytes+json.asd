(asdf:defsystem #:sytes+json
  :serial t
  :description "Adds some JSON capability to Sytes"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "BSD"
  :depends-on (#:sytes
               #:local-time
               #:cl-json
               #:cl-ppcre
               #:cl-ppcre-unicode
               #:trivial-backtrace)
  :components ((:file "json")
               (:file "json-validation")))
