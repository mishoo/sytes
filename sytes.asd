;;;; sytes.asd

(asdf:defsystem #:sytes
  :serial t
  :description "For simple websites"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "BSD"
  :depends-on (#:hunchentoot
               #:anaphora
               #:parse-number
               #:cl-ppcre
               #:cl-unicode
               #:cl-ppcre-unicode
               #:split-sequence
               #:cl-fad
               #:cl-json
               #:bordeaux-threads
               #:trivial-utf-8
               #:closer-mop
               #+sbcl #:sb-daemon)
  :components ((:file "package")
               (:file "utils")
               (:module "template"
                :serial t
                :components ((:file "package")
                             (:file "context")
                             (:file "parser")
                             (:file "compiler")
                             (:file "storage")))
               (:file "sytes")))

