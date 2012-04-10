;;;; sytes.asd

(asdf:defsystem #:sytes
  :serial t
  :description "For simple websites"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "BSD"
  :depends-on (#:hunchentoot
               #:anaphora
               #:iterate
               #:parse-number
               #:cl-unicode)
  :components ((:file "package")
               (:file "sytes")
               (:module "template"
                        :serial t
                        :components ((:file "package")
                                     (:file "parser")
                                     (:file "compiler")))))

