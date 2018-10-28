(asdf:defsystem #:sytes+git
  :serial t
  :description "Static file revisions for Sytes"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "BSD"
  :depends-on (#:sytes)
  :components ((:file "git")))
