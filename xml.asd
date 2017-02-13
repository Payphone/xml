;;;; rss.asd

(asdf:defsystem #:xml
  :description "XML parsing utilities"
  :author "Peyton Farrar <peyton@peytonfarar.com>"
  :license "MIT"
  :serial t
  :depends-on (#:peyton-utils)
  :components ((:file "package")
               (:file "xml")))
