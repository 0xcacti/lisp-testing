(asdf:defsystem #:cl-testing
  :description "A JS style testing framework for Common Lisp"
  :author "0xcacti"
  :version "0.1.0"
  :serial t
  :components 
  ((:module "src"
    :components
    ((:file "package")
     (:file "cl-testing")))))


