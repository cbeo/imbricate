;;;; imbricate.asd

(asdf:defsystem #:imbricate
  :description "Describe imbricate here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:jonathan #:opticl #:uiop #:defclass-std #:lambda-tools)
  :components ((:file "imbricate")))
