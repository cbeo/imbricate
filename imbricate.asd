;;;; imbricate.asd

(asdf:defsystem #:imbricate
  :description "Makes tilesheets"
  :author "colin okay <cbeok@protonmail.com>"
  :license  "AGPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:asdf #:uiop #:opticl)
  :components ((:file "imbricate")))
