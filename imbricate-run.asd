
(asdf:defsystem #:imbricate-run
  :description "cli tool for imbricate"
  :author "colin okay <cbeok@protonmail.com>"
  :license  "AGPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on ( #:asdf #:imbricate )
  :components ((:file "run")))
