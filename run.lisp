
(defpackage #:imbricate.run
  (:use #:cl)
  (:export #:main))

(in-package :imbricate.run)

(defun print-help ()
  (format t "USAGE: imbricate SOURCE-DIR TARGET-DIR SHEET-TITLE~%~%")
  (format t "       SOURCE-DIR a directory containing png files. Non png files are skipped~%")
  (format t "       TARGET-DIR a directory where the tilesheet is to be built.~%")
  (format t "       SHEET-TITLE a name used for this sheet and its meta info~%~%"))

#+ecl
(defun main ()
  (print (ext:command-args)))
  ;; (unless (= 4 (length (ext:command-args)))
  ;;   (print-help)
  ;;   (ext:quit))
  ;; (destructuring-bind (src target title) (cdr (ext:command-args))
  ;;   (imbricate:imbricate-and-save src target title)
  ;;   (ext:quit)))

#+sbcl
(defun main ()
  (unless (= 4 (length sb-ext:*posix-argv*))
    (print-help)
    (uiop:quit))
  (destructuring-bind (src target title) (cdr sb-ext:*posix-argv*)
    (imbricate:imbricate-and-save src target title)
    (uiop:quit)))


