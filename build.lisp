

(ql:register-local-projects)

(dolist (dir ql:*local-project-directories*)
  (push dir asdf:*central-registry*))

(ql:quickload :imbricate-run)

#+sbcl
(progn
  (sb-ext:save-lisp-and-die #p"imbricate" :toplevel #'imbricate.run:main :executable t :compression t))

#+ecl
(progn 
  (asdf:make-build :imbricate-run
                   :type :program
                   :move-here #P"./"
                   :epilogue-code '(imbricate.run:main))
  (ext:exit))

