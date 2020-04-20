;;;; bdef.asd

(asdf:defsystem #:bdef
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems"
  :author "modula t. <defaultxr at gmail dot com>"
  :license "MIT"
  :version "0.6"
  :serial t
  :depends-on (#:alexandria
               #:parse-float
               #:split-sequence
               #:eager-future2)
  :components ((:file "package")
               (:file "bdef")
               (:file "splits")
               #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or))
               (:file "sequence-extensions"))
  :in-order-to ((test-op (test-op "bdef/tests"))))

(asdf:defsystem #:bdef/cl-patterns
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with cl-patterns functionality)."
  :author "modula t. <defaultxr at gmail dot com>"
  :license "MIT"
  :version "0.6"
  :serial t
  :depends-on (#:bdef
               #:cl-patterns)
  :components ((:file "cl-patterns")))

(asdf:defsystem #:bdef/cl-collider
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with cl-collider functionality)."
  :author "modula t. <defaultxr at gmail dot com>"
  :license "MIT"
  :version "0.6"
  :serial t
  :depends-on (#:bdef
               #:cl-collider)
  :components ((:file "cl-collider")))

(asdf:defsystem #:bdef/incudine
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with Incudine functionality)."
  :author "modula t. <defaultxr at gmail dot com>"
  :license "MIT"
  :version "0.6"
  :serial t
  :depends-on (#:bdef
               #:incudine)
  :components ((:file "incudine")))

(asdf:defsystem #:bdef/tests
  :name "bdef/tests"
  :author "modula t. <defaultxr@gmail.com>"
  :description "FiveAM-based tests suite for bdef."
  :license "MIT"
  :depends-on (#:bdef
               #:mutility
               #:fiveam)
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:bdef-tests
                                                         :bdef/tests))))
