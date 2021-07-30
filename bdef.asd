;;;; bdef.asd

(asdf:defsystem #:bdef
  :name "bdef"
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems"
  :author "modula t."
  :license "MIT"
  :version "0.8"
  :homepage "https://github.com/defaultxr/bdef"
  :bug-tracker "https://github.com/defaultxr/bdef/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/bdef.git")
  :serial t
  :depends-on (#:alexandria
               #:mutility
               #:parse-float
               #:jsown
               #:eager-future2)
  :components ((:file "package")
               (:file "bdef")
               (:file "splits")
               #+#.(cl:if (cl:find-package "SEQUENCE") '(:and) '(:or))
               (:file "sequence-extensions"))
  :in-order-to ((test-op (test-op "bdef/tests"))))

(asdf:defsystem #:bdef/cl-patterns
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with cl-patterns functionality)"
  :author "modula t."
  :license "MIT"
  :version "0.8"
  :serial t
  :depends-on (#:bdef
               #:cl-patterns)
  :components ((:file "cl-patterns")))

(asdf:defsystem #:bdef/cl-collider
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with cl-collider functionality)"
  :author "modula t."
  :license "MIT"
  :version "0.8"
  :serial t
  :depends-on (#:bdef
               #:cl-collider)
  :components ((:file "cl-collider")))

(asdf:defsystem #:bdef/incudine
  :description "Buffer definition; audio buffer abstraction for sound synthesis systems (with Incudine functionality)"
  :author "modula t."
  :license "MIT"
  :version "0.8"
  :serial t
  :depends-on (#:bdef
               #:incudine)
  :components ((:file "incudine")))

(asdf:defsystem #:bdef/tests
  :name "bdef/tests"
  :author "modula t."
  :description "FiveAM-based tests suite for bdef"
  :license "MIT"
  :depends-on (#:bdef
               #:bdef/cl-patterns
               #:mutility
               #:mutility/test-helpers
               #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "bdef")
               (:file "splits")
               (:file "cl-patterns"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:bdef-tests
                                                         :bdef/tests))))
