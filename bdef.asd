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
               (:file "splits")))

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
