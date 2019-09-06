;;;; package.lisp

(defpackage #:bdef
  (:use #:cl)
  (:export
   #:*bdef-dictionary*
   #:all-bdefs
   #:bdef
   #:bdef-metadata
   #:bdef-splits
   #:bdef-free))
