;;;; package.lisp

(defpackage #:bdef
  (:use #:cl
        #:alexandria)
  (:export
   #:*bdef-temporary-directory*
   #:*bdef-backends*
   #:*bdef-dictionary*
   #:all-bdefs
   #:bdef
   #:bdef-buffer
   #:bdef-metadata
   #:bdef-metadata-keys
   #:bdef-splits
   #:bdef-free
   #:bdef-length
   #:bdef-elt
   #:bdef-subseq

   #:define-bdef-auto-metadata
   #:set-bdef-auto-metadata
   #:remove-bdef-auto-metadata

   #:define-bdef-dynamic-metadata
   #:remove-bdef-dynamic-metadata

   #:splits
   #:make-splits
   #:splits-length
   #:splits-points
   #:splits-point

   #:splits-from-aubio-onsets
   #:splits-from-audacity-labels
   #:audacity-labels-from-splits))
