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

   #:define-bdef-auto-metadata
   #:set-bdef-auto-metadata
   #:remove-bdef-auto-metadata

   #:splits
   #:make-splits
   #:splits-length
   #:splits-points
   #:splits-point

   #:splits-from-aubio-onsets
   #:splits-from-audacity-labels
   #:audacity-labels-from-splits))
