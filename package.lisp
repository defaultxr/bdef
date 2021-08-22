;;;; package.lisp

(uiop:define-package #:bdef
  (:use #:cl
        #:alexandria
        #:mutility)
  (:export

   ;;; bdef.lisp
   
   #:*bdef-temporary-directory*
   #:*bdef-backends*

   #:bdef
   #:bdef-p
   #:all-bdefs
   #:find-bdef
   #:bdef-free

   #:bdef-key
   #:bdef-id
   #:bdef-buffer

   #:bdef-length
   #:bdef-sample-rate
   #:bdef-channels
   #:bdef-file

   #:bdef-duration
   #:bdef-tempo
   #:bdef-dur
   #:bdef-splits

   #:bdef-frames
   #:bdef-subseq

   #:bdef-frame
   #:bdef-elt

   #:bdef-metadata
   #:bdef-metadata-keys

   #:define-bdef-auto-metadata
   #:set-bdef-auto-metadata
   #:remove-bdef-auto-metadata

   #:define-bdef-dynamic-metadata
   #:remove-bdef-dynamic-metadata

   ;;; splits.lisp

   #:splits
   #:splits-p
   #:make-splits
   #:splits-length
   #:splits-points
   #:splits-point
   #:splits-export

   #:splits-from-aubio
   #:splits-from-aubio-onsets

   #:splits-from-audacity-labels
   #:audacity-labels-from-splits

   #:splits-from-op-1-drumset))
