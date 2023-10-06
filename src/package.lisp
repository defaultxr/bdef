;;;; package.lisp

(uiop:define-package #:bdef
  (:use #:cl
        #:alexandria
        #:mutility)
  (:shadow #:file-exists-p)
  (:export

   ;;; bdef.lisp
   
   #:*bdef-default-num-channels*
   #:*bdef-temporary-directory*
   #:*bdef-backends*

   #:bdef
   #:bdef-p
   #:all-bdefs
   #:all-bdef-names
   #:find-bdef
   #:bdef-free

   #:bdef-name
   #:bdef-key
   #:bdef-id
   #:bdef-names
   #:bdef-buffer

   #:bdef-length
   #:bdef-sample-rate
   #:bdef-channels
   #:bdef-file

   #:bdef-duration
   #:bdef-tempo
   #:bdef-dur
   #:bdef-splits
   #:bdef-splits-events

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
