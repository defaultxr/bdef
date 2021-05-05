(in-package #:bdef)

;;; loading functionality (FIX):

(defmethod bdef-backend-supported-file-types ((backend (eql :incudine)))
  (list :wav :aif :aiff))

(defmethod bdef-backend-load ((backend (eql :incudine)) (file string) &key (wavetable nil) (start-frame 0) &allow-other-keys)
  ;; FIX: wavetable doesn't work yet.
  (unless (null wavetable)
    (error "bdef's buffer loading functionality doesn't yet support the wavetable argument for the Incudine backend."))
  (incudine:buffer-load file :offset start-frame))

;; FIX:
;; (defmethod bdef-backend-load ((backend (eql :incudine)) (object list) &key)
;;   )

;; FIX:
;; (defmethod bdef-backend-load ((backend (eql :incudine)) (object incudine:envelope) &key)
;;   )

(defmethod bdef-backend-free ((buffer incudine:buffer))
  (incudine:free buffer))

(defmethod bdef-length ((buffer incudine:buffer))
  (incudine:buffer-frames buffer))

(defmethod bdef-sample-rate ((buffer incudine:buffer))
  (incudine:buffer-sample-rate buffer))

(defmethod bdef-channels ((buffer incudine:buffer))
  (incudine:buffer-channels buffer))

;; FIX: is there some way to get the buffer ID from incudine? does incudine even use buffer IDs?
;; (defmethod bdef-id ((buffer incudine:buffer))
;;   )

(defmethod bdef-file ((buffer incudine:buffer))
  (incudine:buffer-file buffer))

(defmethod bdef-subseq ((buffer incudine:buffer) &optional start end channel)
  ;; FIX: implement channel argument
  (when channel
    (warn "bdef-subseq's CHANNEL argument is not yet implemented for the Incudine backend"))
  (subseq (incudine:buffer->array buffer) start end))

;;; generics

;; it looks like most of Incudine's buffer functions aren't generics, so we can't provide our own methods for them like we do with cl-collider.

(defmethod incudine:free ((bdef bdef))
  (bdef-free bdef))

;;; enable it!

(push :incudine *bdef-backends*)
