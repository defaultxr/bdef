(in-package #:bdef)

;;; loading functionality (FIX):

(defmethod bdef-backend-load ((backend (eql :incudine)) (file string) &key (num-channels 2) (wavetable nil) (start-frame 0))
  ;; FIX: num-channels and wavetable don't work yet.
  (when (/= 2 num-channels)
    (error "bdef's buffer loading functionality doesn't yet support the num-channels argument for the Incudine backend."))
  (unless (null wavetable)
    (error "bdef's buffer loading functionality doesn't yet support the wavetable argument for the Incudine backend."))
  (let* ((buffer (incudine:buffer-load file :offset start-frame))
         (bdef (make-instance 'bdef
                              :key file
                              :buffer buffer)))
    (bdef-set file bdef)
    (setf (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-backend-load ((backend (eql :incudine)) (object list) &key)
  )

(defmethod bdef-backend-load ((backend (eql :incudine)) (object incudine:envelope) &key)
  )

;;; generics

;; sadly Incudine's buffer functions aren't generics, so we can't provide our own methods for them like we do with cl-collider.

(defmethod incudine:free ((bdef bdef))
  (bdef-free bdef))

;; FIX: figure out how to get buffer IDs with Incudine?
;; (defmethod id ((buffer incudine:buffer))
;;   (incudine:bufnum buffer))

(defmethod frames ((buffer incudine:buffer))
  (incudine:buffer-frames buffer))

(defmethod num-channels ((buffer incudine:buffer))
  (incudine:buffer-channels buffer))

(defmethod path ((buffer incudine:buffer))
  (incudine:buffer-file buffer))

(defmethod duration ((buffer incudine:buffer))
  (/ (incudine:buffer-frames buffer)
     (incudine:buffer-sample-rate buffer)))

;;; enable it!

(push :incudine *bdef-backends*)
