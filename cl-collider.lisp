(in-package #:bdef)

(defmethod bdef-backend-supported-file-types ((backend (eql :cl-collider)))
  (list :wav :aif :aiff))

(defmethod bdef-backend-load ((backend (eql :cl-collider)) (file string) &key (wavetable nil) id server)
  (if wavetable
      (cl-collider:buffer-read-as-wavetable file) ;; FIX: this function should support the bufnum and server arguments
      (apply #'cl-collider:buffer-read file (append (when id
                                                      (list :bufnum id))
                                                    (when server
                                                      (list :server server))))))

;; FIX:
(defmethod bdef-backend-load ((backend (eql :cl-collider)) (object list) &key)
  (error "SuperCollider backend does not yet support loading a list of files.")
  (let* ((buffer (cl-collider:buffer-alloc (length object)))
         (bdef (make-instance 'bdef
                              :key object
                              :buffer buffer)))
    (cl-collider:buffer-setn buffer object)
    (setf (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-backend-load ((backend (eql :cl-collider)) (object cl-collider::env) &key (wavetable t) &allow-other-keys)
  (let* ((wavetable (or wavetable 512)) ;; FIX: if WAVETABLE is t...
         (buffer (cl-collider:buffer-alloc (* 2 wavetable)))
         (bdef (make-instance 'bdef
                              :key object
                              :buffer buffer)))
    (cl-collider:buffer-setn buffer (cl-collider::list-in-wavetable-format (cl-collider::env-as-signal object wavetable)))
    (setf (bdef-metadata bdef :env) object
          (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-buffer ((buffer cl-collider::buffer))
  buffer)

(defmethod bdef-backend-free ((buffer cl-collider::buffer))
  (cl-collider:buffer-free buffer))

(defmethod bdef-load ((buffer cl-collider::buffer) &key) ;; FIX: implement this
  ;; check if the buffer is already assigned to a bdef
  ;; if it is, we can just point to that and don't need to re-run the auto-metadata functions
  (error "Loading an existing buffer to a bdef is not yet implemented."))

(defmethod bdef-load ((env cl-collider::env) &rest rest &key &allow-other-keys)
  (apply #'bdef-backend-load :cl-collider env rest))

(defmethod bdef-length ((buffer cl-collider::buffer))
  (cl-collider:frames buffer))

(defmethod bdef-sample-rate ((buffer cl-collider::buffer))
  (cl-collider:sr buffer))

(defmethod bdef-channels ((buffer cl-collider::buffer))
  (cl-collider:chanls buffer))

(defmethod bdef-id ((buffer cl-collider::buffer))
  (cl-collider:bufnum buffer))

(defmethod bdef-file ((buffer cl-collider::buffer))
  (cl-collider:path buffer))

(defmethod bdef-subseq ((buffer cl-collider::buffer) &optional (start 0) (end (bdef-length buffer)) channel)
  (cl-collider:buffer-to-array buffer start end channel))

;;; cl-collider methods

(defmethod cl-collider:bufnum ((bdef bdef))
  (cl-collider:bufnum (bdef-buffer bdef)))

(defmethod cl-collider::floatfy ((bdef bdef))
  (cl-collider:bufnum (bdef-buffer bdef)))

(defmethod cl-collider:frames ((bdef bdef))
  (frames (bdef-buffer bdef)))

(defmethod cl-collider:chanls ((bdef bdef))
  (num-channels (bdef-buffer bdef)))

(defmethod cl-collider::path ((bdef bdef))
  (path (bdef-buffer bdef)))

(defmethod cl-collider:buffer-dur ((bdef bdef))
  (bdef-duration bdef))

(defmethod cl-collider:sr ((bdef bdef))
  (cl-collider:sr (bdef-buffer bdef)))

(defmethod cl-collider:free ((bdef bdef))
  (bdef-free bdef))

;;; enable it!

(push :cl-collider *bdef-backends*)
