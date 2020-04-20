(in-package #:bdef)

(defun sc-buffer-read-any (path &key (num-channels 2) wavetable (start-frame 0) bufnum (server cl-collider:*s*))
  "Read any file as a buffer, making sure it's converted to a SuperCollider-compatible format first, and making sure it loads as a stereo buffer even if the file is mono."
  (let* ((path (ensure-readable-audio-file (namestring (truename path))))
         (ffmpeg-data (ffmpeg-data path))
         (stream-line-pos (search "Stream #0:0: Audio: " ffmpeg-data))
         (newline (position #\newline ffmpeg-data :start stream-line-pos))
         (mono-p (search "mono," (subseq ffmpeg-data stream-line-pos newline))))
    (when (/= 0 start-frame)
      (error "Loading sounds with a start frame is not yet supported.")) ;; FIX
    (if wavetable
        (cl-collider:buffer-read-as-wavetable path)
        (cl-collider:buffer-read-channel path
                                         :channels (cond ((null num-channels)
                                                          -1)
                                                         ((= num-channels 1)
                                                          (if mono-p
                                                              (list 0)
                                                              (progn
                                                                (warn "The requested sound (\"~s\") is stereo but a 1-channel buffer was requested. Mixing stereo files down to mono is not yet supported; therefore only the first channel was loaded." path)
                                                                (list 0))))
                                                         ((= num-channels 2)
                                                          (if mono-p
                                                              (list 0 0)
                                                              (list 0 1))))
                                         :bufnum bufnum
                                         :server server))))

(defmethod bdef-backend-load ((backend (eql :cl-collider)) (file string) &key (num-channels 2) (wavetable nil) (start-frame 0))
  (let* ((buffer (sc-buffer-read-any file :num-channels num-channels :wavetable wavetable :start-frame start-frame))
         (bdef (make-instance 'bdef
                              :key file
                              :buffer buffer)))
    (bdef-set file bdef)
    (setf (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-backend-load ((backend (eql :cl-collider)) (object list) &key)
  (let* ((buffer (cl-collider:buffer-alloc (length object)))
         (bdef (make-instance 'bdef
                              :key object
                              :buffer buffer)))
    (cl-collider:buffer-setn buffer object)
    (setf (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-backend-load ((backend (eql :cl-collider)) (object cl-collider::env) &key (num-channels 1) (wavetable t))
  (let* ((wavetable (or wavetable 512)) ;; FIX: if WAVETABLE is t...
         (buffer (cl-collider:buffer-alloc (* 2 wavetable) :chanls num-channels))
         (bdef (make-instance 'bdef
                              :key object
                              :buffer buffer)))
    (cl-collider:buffer-setn buffer (cl-collider::list-in-wavetable-format (cl-collider::env-as-signal object wavetable)))
    (setf (bdef-metadata bdef :env) object
          (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

(defmethod bdef-free-buffer ((buffer cl-collider::buffer))
  (cl-collider:buffer-free buffer))

(defmethod bdef-length ((buffer cl-collider::buffer))
  (cl-collider:frames buffer))

(defmethod bdef-subseq ((buffer cl-collider::buffer) start &optional end channel) ;; FIX: this doesn't get all channels (this needs to be fixed in cl-collider)
  ;; FIX: implement CHANNEL
  (let ((end (or end (frames buffer))))
    (coerce (cl-collider:buffer-get-to-list buffer start (- end start)) 'vector)))

;;; generics

(defmethod cl-collider:bufnum ((bdef bdef))
  (id (bdef-buffer bdef)))

(defmethod cl-collider::floatfy ((bdef bdef))
  (id (bdef-buffer bdef)))

(defmethod cl-collider:frames ((bdef bdef))
  (frames (bdef-buffer bdef)))

(defmethod cl-collider:chanls ((bdef bdef))
  (num-channels (bdef-buffer bdef)))

(defmethod cl-collider::path ((bdef bdef))
  (path (bdef-buffer bdef)))

(defmethod cl-collider:buffer-dur ((bdef bdef))
  (duration (bdef-buffer bdef)))

(defmethod cl-collider:sr ((bdef bdef))
  (cl-collider:sr (bdef-buffer bdef)))

(defmethod cl-collider:free ((bdef bdef))
  (bdef-free bdef))

(defmethod id ((buffer cl-collider::buffer))
  (cl-collider:bufnum buffer))

(defmethod frames ((buffer cl-collider::buffer))
  (cl-collider:frames buffer))

(defmethod num-channels ((buffer cl-collider::buffer))
  (cl-collider:chanls buffer))

(defmethod path ((buffer cl-collider::buffer))
  (cl-collider::path buffer))

(defmethod duration ((buffer cl-collider::buffer))
  (cl-collider:buffer-dur buffer))

(defmethod sample-rate ((buffer cl-collider::buffer))
  (cl-collider:sr buffer))

;;; enable it!

(push :cl-collider *bdef-backends*)
