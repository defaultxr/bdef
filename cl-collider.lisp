(in-package #:bdef)


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

