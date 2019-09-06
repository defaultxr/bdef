(in-package #:bdef)

(defmethod cl-collider:sr ((bdef bdef))
  (cl-collider:sr (bdef-buffer bdef)))

(defmethod cl-collider:bufnum ((bdef bdef))
  (cl-collider:bufnum (bdef-buffer bdef)))

(defmethod cl-collider:chanls ((bdef bdef))
  (cl-collider:chanls (bdef-buffer bdef)))

(defmethod cl-collider::floatfy ((bdef bdef))
  (cl-collider:bufnum (bdef-buffer bdef)))

(defmethod cl-collider:frames ((bdef bdef))
  (frames (bdef-buffer bdef)))

;; FIX: cl-collider:free is a regular function, not a generic one
;; (defmethod sc:free ((bdef bdef))
;;   (bdef-free bdef))

(defmethod duration ((buffer cl-collider::buffer))
  (cl-collider:buffer-dur buffer))

(defmethod frames ((buffer cl-collider::buffer))
  (cl-collider:frames buffer))

(defmethod num-channels ((buffer cl-collider::buffer))
  (cl-collider:chanls buffer))

(defmethod path ((buffer cl-collider::buffer))
  (slot-value buffer 'cl-collider::path))
