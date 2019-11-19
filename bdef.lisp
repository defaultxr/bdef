;;;; bdef.lisp
;; FIX: use uiop:with-temporary-file to generate temporary files?
;; FIX: allow a sound to be loaded as a wavetable and a normal sound at the same time

(in-package #:bdef)

;;; global variables/configuration

(defparameter *bdef-temporary-directory*
  #+linux "/tmp/supercollider/"
  #+darwin "/tmp/supercollider/"
  #+windows (concatenate 'string (uiop:getenv-pathname "USERPROFILE") "/AppData/Local/")
  "The directory bdef should store its temporary files in (i.e. the .wav files generated from format auto-conversion).")

(defparameter *ffmpeg-path*
  #+(or linux darwin) (ignore-errors (uiop:run-program "which ffmpeg" :output (list :string :stripped t)))
  #+windows (uiop:getenv-pathname "TEMP")
  "The path to ffmpeg, or nil if ffmpeg could not be found.")

;;; file handling

(defun ensure-readable-audio-file (path &key (extensions (list "wav" "aif" "aiff")))
  "If PATH ends in any of EXTENSIONS, return it unchanged. Otherwise, use ffmpeg to convert it to the first format in EXTENSIONS. The converted file is stored in `*bdef-temporary-directory*'."
  (let* ((path (namestring (truename path)))
         (ext-pos (position #\. path :from-end t)))
    (if (position (string-downcase (subseq path (1+ ext-pos))) extensions :test #'equal)
        path
        (let ((output-filename (concatenate 'string *bdef-temporary-directory* (file-namestring (subseq path 0 ext-pos)) "." (car extensions))))
          (if (probe-file output-filename)
              output-filename
              (progn
                (ensure-directories-exist *bdef-temporary-directory*)
                (uiop:run-program (list *ffmpeg-path* "-i" path output-filename) ;; FIX: cache this output and parse it for bdef metadata
                                  ;; :output '(:string :stripped t))
                                  )
                output-filename))))))

;; FIX: consider using easy-audio to get audio file metadata instead?
(defun ffmpeg-data (file)
  "Get the ffmpeg output for FILE."
  (nth-value 1 (uiop:run-program (list *ffmpeg-path* "-i" (namestring (truename file))) :error-output '(:string :stripped t) :ignore-error-status t)))

(defun ffmpeg-metadata (file)
  "Get the metadata for a file from ffmpeg's output."
  (alexandria:when-let* ((split (split-sequence:split-sequence #\newline (ffmpeg-data file)))
                         (metadata-pos (position "  Metadata:" split :test 'string=))
                         (metadata-end (position-if (lambda (s) (eq 0 (search "  Duration: " s))) split))
                         (metadata (subseq split (1+ metadata-pos) metadata-end)))
    (loop :for i :in metadata
       :for pos = (position #\: i)
       :for key = (subseq i 0 pos)
       :for value = (subseq i (1+ pos))
       :append (list (intern (string-upcase (string-trim (list #\space) key)) :keyword)
                     (string-trim (list #\space) value)))))

(defun buffer-read-any (path &key (num-channels 2) wavetable (start-frame 0) bufnum (server sc:*s*))
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

;;; bdef

(defclass bdef ()
  ((key :initarg :key :reader bdef-key :type symbol :documentation "The name given to the bdef.")
   (buffer :initarg :buffer :reader bdef-buffer :documentation "The actual buffer object that the bdef refers to.")
   (metadata :initarg :metadata :initform (make-hash-table) :type hash-table :documentation "Hash table of additional data associated with the bdef, accessible with the `bdef-metadata' function.")))

(defmethod print-object ((bdef bdef) stream)
  (with-slots (key buffer) bdef
    (if (slot-boundp bdef 'key)
        (format stream "(~s ~s)" 'bdef key)
        (format stream "#<~s>" 'bdef))))

(defmethod describe-object ((bdef bdef) stream)
  (with-slots (key buffer metadata) bdef
    (format stream "~s is a ~s.~%  It is ~s seconds long (~s frames), with ~s channels.~%" bdef 'bdef (duration bdef) (frames bdef) (num-channels bdef))
    (format stream "~@[  It contains the audio from the file ~s.~%~]" (path bdef))
    (format stream "  Keys that point to this buffer are: ~s~%" (bdef-keys-pointing-to bdef))
    (alexandria:when-let ((meta-keys (bdef-metadata-keys bdef)))
      (format stream "  It has the following metadata:~%")
      (loop :for key :in meta-keys
         :do (format stream "    ~s -> ~s~%" key (bdef-metadata bdef key)))))) ;; FIX: use format's indentation directive?

(defvar *bdef-dictionary* (make-hash-table :test #'equal)
  "The global dictionary of bdefs.")

(defun all-bdefs (&optional include-aliases)
  "Return a list of the names of all bdefs loaded.

Note that this doesn't include aliases (i.e. bdef keys that point to another key) unless INCLUDE-ALIASES is true."
  (if include-aliases
      (alexandria:hash-table-keys *bdef-dictionary*)
      (loop :for i :being :the hash-keys :of *bdef-dictionary*
         :if (typep (gethash i *bdef-dictionary*) 'bdef)
         :collect i)))

(defun bdef-dictionary-keys (&key (include-redirects t) (dictionary *bdef-dictionary*))
  "Return a list of all the keys for the bdef dictionary DICTIONARY."
  (let ((keys (alexandria:hash-table-keys dictionary)))
    (if include-redirects
        keys
        (loop :for key :in keys
           :if (typep (gethash key dictionary) 'bdef)
           :collect key))))

(defun bdef-key-cleanse (key)
  "Expands pathnames to their full ones."
  (typecase key
    (pathname (namestring (truename key)))
    (string (bdef-key-cleanse (pathname key)))
    (otherwise key)))

(defun ensure-bdef (object) ;; FIX: use this in all bdef functions
  "Return OBJECT if object is a bdef, otherwise look up a bdef with OBJECT as its key."
  (if (typep object 'bdef)
      object
      (bdef object)))

(defun bdef-get (key &optional (dictionary *bdef-dictionary*))
  "Get the value of KEY in the bdef dictionary DICTIONARY."
  (let* ((key (bdef-key-cleanse key))
         (result (gethash key dictionary)))
    (if (and (not (null result))
             (typep result '(or string symbol)))
        (bdef-get result dictionary)
        result)))

(defun bdef-metadata (bdef key)
  "Get the value of BDEF's metadata for KEY. Returns true as a second value if the metadata had an entry for KEY, or false if it did not.

Note that this function will block if the specified metadata is one of the `*auto-metadata*' that hasn't finished being generated yet."
  (let ((bdef (ensure-bdef bdef)))
    (multiple-value-bind (val present-p) (gethash key (slot-value bdef 'metadata))
      (values
       (if val
           (if (typep val 'eager-future2:future)
               (setf (bdef-metadata bdef key) (eager-future2:yield val))
               val)
           val)
       present-p))))

(defun (setf bdef-metadata) (value bdef key)
  ;; if VALUE is a splits object and its `splits-bdef' is nil, set it to point to this bdef.
  (let ((bdef (ensure-bdef bdef)))
    (alexandria:when-let ((type-sym (ignore-errors (find-symbol "SPLITS" 'bdef))))
      (when (and (typep value type-sym)
                 (null (funcall 'splits-bdef value)))
        (setf (splits-bdef value) bdef)))
    (setf (gethash key (slot-value bdef 'metadata)) value)))

(defun bdef-metadata-keys (bdef)
  "Get a list of all keys in BDEF's metadata."
  (alexandria:hash-table-keys (slot-value (ensure-bdef bdef) 'metadata)))

(defun bdef-splits (bdef)
  "Get any `splits' from BDEF's metadata, searching in preferred order (i.e. :splits key first, etc)."
  (let ((bdef (ensure-bdef bdef)))
    (loop :for key :in (list :splits :onsets) ;; FIX: check other keys too?
       :for val = (bdef-metadata bdef key)
       :if val
       :return val)))

(defun (setf bdef-splits) (splits bdef)
  (setf (bdef-metadata (ensure-bdef bdef) :splits) splits))

(defun bdef-keys-pointing-to (bdef &optional (dictionary *bdef-dictionary*))
  "Get a list of all the keys in `*bdef-dictionary*' that point to this bdef."
  (alexandria:when-let ((bdef (ensure-bdef bdef)))
    (loop :for key :being :the hash-keys :of dictionary
       :using (hash-value value)
       :if (eq value bdef)
       :collect key)))

(defmethod bdef-buffer ((symbol symbol))
  (bdef-buffer (ensure-bdef symbol)))

(defun bdef-free (bdef &optional (dictionary *bdef-dictionary*))
  "Free a buffer from the bdef dictionary, removing all keys that point to it."
  (etypecase bdef
    (symbol (bdef-free (bdef bdef)))
    (string (bdef-free (bdef bdef)))
    (bdef
     (cl-collider:buffer-free (bdef-buffer bdef))
     (let ((keys (bdef-keys-pointing-to bdef dictionary)))
       (dolist (key keys)
         (remhash key dictionary))))))

(defun bdef-set (key value &optional (dictionary *bdef-dictionary*))
  "Set the KEY in the bdef dictionary DICTIONARY to VALUE."
  (setf (gethash key dictionary) (bdef-key-cleanse value))
  (bdef-get key dictionary))

(defun bdef-load (object &key (num-channels 2) (wavetable nil) (start-frame 0))
  "Load an object into a bdef, inserting the relevant keys into the bdef dictionary."
  (let ((bdef
         (etypecase object
           (sc::env ;; FIX: we shouldn't assume that the user always wants an env to be a wavetable...
            (let* ((wavetable (or wavetable 512)) ;; FIX: if WAVETABLE is t...
                   (buffer (cl-collider:buffer-alloc (* 2 wavetable) :chanls num-channels))
                   (bdef (make-instance 'bdef
                                        :key object
                                        :buffer buffer)))
              (setf (bdef-metadata bdef :env) object)
              (cl-collider:buffer-setn buffer (cl-collider::list-in-wavetable-format (cl-collider::env-as-signal object wavetable)))
              bdef))
           (string
            (let* ((file (bdef-key-cleanse object))
                   (buffer (buffer-read-any file :num-channels num-channels :wavetable wavetable :start-frame start-frame))
                   (bdef (make-instance 'bdef
                                        :key file
                                        :buffer buffer)))
              (bdef-set file bdef)
              (alexandria:doplist (key function *auto-metadata*)
                  (let ((k key)
                        (f function))
                    (setf (bdef-metadata bdef key)
                          (eager-future2:pcall
                           (lambda ()
                             (let ((value (funcall f bdef)))
                               (setf (bdef-metadata bdef k) value)
                               value))))))
              bdef))
           (list ;; FIX: this needs to work properly when :wavetable is t
            (let* ((buffer (cl-collider:buffer-alloc (length object)))
                   (bdef (make-instance 'bdef
                                        :key object
                                        :buffer buffer)))
              (cl-collider:buffer-setn buffer object)
              bdef)))))
    (setf (bdef-metadata bdef :wavetable) (and wavetable t))
    bdef))

;;; auto-metadata

(defvar *auto-metadata* (list)
  "Plist of keys that will automatically be populated in a bdef's metadata for all newly-created or loaded buffers. The value for each key is the function that generates the value of the key for the bdef metadata. Use the `define-auto-metadata' macro or `set-auto-metadata' function to define auto-metadata keys, or `remove-auto-metadata' to remove them.")

(defun set-auto-metadata (key function)
  "Add KEY as an auto-metadata key for bdefs. FUNCTION will be run with the bdef as its argument, and the result will be set to the bdef's metadata for KEY."
  (setf (getf *auto-metadata* key) function))

(defmacro define-auto-metadata (key &body body)
  "Define an auto-metadata key for bdefs. The variable BDEF will be bound in BODY to the bdef in question."
  `(setf (getf *auto-metadata* ,key)
         (lambda (bdef) ,@body)))

(defun remove-auto-metadata (key)
  "Remove a previously-defined auto-metadata key."
  (alexandria:remove-from-plistf *auto-metadata* key))

(define-auto-metadata :onsets
  (unless (< (duration bdef) 1)
    (splits-from-aubio-onsets bdef)))

(define-auto-metadata :tempo
  (let* ((path (path bdef))
         (bpm (or
               (extract-bpm-from-string path)
               (extract-bpm-from-file-metadata path)
               (bpm-tools-bpm path))))
    (when bpm
      (/ bpm 60))))

(define-auto-metadata :dur
  (when (bdef-metadata bdef :tempo)
    (round (* (bdef-metadata bdef :tempo)
              (duration bdef)))))

;; FIX: add 'metadata' key. it should probably add to the default metadata fields. if the user doesn't want them, they can provide 'nil' for the key they don't want generated.
(defun bdef (key &optional (value nil value-provided-p) &key (num-channels 2) (wavetable nil) (start-frame 0) metadata) ;; FIX: start-frame key doesn't work yet
  "Automaticaly load a buffer or reference one that's already loaded. KEY is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without a VALUE, bdef will look up the key and return the buffer that already exists. If the KEY is a string, it's assumed to be a pathname and will be loaded automatically if it's not already in memory."
  (assert (not (and value
                    (stringp key)))
          (key)
          "Cannot use a string as a key.")
  (when (typep key 'bdef)
    (return-from bdef key))
  (let ((key (bdef-key-cleanse key))
        (value (bdef-key-cleanse value)))
    (if value-provided-p
        (let ((res (bdef-set key (if (bdef-get value)
                                     value
                                     (bdef-load value :num-channels num-channels :wavetable wavetable :start-frame start-frame)))))
          (alexandria:doplist (key value metadata)
              (setf (bdef-metadata res key) value))
          res)
        (or (bdef-get key)
            (if (symbolp key)
                (error "No bdef with the key ~a defined." key)
                (bdef-load key))))))

(defun bdef.new (&rest args) ;; (value nil value-provided-p) &key (num-channels 2) (wavetable nil) (start-frame 0) metadata
  "Automaticaly load a buffer or reference one that's already loaded. KEY is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without a VALUE, bdef will look up the key and return the buffer that already exists. If the KEY is a string, it's assumed to be a pathname and will be loaded automatically if it's not already in memory."
  (let* ((key (car args))
         (key-is-value (or (stringp key)
                           (pathnamep key)
                           (typep key 'cl-collider:env)))
         (value (if key-is-value
                    key
                    (elt args 1)))
         (options (if key-is-value
                      (cdr args)
                      (cddr args))
           ))
    (if (= 1 (length args))
        (format t "key lookup: ~s~%" key)
        (if (oddp (length options))
            (error "Invalid arguments for bdef.new: ~s" args)
            (format t "key: ~s; value: ~s; options: ~s~%" key value options)))))

;;; generics

(defgeneric id (object)
  (:documentation "Get the ID number of OBJECT."))

(defmethod id ((bdef bdef))
  (id (bdef-buffer bdef)))

(defmethod id ((symbol symbol))
  (id (bdef symbol)))

(defgeneric frames (object)
  (:documentation "Get the number of frames (samples) in OBJECT."))

(defmethod frames ((bdef bdef))
  (frames (bdef-buffer bdef)))

(defgeneric num-channels (object)
  (:documentation "Get the number of channels of OBJECT."))

(defmethod num-channels ((bdef bdef))
  (num-channels (bdef-buffer bdef)))

(defgeneric path (object)
  (:documentation "Get the path of OBJECT's file, or nil if it was not loaded from a file."))

(defmethod path ((bdef bdef))
  (path (bdef-buffer bdef)))

(defmethod path ((symbol symbol))
  (path (bdef symbol)))

(defmethod path ((string string))
  string)

(defmethod path ((pathname pathname))
  pathname)

(defgeneric duration (object)
  (:documentation "Get the duration of OBJECT in seconds."))

(defmethod duration ((bdef bdef))
  (duration (bdef-buffer bdef)))

