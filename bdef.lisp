;;;; bdef.lisp
;; FIX: use uiop:with-temporary-file to generate temporary files?
;; FIX: allow a sound to be loaded as a wavetable and a normal sound at the same time

(in-package #:bdef)

;;; global variables/configuration

(defvar *bdef-temporary-directory*
  (merge-pathnames "bdef/" (uiop:temporary-directory))
  "The directory bdef should store its temporary files in (i.e. the .wav files generated from format auto-conversion).")

(defvar *ffmpeg-path*
  #+(or linux darwin) (ignore-errors (uiop:run-program "which ffmpeg" :output (list :string :stripped t)))
  #+windows (concat (uiop:getenv-pathname "USERPROFILE") "/AppData/Local/") ;; FIX; this is almost certainly wrong
  "The path to ffmpeg, or nil if ffmpeg could not be found.")

(defvar *bdef-backends* (list)
  "The list of enabled backends. When the user attempts to create a bdef, each backend in this list is used to try to create the bdef from the file or object. If a backend returns nil, the next backend in the list will be tried. This is repeated until the first backend successfully returns a bdef object.

Note that backends are made available by loading the relevant bdef subsystem with Quicklisp or ASDF. For example, if you want to use SuperCollider/cl-collider, load `bdef/cl-collider'. Once you've loaded it, you should see that the :cl-collider keyword is in this list.")

;;; file handling

(defun ensure-readable-audio-file (path &key (num-channels 2) (extensions (list :wav :aif :aiff)))
  "If PATH ends in any of EXTENSIONS, return it unchanged. Otherwise, use ffmpeg to convert it to the first format in EXTENSIONS and return the path to the result. The converted file is stored under `*bdef-temporary-directory*'. Returns the file's metadata as a second value.

See also: `file-metadata', `*ffmpeg-path*', `*bdef-temporary-directory*'"
  (let* ((path (truename path))
         (ext (pathname-type path))
         (file-metadata (file-metadata path)))
    (values
     (namestring
      (if (and (position ext extensions :test #'string-equal)
               (eql num-channels (getf file-metadata :channels)))
          path
          (let* ((output-directory (merge-pathnames (concat num-channels "-channel/") *bdef-temporary-directory*))
                 (output-filename (make-pathname
                                   :directory (pathname-directory output-directory)
                                   :name (pathname-name path)
                                   :type (string-downcase (symbol-name (car extensions))))))
            (unless (probe-file output-filename)
              (ensure-directories-exist output-directory)
              (uiop:run-program (list *ffmpeg-path*
                                      "-i" (namestring path)
                                      "-ac" (write-to-string num-channels)
                                      (namestring output-filename))))
            output-filename)))
     file-metadata)))

;; FIX: just parse the json from the following command instead:
;; ffprobe -v quiet -print_format json -show_format -show_streams FILE
(defun file-metadata (file)
  "Get the metadata of FILE as a plist."
  (multiple-value-bind (stdout stderr)
      (uiop:run-program (list *ffmpeg-path* "-i" (namestring (truename file)) "-f" "ffmetadata" "-")
                        :output (list :string :stripped t)
                        :error-output (list :string :stripped t)
                        :ignore-error-status t)
    (let* ((split (cdr (split-string stdout :char-bag (list #\newline))))
           (kv (flatten (mapcar (lambda (line)
                                  (split-string line :char-bag #\= :max-num 2))
                                split)))
           (plist (loop :for (key value) :on kv :by #'cddr
                        :append (list (make-keyword (string-upcase key))
                                      value)))
           (channels (block channels ;; FIX: we don't detect if there are more than two or less than one (??) channels
                       (dotimes (n 2)
                         (when (search (nth n (list "mono," "stereo,")) stderr)
                           (return-from channels (1+ n))))
                       nil)))
      (append (when channels
                (list :channels
                      channels))
              plist))))

;;; backend generics

(defgeneric bdef-backend-supported-file-types (backend)
  (:documentation "Method returning a list of file extensions as keywords naming the file types that the backend natively supports (i.e. that don't need conversion).

See also: `bdef-backend-load'"))

(defgeneric bdef-backend-load (backend object &key)
  (:documentation "Load a file or other object into a bdef via the specified backend. This method should return the backend's buffer object. OBJECT will either be a string (the path to the file to load) or a list of strings (if the files should be loaded as consecutive buffers, i.e. for SuperCollider's VOsc UGen). Requested files will always be in a format that the backend reports supporting via the `bdef-backend-supported-file-types' method.

See also: `bdef-backend-free', `bdef-backend-supported-file-types'"))

(defmethod bdef-backend-load (backend (object pathname) &rest args &key &allow-other-keys)
  (apply #'bdef-backend-load backend (namestring object) args))

(defgeneric bdef-backend-free (buffer)
  (:documentation "Free a buffer via its backend.

See also: `bdef-backend-load'"))

;;; auto-metadata

(defvar *bdef-auto-metadata* (list)
  "Plist of keys that will automatically be populated in a bdef's metadata for all newly-created or loaded buffers. The value for each key is the function that generates the value of the key for the bdef metadata. Use the `define-bdef-auto-metadata' macro or `set-bdef-auto-metadata' function to define bdef-auto-metadata keys, or `remove-bdef-auto-metadata' to remove them.")

(defun set-bdef-auto-metadata (key function)
  "Add KEY as an auto-metadata key for bdefs. FUNCTION will be run with the bdef as its argument, and the result will be set to the bdef's metadata for KEY."
  (setf (getf *bdef-auto-metadata* key) function))

(defmacro define-bdef-auto-metadata (key &body body)
  "Define an auto-metadata key for bdefs. The variable BDEF will be bound in BODY to the bdef in question."
  `(setf (getf *bdef-auto-metadata* ,key)
         (lambda (bdef) ,@body)))

(defun remove-bdef-auto-metadata (key)
  "Remove a previously-defined auto-metadata key."
  (remove-from-plistf *bdef-auto-metadata* key))

(define-bdef-auto-metadata :onsets
  (unless (< (bdef-duration bdef) 1)
    (splits-from-aubio bdef :utility :onset)))

(define-bdef-auto-metadata :tempo
  (when-let* ((path (bdef-file bdef))
              (bpm (or (extract-bpm-from-string path)
                       (bpm-tools-bpm path))))
    (/ bpm 60)))

(define-bdef-auto-metadata :dur
  (when-let ((tempo (bdef-metadata bdef :tempo)))
    (round (* tempo (bdef-duration bdef)))))

(define-bdef-auto-metadata :splits
  (when-let ((original-file (bdef-metadata bdef :original-file)))
    (when (member (pathname-type (pathname original-file)) (list "aif" "aiff") :test #'string-equal)
      (splits-from-op-1-drumset original-file :if-invalid nil))))

;;; dynamic metadata

(defvar *bdef-dynamic-metadata* nil
  "Alist of dynamic metadata keys and their get and set functions.")

(defmacro define-bdef-dynamic-metadata (key get-body set-body)
  "Define a metadata key that is calculated dynamically. For example, bpm is just tempo multiplied by 60.

KEY is the name of the key, GET-BODY is the function to run to get the key, and SET-BODY is the function to run to set metadata when (setf (bdef-metadata bdef KEY) ...) is called.

See also: `define-bdef-auto-metadata', `bdef-metadata'"
  `(push (list (make-keyword ',key)
               (lambda (bdef) ,get-body)
               (lambda (bdef value) ,set-body))
         *bdef-dynamic-metadata*))

(define-bdef-dynamic-metadata bpm
    (when-let ((tempo (bdef-metadata bdef :tempo)))
      (* 60 tempo))
  (setf (bdef-metadata bdef :tempo) (/ value 60)))

(defun remove-bdef-dynamic-metadata (key)
  "Remove a key from the bdef dynamic metadata list."
  (let ((key (make-keyword key)))
    (setf *bdef-dynamic-metadata* (remove-if
                                   (lambda (item) (eql (car item) key))
                                   *bdef-dynamic-metadata*))))

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
    (format stream "~s is a ~s.~%  It is ~s seconds long (~s frames), with ~s channels.~%" bdef 'bdef (bdef-duration bdef) (bdef-length bdef) (bdef-channels bdef))
    (format stream "~@[  It contains the audio from the file ~s.~%~]" (bdef-file bdef))
    (format stream "  Keys that point to this buffer are: ~s~%" (bdef-keys bdef))
    (when-let ((meta-keys (bdef-metadata-keys bdef)))
      (format stream "  It has the following metadata:~%")
      (loop :for key :in meta-keys
            :do (format stream "    ~s -> ~s~%" key (bdef-metadata bdef key)))))) ;; FIX: use format's indentation directive?

(defun bdef-p (object)
  "True if OBJECT is a bdef.

See also: `bdef'"
  (typep object 'bdef))

(defgeneric bdef-key (bdef)
  (:documentation "The \"key\" (name) of this bdef."))

(defgeneric bdef-id (bdef)
  (:documentation "The ID number of this bdef, or nil if the bdef's backend does not support IDs."))

(defun bdef-key-cleanse (key)
  "Expands file names to their full unabbreviated forms as strings. Other values are simply returned as-is."
  (typecase key
    (pathname (namestring (truename key)))
    (string (bdef-key-cleanse (pathname key)))
    (otherwise key)))

;; FIX: add 'metadata' key. it should probably add to the default metadata fields. if the user doesn't want them, they can provide 'nil' for the key they don't want generated.
(defun bdef (key &optional (value nil value-provided-p) &key (num-channels 2) (wavetable nil) metadata)
  "Automaticaly load a buffer or reference one that's already loaded. KEY is the name to give the buffer in the bdef dictionary. VALUE is the path to the file to load, or the data to construct the buffer from (i.e. an envelope, a list of frames, etc).

Without a VALUE, bdef will look up the key and return the buffer that already exists. If the KEY is a string, it's assumed to be a pathname and will be loaded automatically if it's not already in memory."
  (assert (not (and value
                    (stringp key)))
          (key)
          "Cannot use a string as a key.")
  (when (bdef-p key)
    (return-from bdef key))
  (let ((key (bdef-key-cleanse key))
        (value (bdef-key-cleanse value)))
    (if value-provided-p
        (let ((res (bdef-set key (if (bdef-get value)
                                     value
                                     (bdef-load value :num-channels num-channels :wavetable wavetable)))))
          (doplist (key value metadata)
            (setf (bdef-metadata res key) value))
          res)
        (or (bdef-get key)
            (if (symbolp key)
                (error "No bdef with the key ~a defined." key)
                (bdef-load key))))))

(defgeneric bdef-buffer (object)
  (:documentation "The actual buffer object that the bdef refers to."))

(defmethod bdef-buffer ((symbol symbol))
  (bdef-buffer (ensure-bdef symbol)))

(defvar *bdef-dictionary* (make-hash-table :test #'equal)
  "The global dictionary of bdefs.")

(defun all-bdefs (&optional include-aliases)
  "Return a list of the names of all bdefs loaded.

Note that this doesn't include aliases (i.e. bdef keys that point to another key) unless INCLUDE-ALIASES is true.

See also: `bdef-dictionary-keys'"
  (if include-aliases
      (hash-table-keys *bdef-dictionary*)
      (loop :for i :being :the hash-keys :of *bdef-dictionary*
            :if (bdef-p (gethash i *bdef-dictionary*))
              :collect i)))

(defun bdef-dictionary-keys (&key (include-redirects t) (dictionary *bdef-dictionary*))
  "Return a list of all the keys for the bdef dictionary DICTIONARY.

See also: `all-bdefs'"
  (let ((keys (hash-table-keys dictionary)))
    (if include-redirects
        keys
        (loop :for key :in keys
              :if (bdef-p (gethash key dictionary))
                :collect key))))

(defun ensure-bdef (object) ;; FIX: make sure this (or equivalent, i.e. bdef-buffer) is used in all bdef functions
  "Return OBJECT if object is a bdef, otherwise look up a bdef with OBJECT as its key."
  (if (bdef-p object)
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

(defun bdef-metadata (bdef &optional key)
  "Get the value of BDEF's metadata for KEY. Returns true as a second value if the metadata had an entry for KEY, or false if it did not.

Note that this function will block if the specified metadata is one of the `*bdef-auto-metadata*' that hasn't finished being generated yet."
  (let ((bdef (ensure-bdef bdef)))
    (if key
        (if-let ((dyn-meta (assoc key *bdef-dynamic-metadata*)))
          (funcall (cadr dyn-meta) bdef)
          (multiple-value-bind (val present-p) (gethash key (slot-value bdef 'metadata))
            (values
             (if val
                 (if (typep val 'eager-future2:future)
                     (setf (bdef-metadata bdef key) (eager-future2:yield val))
                     val)
                 val)
             present-p)))
        (slot-value bdef 'metadata))))

(defun (setf bdef-metadata) (value bdef key)
  ;; if VALUE is a splits object and its `splits-bdef' is nil, set it to point to this bdef.
  (let ((bdef (ensure-bdef bdef)))
    (if-let ((dyn-meta (assoc key *bdef-dynamic-metadata*)))
      (funcall (caddr dyn-meta) bdef value)
      (progn
        (when (and (typep value 'splits)
                   (null (funcall 'splits-bdef value)))
          (setf (splits-bdef value) bdef))
        (setf (gethash key (slot-value bdef 'metadata)) value)))))

(defun bdef-metadata-keys (bdef)
  "Get a list of all keys in BDEF's metadata."
  (hash-table-keys (slot-value (ensure-bdef bdef) 'metadata)))

(defun bdef-splits (bdef)
  "Get any `splits' from BDEF's metadata, searching in preferred order (i.e. :splits key first, etc)."
  (let ((bdef (ensure-bdef bdef)))
    (loop
      :for key :in (list :splits :onsets) ;; FIX: check other keys too?
      :for val := (bdef-metadata bdef key)
      :if val
        :return val)))

(defun (setf bdef-splits) (splits bdef)
  (setf (bdef-metadata (ensure-bdef bdef) :splits) splits))

(defun bdef-keys (bdef &optional (dictionary *bdef-dictionary*))
  "Get a list of all the keys in DICTIONARY that point to BDEF."
  (when-let ((bdef (ensure-bdef bdef)))
    (loop :for key :being :the hash-keys :of dictionary
            :using (hash-value value)
          :if (eq value bdef)
            :collect key)))

(defun bdef-free (bdef &optional (dictionary *bdef-dictionary*))
  "Free a buffer from the bdef dictionary, removing all keys that point to it."
  (etypecase bdef
    (symbol (bdef-free (bdef bdef)))
    (string (bdef-free (bdef bdef)))
    (bdef
     (bdef-backend-free (bdef-buffer bdef))
     (let ((keys (bdef-keys bdef dictionary)))
       (dolist (key keys)
         (remhash key dictionary))))))

(defun bdef-set (key value &optional (dictionary *bdef-dictionary*))
  "Set the KEY in the bdef dictionary DICTIONARY to VALUE."
  (setf (gethash key dictionary) (bdef-key-cleanse value))
  (bdef-get key dictionary))

(defgeneric bdef-load (object &key)
  (:documentation "Load a file or other object into a bdef. Different backends will support different object types, and different key arguments. Here are the key arguments supported by one or more backends:

- id - Requested buffer ID number.
- num-channels - Number of channels to load (defaults to 2).
- wavetable - Whether to load the object in wavetable format. If T, the buffer length will be the next power of two. If a number is provided, load the object as a wavetable of that size.
- metadata - Plist of metadata to include in the bdef."))

(defmethod bdef-load ((object string) &rest args &key backend (num-channels 2) (wavetable nil) id metadata)
  (declare (ignorable id wavetable))
  (let ((original-file (bdef-key-cleanse object))
        (backend (or backend
                     (car *bdef-backends*)
                     (error "No bdef backends are currently enabled. Enable one by loading its subsystem."))))
    (multiple-value-bind (file file-metadata)
        (ensure-readable-audio-file original-file :num-channels num-channels :extensions (bdef-backend-supported-file-types backend))
      (let* ((buffer (apply 'bdef-backend-load (or backend (car *bdef-backends*)) file (remove-from-plist args :backend :num-channels)))
             (bdef (make-instance 'bdef
                                  :key file
                                  :buffer buffer)))
        (setf (bdef-metadata bdef :original-file) original-file)
        (bdef-set file bdef)
        (doplist (key value file-metadata)
          (unless (eql key :channels) ;; the channels key is inserted by the `file-metadata' function for the number of channels the source (pre-conversion) has
            (if (member key (list :bpm :tbpm))
                (let ((parsed (parse-float:parse-float value)))
                  (unless (= 0 parsed)
                    (setf (bdef-metadata bdef :bpm) parsed)))
                (setf (bdef-metadata bdef key) value))))
        (doplist (key function *bdef-auto-metadata*)
          (unless (or (bdef-metadata bdef key) ;; we trust the file's tags; no need to detect the bpm if we already know it
                      (getf metadata key))
            (let ((k key)
                  (f function))
              (setf (bdef-metadata bdef key)
                    (eager-future2:pcall
                     (lambda ()
                       (let ((value (funcall f bdef)))
                         (setf (bdef-metadata bdef k) value)
                         value)))))))
        (doplist (key value metadata)
          (setf (bdef-metadata bdef key) value))
        bdef))))

(defmethod no-applicable-method ((method (eql #'bdef-load)) &rest args)
  (error "None of the enabled bdef backends support loading ~s." (car args)))

;;; bdef methods:
;; (these should be implemented for each bdef backend)

(defgeneric bdef-length (bdef)
  (:documentation "Get the length of the bdef in frames."))

(defmethod bdef-length (bdef)
  (bdef-length (bdef-buffer bdef)))

(defgeneric bdef-sample-rate (bdef)
  (:documentation "Get the length of the bdef in frames."))

(defmethod bdef-sample-rate (bdef)
  (bdef-sample-rate (bdef-buffer bdef)))

(defgeneric bdef-channels (bdef)
  (:documentation "Get the number of channels of the bdef."))

(defmethod bdef-channels (bdef)
  (bdef-channels (bdef-buffer bdef)))

(defgeneric bdef-file (bdef)
  (:documentation "The file that the bdef was loaded from, or nil if it was not loaded from a file."))

(defmethod bdef-file (bdef)
  (bdef-file (bdef-buffer bdef)))

(defgeneric bdef-subseq (bdef &optional start end channel)
  (:documentation "Get an array of the frames of BDEF's buffer, from START below END. CHANNEL specifies which channel(s) to get; if an integer, returns a single-dimensional array of the specified channel; if a list, returns a multi-dimensional array of the specified channels. Defaults to all frames of all channels.

See also: `bdef-elt'"))

(defmethod bdef-subseq (bdef &optional (start 0) (end (bdef-length bdef)) channel)
  (bdef-subseq (bdef-buffer bdef) start end channel))

;;; derived methods:
;; (you shouldn't need to implement these manually in a backend)

(defgeneric bdef-duration (bdef)
  (:documentation "Get the duration of the bdef in seconds."))

(defmethod bdef-duration ((bdef t))
  (let ((buffer (bdef-buffer bdef)))
    (/ (bdef-length buffer) (bdef-sample-rate buffer))))

(defgeneric bdef-tempo (bdef)
  (:documentation "Get the tempo of the bdef in beats per second. Defaults to 1 if no tempo metadatum exists."))

(defmethod bdef-tempo (bdef)
  (or (bdef-metadata bdef :tempo) 1))

(defgeneric bdef-dur (bdef)
  (:documentation "Get the dur of the bdef in beats."))

(defmethod bdef-dur (bdef)
  (let ((time (bdef-duration bdef))
        (tempo (bdef-tempo bdef)))
    (* time tempo)))

(defgeneric bdef-elt (bdef index &optional channel)
  (:documentation "Get the value of the bdef frame at INDEX. Without CHANNEL, return an array of the specified index in all channels. With CHANNEL, return just the value as a number.

See also: `bdef-subseq'"))

(defmethod bdef-elt (bdef index &optional channel)
  (let ((res (bdef-subseq bdef index (1+ index) channel)))
    (if (integerp channel)
        (aref res 0)
        (let ((len (apply #'* (array-dimensions res))))
          (make-array (list len)
                      :initial-contents (loop :for i :from 0 :repeat len
                                              :collect (row-major-aref res i)))))))
