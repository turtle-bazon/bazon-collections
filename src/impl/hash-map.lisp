;;; -*- lisp -*-

(in-package :ru.bazon.bazon-collections)

(defclass hash-map (abstract-map)
  ((initial-capacity
    :initarg :initial-capacity
    :initform 16
    :type fixnum
    :documentation "Capacity of back array, MUST be power of 2")
   (load-factor
    :initargs :load-factor
    :initform 0.75
    :type single-float
    :documentation "Load factor")
   (threshold
    :initform 0
    :type (integer 0 *)
    :documentation "Next size at wich back array will resize")
   (size
    :initform 0
    :type fixnum
    :documentation "Total number of stored objects")
   (elements-array
    :initform nil
    :type (simple-vector *)))
  (:documentation "HashMap implementation."))

(defclass hash-map-iterator (abstract-iterator)
  ((hash-map
    :initarg :hash-map
    :type hash-map
    :initform (error "Specify hash-map."))
   (current-index
    :initform -1
    :type fixnum)
   (current-bucket
    :initform nil)
   (extractor-fn
    :initform #'identity
    :initarg :extractor-fn))
  (:documentation "Iterator over hash-map."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-map-new-elements (capacity)
  (make-array capacity :initial-element nil))

(defun hash-map-hash (hash)
  (let ((h (logxor hash (logxor (ash hash -20) (ash hash -12)))))
    (logxor h (logxor (ash h -7) (ash h -4)))))

(defun hash-map-index (hash size)
  (logand hash (- size 1)))

(defun hash-map-key-index (hash-map key)
  (with-slots (hash elements-array)
      hash-map
    (let* ((current-capacity (length elements-array))
           (key-hash (hash-map-hash (funcall hash key)))
           (index (hash-map-index key-hash current-capacity)))
      index)))

(defun hash-map-find-entry-at-index (hash-map key index)
  (with-slots (test elements-array)
      hash-map
    (loop for sub-bucket on (svref elements-array index)
          as entry = (car (car sub-bucket))
          as entry-key = (slot-value entry 'key)
          when (funcall test key entry-key)
            return entry)))

(defun hash-map-find-entry (hash-map key)
  (with-slots (test elements-array)
      hash-map
    (let ((index (hash-map-key-index hash-map key)))
      (hash-map-find-entry-at-index hash-map key index))))

(defun hash-map-resize (hash-map new-capacity)
  (with-slots (test hash load-factor threshold elements-array)
      hash-map
    (let ((new-elements-array (hash-map-new-elements new-capacity)))
      (loop for bucket across elements-array
            do (loop for sub-bucket on bucket
                     as entry = (car (car sub-bucket))
                     do (with-slots ((entry-key key))
                            entry
                          (let* ((key-hash (hash-map-hash (funcall hash entry-key)))
                                 (index (hash-map-index key-hash new-capacity))
                                 (existing-bucket (svref new-elements-array index))
                                 (new-bucket (cons (cons entry nil) existing-bucket)))
                            (when existing-bucket
                              (setf (cdr (car existing-bucket)) new-bucket))
                            (setf (svref new-elements-array index) new-bucket)))))
      (setf elements-array new-elements-array)
      (setf threshold (floor (* load-factor new-capacity))))))

(defmethod initialize-instance :after ((map hash-map) &key)
  (with-slots (initial-capacity load-factor threshold elements-array)
      map
    (setf elements-array (hash-map-new-elements initial-capacity))
    (setf threshold (floor (* initial-capacity load-factor)))))

(defmethod iterator ((map hash-map) &optional condition)
  (let ((iterator (make-instance 'hash-map-iterator :hash-map map)))
    (if condition
        (make-instance 'conditional-iterator :back-iterator iterator
                                             :condition condition)
	iterator)))

(defmethod size ((map hash-map))
  (with-slots (size)
      map
    size))

(defmethod clear ((map hash-map))
  (with-slots (initial-capacity load-factor threshold size elements-array)
      map
    (setf size 0)
    (setf elements-array (hash-map-new-elements initial-capacity))
    (setf threshold (floor (* initial-capacity load-factor)))))

(defmethod get-keys ((map hash-map))
  (make-instance 'hash-map-iterator :hash-map map :extractor-fn (lambda (entry) (slot-value entry 'key))))

(defmethod get-values ((map hash-map))
  (make-instance 'hash-map-iterator :hash-map map :extractor-fn (lambda (entry) (slot-value entry 'value))))

(defmethod contains-key ((map hash-map) key)
  (let ((existing-entry (hash-map-find-entry map key)))
    (when existing-entry
      t)))

(defmethod contains-value ((map hash-map) object)
  (with-slots (test elements-array)
      map
    (loop for bucket across elements-array
          as found-in-bucket = (loop for sub-bucket on bucket
                                     as entry = (car (car sub-bucket))
                                     as entry-value = (slot-value entry 'value)
                                     when (funcall test object entry-value)
                                       return entry)
          when found-in-bucket
            return t)))

(defmethod get-object ((map hash-map) key)
  (let ((existing-entry (hash-map-find-entry map key)))
    (when existing-entry
      (slot-value existing-entry 'value))))

(defmethod rem-object ((map hash-map) key)
  (with-slots (test size elements-array)
      map
    (let* ((index (hash-map-key-index map key))
           (existing-bucket (svref elements-array index))
           (removed-struct (when existing-bucket
                             (loop for sub-bucket on existing-bucket
                                   as entry = (car (car sub-bucket))
                                   as prev-bucket = (cdr (car sub-bucket))
                                   as entry-key = (slot-value entry 'key)
                                   when (funcall test key entry-key)
                                     return (let ((next-bucket (cdr sub-bucket)))
                                              (when prev-bucket
                                                (setf (cdr prev-bucket) next-bucket))
                                              (when next-bucket
                                                (setf (cdr (car next-bucket)) prev-bucket))
                                              (cons t (if prev-bucket
                                                          existing-bucket
                                                          next-bucket)))))))
      (when removed-struct
        (let ((removed-p (car removed-struct))
              (new-bucket (cdr removed-struct)))
          (when removed-p
            (decf size)
            (setf (svref elements-array index) new-bucket)))))))

(defmethod put-object ((map hash-map) key object)
  (with-slots (threshold size elements-array)
      map
    (let* ((index (hash-map-key-index map key))
           (existing-bucket (svref elements-array index))
           (existing-entry (hash-map-find-entry-at-index map key index)))
      (if existing-entry
          (with-slots ((existing-value value))
              existing-entry
            (setf existing-value object))
          (let ((new-entry (make-instance 'map-entry :key key :value object)))
            (setf (svref elements-array index) (cons (cons new-entry nil) existing-bucket))
            (incf size)
            (when (>= size threshold)
              (hash-map-resize map (ash (length elements-array) 1)))
            object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod it-current ((iterator hash-map-iterator))
  (with-slots (current-bucket extractor-fn)
      iterator
    (funcall extractor-fn (car (car current-bucket)))))

(defmethod it-next ((iterator hash-map-iterator))
  (with-slots (hash-map current-index current-bucket)
      iterator
    (with-slots (elements-array)
        hash-map
      (let* ((array-size (length elements-array))
             (last-index (- array-size 1))
             (new-position (if (or current-bucket
                                   (< current-index last-index))
                               (loop for (index . bucket) = (cons current-index current-bucket)
                                       then next-position
                                     for next-position
                                       = (if bucket
                                             (cons index (cdr bucket))
                                             (let ((next-index (+ index 1)))
                                               (cons next-index (svref elements-array next-index))))
                                     as next-index = (car next-position)
                                     as next-bucket = (cdr next-position)
                                     when next-bucket
                                       return (cons next-index next-bucket)
                                     while (< next-index last-index)
                                     finally (return (cons array-size nil)))
                               (cons current-index current-bucket)))
             (new-index (car new-position))
             (new-bucket (cdr new-position)))
        (setf current-index new-index)
        (setf current-bucket new-bucket)
        (not (null current-bucket))))))

(defmethod it-prev ((iterator hash-map-iterator))
  (with-slots (hash-map current-index current-bucket)
      iterator
    (with-slots (elements-array)
        hash-map
      (let* ((new-position (if (or current-bucket
                                   (> current-index 0))
                               (loop for (index . bucket) = (cons current-index current-bucket)
                                          then prev-position
                                        for prev-position
                                          = (if bucket
                                                (cons index (cdr (car bucket)))
                                                (let ((prev-index (- index 1)))
                                                  (cons prev-index (last (svref elements-array prev-index)))))
                                        as prev-index = (car prev-position)
                                        as prev-bucket = (cdr prev-position)
                                        when prev-bucket
                                          return (cons prev-index prev-bucket)
                                        while (> prev-index 0)
                                     finally (return (cons -1 nil)))
                               (cons current-index current-bucket)))
             (new-index (car new-position))
             (new-bucket (cdr new-position)))
        (setf current-index new-index)
        (setf current-bucket new-bucket)
        (not (null current-bucket))))))

(defmethod it-before-first ((iterator hash-map-iterator))
  (with-slots (current-index current-bucket)
      iterator
    (setf current-index -1)
    (setf current-bucket nil)))

(defmethod it-after-last ((iterator hash-map-iterator))
  (with-slots (hash-map current-index current-bucket)
      iterator
    (with-slots (elements-array)
        hash-map
      (setf current-index (length elements-array))
      (setf current-bucket nil))))
