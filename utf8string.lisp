(defpackage #:utf8
  (:use #:cl)
  (:export #:utf8-string))

(in-package #:utf8)

(deftype data () '(simple-array (unsigned-byte 8) (*)))
(deftype index () `(integer 0 (,(1- array-dimension-limit))))

(defclass utf8-string (sequence standard-object)
  ((%length :accessor utf8-string-length :initarg :length
            :reader sequence:length
            :type (integer 0 (#.array-dimension-limit)))
   (%data :accessor utf8-string-data :initarg :data
          :type data)))

(defmethod print-object ((o utf8-string) stream)
  (print-unreadable-object (o stream :type t)
    ;; We do this so that errors while iterating
    ;; don't result in the entire print failing.
    (write-string
     (handler-case
         ;; Build a string, escaping as you go
         (with-output-to-string (stream)
           (write-char #\" stream)
           (map nil (lambda (char)
                      (when (or (char= char #\")
                                (char= char #\\))
                        (write-char #\\ stream))
                      (write-char char stream))
                o)
           (write-char #\" stream))
       (serious-condition () "<invalid>"))
     stream)))

(defun %make-utf8-string (length
                          &optional (data
                                     (make-array
                                      length
                                      :element-type
                                      '(unsigned-byte 8))))
  (make-instance 'utf8-string :length length :data data))

;;; These functions determine what role the given byte
;;; has in the utf8 encoding. byte-1-start-p tests if it
;;; starts a 1-byte codepoint, etc with other n, and
;;; continuation-byte-p tests if it's a later byte in a
;;; codepoint.
(declaim (inline byte-1-start-p byte-2-start-p
                 byte-3-start-p byte-4-start-p
                 continuation-byte-p))
(defun byte-1-start-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (not (logbitp 7 byte)))
(defun byte-2-start-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (= (ldb (byte 3 5) byte) #b110))
(defun byte-3-start-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (= (ldb (byte 4 4) byte) #b1110))
(defun byte-4-start-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (= (ldb (byte 5 3) byte) #b11110))
(defun continuation-byte-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (= (ldb (byte 2 6) byte) #b10))

;;; Given a start byte, how many bytes is the codepoint?
(defun start-byte-length (byte)
  (declare (type (unsigned-byte 8) byte))
  (cond ((byte-1-start-p byte) 1)
        ((byte-2-start-p byte) 2)
        ((byte-3-start-p byte) 3)
        ((byte-4-start-p byte) 4)
        (t (error "Invalid start byte #x~x" byte))))

(defun codepoint-length (codepoint)
  (declare (type (and fixnum (integer 0)) codepoint))
  (cond ((< codepoint #x80) 1)
        ((< codepoint #x800) 2)
        ((< codepoint #x10000) 3)
        ((< codepoint #x110000) 4)
        (t (error "BUG: Codepoint #x~x out of range" codepoint))))

(defun char-length (char)
  (codepoint-length (char-code char)))

;; Given ub8vector data and an index into it, return the index
;; of the beginning of the next character.
;; Does no bounds checking and mostly assumes valid encoding.
(defun next-index (data index)
  (declare (type data data) (type index index))
  (+ index (start-byte-length (aref data index))))

;; Like the above but backwards.
;; If the provided index is zero, returns -1.
;; Assumes valid encoding.
(defun prev-index (data index)
  (declare (type data data) (type index index))
  (when (zerop index) (return-from prev-index -1))
  ;; Just go back until a start byte is encountered.
  (loop for i downfrom (1- index)
        unless (continuation-byte-p (aref data index))
          return i))

;; Given data and an underlying index into it,
;; return the codepoint at that position. No bounds checks.
(defun get-codepoint (data index)
  (declare (type data data) (type index index))
  (let ((byte0 (aref data index)))
    (if (byte-1-start-p byte0)
        byte0
        (let ((byte1 (aref data (+ index 1))))
          (if (byte-2-start-p byte0)
              ;; Two byte character
              ;; Take low five of byte0 and low six of byte1
              (logior      (ldb (byte 6 0) byte1)
                      (ash (ldb (byte 5 0) byte0) 6))
              (let ((byte2 (aref data (+ index 2))))
                (if (byte-3-start-p byte0)
                    ;; Three byte character
                    ;; 4 of byte0, 6 of byte1, 6 of byte2
                    (logior      (ldb (byte 6 0) byte2)
                            (ash (ldb (byte 6 0) byte1) 6)
                            (ash (ldb (byte 4 0) byte0) 12))
                    (let ((byte3 (aref data (+ index 3))))
                      (if (byte-4-start-p byte0)
                          ;; Four byte character
                          ;; 3 of byte0, 6 each of bytes 1-3
                          (logior      (ldb (byte 6 0) byte3)
                                  (ash (ldb (byte 6 0) byte2) 6)
                                  (ash (ldb (byte 6 0) byte1) 12)
                                  (ash (ldb (byte 3 0) byte0) 18))
                          (error "Invalid encoding at position ~d"
                                 index))))))))))

;; Given data and an underlying index into it,
;; return the character at that position. No bounds checks.
(defun get-char (data index)
  (code-char (get-codepoint data index)))

;;; Given a codepoint, data, and an underlying index into it,
;;; write the codepoint into the data at that position.
;;; No bounds checks, may overwrite with impunity.
;;; Return value undefined.
(defun set-codepoint (codepoint data index)
  (declare (type data data) (type index index))
  (cond ((< codepoint #x80) ; one byte
         (setf (aref data index) codepoint))
        ((< codepoint #x800) ; two bytes
         (let ((byte0 (logior #xc0 (ldb (byte 5 6) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 0) codepoint))))
           (setf (aref data      index) byte0
                 (aref data (1+ index)) byte1)))
        ((< codepoint #x10000) ; three byte
         (let ((byte0 (logior #xe0 (ldb (byte 4 12) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  0) codepoint))))
           (setf (aref data       index) byte0
                 (aref data  (1+ index)) byte1
                 (aref data (+ index 2)) byte2)))
        ((< codepoint #x11000) ; four byte
         (let ((byte0 (logior #xf0 (ldb (byte 3 18) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 12) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte3 (logior #x80 (ldb (byte 6  0) codepoint))))
           (setf (aref data       index) byte0
                 (aref data  (1+ index)) byte1
                 (aref data (+ index 2)) byte2
                 (aref data (+ index 3)) byte3)))
        (t (error "BUG: Codepoint #x~x out of range" codepoint))))

;;; Given a character, data, and an underlying index into it,
;;; set the character at that position. No bounds checks.
;;; Returns the character.
(defun set-char (character data index)
  (set-codepoint (char-code character) data index)
  character)

;;; Make a new data vector based on an old one.
;;; Bytes before END are copied into the new one.
;;; Then LEN bytes of space are allocated and uninitialized.
;;; Then the space between start2 and end2 is copied in.
(defun expand-data (data end len start2 end2)
  (let ((result (make-array (+ end len (- end2 start2))
                            :element-type '(unsigned-byte 8))))
    (replace result data :end1 end :end2 end)
    (replace result data :start1 (+ end len)
                         :start2 start2 :end2 end2)
    result))

;;; Like the above, but get the sequence as an argument,
;;; and resize the data if necessary (i.e. if the new and
;;; old characters have different lengths).
;;; Resizing is, of course, hells of slow.
(defun set-data-char (new sequence byte-index)
  (let* ((data (utf8-string-data sequence))
         (byte (aref data byte-index))
         (old-length (start-byte-length byte))
         (new-length (char-length new)))
    (unless (= old-length new-length)
      ;; Apocalyptically slow case: Resize.
      (setf data
            (expand-data data byte-index new-length
                         (+ byte-index old-length) (length data))
            (utf8-string-data sequence)
            data))
    ;; Now write in the codepoint.
    (set-char new data byte-index)))

;; Given the index of a character, return an index into the
;; underlying data (provided). This basically has to iterate
;; through the sequence, so it's linear time probably.
;; If passed the length of the string, returns the length of
;; the data.
;; FIXME: We sometimes do this twice for a start and end,
;; in which case we coudl start the second iteration later
;; and save a bit of time.
(defun char-index (data index)
  (loop for r = 0 then (next-index data r)
        repeat index
        finally (return r)))

;; Given a data vector and a codepoint, fill the vector with
;; that codepoint. It's assumed to be the correct length.
(defun fill-vec-with-codepoint (data codepoint
                                &optional (start-byte 0)
                                  (end-byte (length data)))
  (declare (type data data)
           (type (and fixnum (integer 0)) codepoint))
  (cond ((< codepoint #x80) ; one byte
         (fill data codepoint :start start-byte :end end-byte))
        ((< codepoint #x800) ; two byte
         (let ((byte0 (logior #xc0 (ldb (byte 5 6) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 0) codepoint))))
           (loop for i from start-byte below end-byte by 2
                 do (setf (aref data      i) byte0
                          (aref data (1+ i)) byte1))))
        ((< codepoint #x10000) ; three byte
         (let ((byte0 (logior #xe0 (ldb (byte 4 12) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  0) codepoint))))
           (loop for i from start-byte below end-byte by 3
                 do (setf (aref data       i) byte0
                          (aref data  (1+ i)) byte1
                          (aref data (+ i 2)) byte2))))
        ((< codepoint #x11000) ; four byte
         (let ((byte0 (logior #xf0 (ldb (byte 3 18) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 12) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte3 (logior #x80 (ldb (byte 6  0) codepoint))))
           (loop for i from start-byte below end-byte by 4
                 do (setf (aref data       i) byte0
                          (aref data  (1+ i)) byte1
                          (aref data (+ i 2)) byte2
                          (aref data (+ i 3)) byte3))))
        (t (error "BUG: Codepoint #x~x out of range" codepoint)))
  data)

(defun fill-vec-with-char (data char
                           &optional (start-byte 0)
                             (end-byte (length data)))
  (fill-vec-with-codepoint data (char-code char)
                           start-byte end-byte))

;;; Given a data vector and an svector of characters, replace the
;;; data with the vector (beginning at index START).
;;; Assumes correct length.
(defun replace-vec-with-charv (data string
                               &optional (start 0)
                                 (start-byte 0)
                                 (end-byte (length data)))
  (declare (type (simple-array * (*)) string)
           (type data data))
  (loop with byte-index = start-byte
        for string-index from start
        until (= byte-index end-byte)
        do (let ((char (aref string string-index)))
             (set-char char data byte-index)
             (incf byte-index (char-length char)))))

;;; Like the above, but with data vectors, so the bytes are just
;;; copied directly.0
(defun replace-vec-with-vec (data data2
                             &optional (start-byte2 0)
                               (start-byte 0)
                               (end-byte (length data)))
  (declare (type data data data2))
  (replace data data2 :start1 start-byte :end1 end-byte :start2 start-byte2))

;;; Given a vector of characters, return the number
;;; of UTF-8 bytes required to represent it.
(defgeneric sequence-nbytes (sequence))
(defmethod sequence-nbytes ((sequence utf8-string))
  (length (utf8-string-data sequence)))
(defmethod sequence-nbytes ((sequence sequence))
  (reduce #'+ sequence :key #'char-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extensible sequences protocol
;;;

(defmethod sequence:elt ((sequence utf8-string) index)
  (let ((data (utf8-string-data sequence)))
    (get-char data (char-index data index))))

(defmethod (setf sequence:elt) (new (sequence utf8-string) index)
  (set-data-char new sequence
                 (char-index (utf8-string-data sequence) index)))

(defmethod sequence:make-sequence-like
    ((sequence utf8-string) length
     &key (initial-element nil iep) (initial-contents nil icp))
  (cond ((and iep icp)
         (error "supplied both ~s and ~s to ~s"
                :initial-element :initial-contents
                'sequence:make-sequence-like))
        (iep
         (let ((char-length (char-length initial-element)))
           (if (= char-length 1)
               (%make-utf8-string length
                                  (make-array length
                                              :element-type '(unsigned-byte 8)
                                              :initial-element (char-code initial-element)))
               (let ((data (make-array (* length char-length)
                                       :element-type '(unsigned-byte 8))))
                 (prog1 (%make-utf8-string length data)
                   (fill-vec-with-char data initial-element))))))
        (icp
         (if (typep initial-contents 'utf8-string)
             ;; Special case: just copy
             (%make-utf8-string
              (utf8-string-length initial-contents)
              (copy-seq (utf8-string-data initial-contents)))
             ;; Normal case: Slow
             (let* ((vec-length (sequence-nbytes initial-contents))
                    (data (make-array vec-length
                                      :element-type
                                      '(unsigned-byte 8))))
               (replace-vec-with-charv data initial-contents)
               (%make-utf8-string length data))))
        (t (%make-utf8-string length))))

;; adjust-sequence

;;; make-sequence-iterator uses default implementation
;;; (i.e., make-simple-sequence-iterator)

;;; Iterators are a cons of the character index with the
;;; byte index. Both are necessary due to possible resizing.
;;; The endpoint is just the character index.
(defmacro iterator-char-index (it) `(car ,it))
(defmacro iterator-byte-index (it) `(cdr ,it))
(defmethod sequence:make-simple-sequence-iterator
    ((sequence utf8-string) &key from-end (start 0) end)
  (when (null end) (setf end (length sequence)))
  (let ((data (utf8-string-data sequence)))
    (if from-end
        ;; FIXME: Probably broken at the edges
        (values (cons (1- end) (char-index data (1- end)))
                (1- start)
                t)
        (values (cons start (char-index data start))
                end
                nil))))

(defmethod sequence:iterator-step
    ((sequence utf8-string) iterator from-end)
  (let ((data (utf8-string-data sequence)))
    (if from-end
        (setf (iterator-char-index iterator)
              (1- (iterator-char-index iterator))
              (iterator-byte-index iterator)
              (prev-index data (iterator-byte-index iterator)))
        (setf (iterator-char-index iterator)
              (1+ (iterator-char-index iterator))
              (iterator-byte-index iterator)
              (next-index data (iterator-byte-index iterator)))))
  iterator)

(defmethod sequence:iterator-endp
    ((sequence utf8-string) iterator limit from-end)
  (declare (ignore from-end))
  (= (iterator-char-index iterator) limit))

(defmethod sequence:iterator-element
    ((sequence utf8-string) iterator)
  (get-char (utf8-string-data sequence)
            (iterator-byte-index iterator)))

;;; Also hella slow.
(defmethod (setf sequence:iterator-element)
    (new (sequence utf8-string) iterator)
  (set-data-char new sequence (iterator-byte-index iterator)))

(defmethod sequence:iterator-index ((sequence utf8-string) iterator)
  (iterator-char-index iterator))

(defmethod sequence:iterator-copy ((sequence utf8-string) iterator)
  (cons (iterator-char-index iterator)
        (iterator-byte-index iterator)))

;;; map, count, find, position are probably fine
;;; however, MAP /could/ allocate four bytes per character
;;; and then shrink at the end; this would save time if
;;; any character is more than one byte, but make the all
;;; one byte case a little slower.

;; Copy the underlying bytes
(defmethod sequence:subseq
    ((sequence utf8-string) start &optional end)
  (let ((end (or end (length sequence)))
        (data (utf8-string-data sequence)))
    (%make-utf8-string
     (- end start)
     (subseq data (char-index data start) (char-index data end)))))

;; Default method (that is, (subseq seq 0)) would work,
;; but this is even easier and doesn't iterate
(defmethod sequence:copy-seq ((sequence utf8-string))
  (%make-utf8-string
   (utf8-string-length sequence)
   (copy-seq (utf8-string-data sequence))))

;;; While the default method is adequate, we can do better:
;;; Only resize once, and write the bytes in a tighter loop.
(defmethod sequence:fill
    ((sequence utf8-string) item &key (start 0) end)
  (when (null end) (setf end (utf8-string-length sequence)))
  (let* ((data (utf8-string-data sequence))
         (start-byte (char-index data start))
         (end-byte (char-index data end))
         (new-char-length (char-length item))
         (new-area-len (* (- end start) new-char-length)))
    (unless (= new-area-len (- end-byte start-byte))
      ;; We have to resize the vector (slow path)
      (setf data
            (expand-data data start-byte new-area-len
                         end-byte
                         (length data))
            (utf8-string-data sequence) data))
    (fill-vec-with-char data item
                        start-byte (+ start-byte new-area-len)))
  sequence)
;;; (n)substitute will be fucky

(defmethod sequence:replace
    ((s1 utf8-string) (s2 sequence)
     &key (start1 0) end1 (start2 0) end2)
  (when (null end1) (setf end1 (length s1)))
  (when (null end2) (setf end2 (length s2)))
  (let* ((data (utf8-string-data s1))
         (start-byte (char-index data start1))
         (end-byte (char-index data end1))
         ;; How many bytes do we need for these characters?
         (required-area-len (reduce #'+ s2
                                    :start start2 :end end2
                                    :key #'char-length)))
    (unless (= required-area-len (- end-byte start-byte))
      ;; Resize the data if necessary
      (setf data
            (expand-data data start-byte required-area-len
                         end-byte (length data))
            (utf8-string-data s1) data))
    ;; Actually replace
    (replace-vec-with-charv data s2 start2
                            start-byte
                            (+ start-byte required-area-len)))
  s1)
;;; When s2 is another utf8-string, we can work with bytes
;;; directly and skip encoding/decoding.
(defmethod sequence:replace
    ((s1 utf8-string) (s2 utf8-string)
     &key (start1 0) end1 (start2 0) end2)
  (when (null end1) (setf end1 (length s1)))
  (when (null end2) (setf end2 (length s2)))
  (let* ((data1 (utf8-string-data s1))
         (start-byte1 (char-index data1 start1))
         (end-byte1 (char-index data1 end1))
         (data2 (utf8-string-data s2))
         (start-byte2 (char-index data2 start2))
         (end-byte2 (char-index data2 end2))
         (new-len (- end-byte2 start-byte2)))
    (unless (= new-len (- end-byte1 start-byte1))
      (setf data1
            (expand-data data1 start-byte1 new-len
                         end-byte1 (length data1))
            (utf8-string-data s1) data1))
    (replace-vec-with-vec data1 data2 start-byte2
                          start-byte1
                          (+ start-byte1 new-len)))
  s1)

;;; the default reverse = nreverse copy-seq is fine

;;; NREVERSE: Reverse the bytes, then iterate over
;;; them reversing individual codepoints.
;;; NOTE: Probably a faster way to do this, but I don't
;;; know it.

;;; This function is separated out for debugging
(defun fix-nreverse-data (data)
  (declare (type data data))
  (loop ;; how many bytes in this codepoint so far
        with nbytes = 0
        for i from 0 below (length data)
        do (aref data i)
        if (continuation-byte-p (aref data i))
          do (incf nbytes)
        else ; reverse codepoint
        do (loop for j below (ceiling nbytes 2)
                 do (rotatef (aref data (- i j))
                             (aref data (- (+ i j) nbytes))))
           (setf nbytes 0))
  data)
(defmethod sequence:nreverse ((sequence utf8-string))
  (setf (utf8-string-data sequence)
        (fix-nreverse-data
         (nreverse (utf8-string-data sequence))))
  sequence)

;;; Make a sequence of the correct byte length
;;; so we don't have to resize
#+(or)
(defmethod sequence:concatenate ((proto utf8-string)
                                 &rest sequences)
  (let* ((lengths (mapcar #'length sequences))
         (byte-length (reduce #'+ sequences
                              :key #'sequence-nbytes))
         (data (make-array byte-length
                           :element-type '(unsigned-byte 8)))
         (result (%make-utf8-string (reduce #'+ lengths) data)))
    (format t "byte-length before: ~d~%" byte-length)
    (loop with index = 0
          for sequence in sequences
          for length in lengths
          do (replace result sequence :start1 index)
             (incf index length))
    (format t "byte-length after: ~d~%" (sequence-nbytes result))
    result))

;; concatenate will be fucky
;; reduce, mismatch, search is ok with default implementation
;; delete will be fucky
;; remove will be fucky but maybe less??
;; delete/remove-duplicates, same shit i guess
;; (stable-)sort, merge, probably bad
