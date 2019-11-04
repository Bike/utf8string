(defpackage #:utf8
  (:use #:cl)
  (:export #:utf8-string))

(in-package #:utf8)

(deftype data () '(simple-array (unsigned-byte 8) (*)))
(deftype index () `(integer 0 (,(1- array-dimension-limit))))

(defun required-slot ()
  (error "BUG: Tried to make a utf8-string with missing slot"))

(defclass utf8-string (sequence standard-object)
  ((%length :accessor utf8-string-length :initarg :length
            :reader sequence:length
            :type (integer 0 (#.array-dimension-limit)))
   (%data :accessor utf8-string-data :initarg :data
          :type data)))

(defmethod print-object ((o utf8-string) stream)
  (print-unreadable-object (o stream :type t)
    (write-char #\" stream)
    (map nil (lambda (char)
               (when (or (char= char #\")
                         (char= char #\\))
                 (write-char #\\ stream))
               (write-char char stream))
         o)
    (write-char #\" stream)))

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
               (byte1 (logior #xc8 (ldb (byte 6 0) codepoint))))
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
(defun fill-vec-with-codepoint (data codepoint)
  (declare (type data data)
           (type (and fixnum (integer 0)) codepoint))
  (cond ((< codepoint #x80) ; one byte
         (fill data codepoint))
        ((< codepoint #x800) ; two byte
         (let ((byte0 (logior #xc0 (ldb (byte 5 6) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 0) codepoint))))
           (loop for i below (length data) by 2
                 do (setf (aref data      i) byte0
                          (aref data (1+ i)) byte1))))
        ((< codepoint #x10000) ; three byte
         (let ((byte0 (logior #xe0 (ldb (byte 4 12) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  0) codepoint))))
           (loop for i below (length data) by 3
                 do (setf (aref data       i) byte0
                          (aref data  (1+ i)) byte1
                          (aref data (+ i 2)) byte2))))
        ((< codepoint #x11000) ; four byte
         (let ((byte0 (logior #xf0 (ldb (byte 3 18) codepoint)))
               (byte1 (logior #x80 (ldb (byte 6 12) codepoint)))
               (byte2 (logior #x80 (ldb (byte 6  6) codepoint)))
               (byte3 (logior #x80 (ldb (byte 6  0) codepoint))))
           (loop for i below (length data) by 4
                 do (setf (aref data       i) byte0
                          (aref data  (1+ i)) byte1
                          (aref data (+ i 2)) byte2
                          (aref data (+ i 3)) byte3))))
        (t (error "BUG: Codepoint #x~x out of range" codepoint)))
  data)

(defun fill-vec-with-char (data char)
  (fill-vec-with-codepoint data (char-code char)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extensible sequences protocol
;;;

(defmethod sequence:elt ((sequence utf8-string) index)
  (let ((data (utf8-string-data sequence)))
    (get-char data (char-index data index))))

;;; This can be HELLA SLOW because it needs to allocate
;;; a new data vector if the new character isn't the same
;;; size as the old one.
(defmethod (setf sequence:elt) (new (sequence utf8-string) index)
  (let* ((data (utf8-string-data sequence))
         (byte-index (char-index data index))
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
             (let* ((vec-length (reduce #'+ initial-contents
                                        :key #'char-length))
                    (data (make-array vec-length
                                      :element-type
                                      '(unsigned-byte 8))))
               (replace (%make-utf8-string length data)
                        initial-contents))))
        (t (%make-utf8-string length))))

;; adjust-sequence

;; make-sequence-iterator uses default implementation
;; (i.e., make-simple-sequence-iterator)

;;; FIXME: This does not work. By setting iterator-element
;;; while iterating, the vector can be resized, which will
;;; alter the iterator endpoint.
(defmethod sequence:make-simple-sequence-iterator
    ((sequence utf8-string) &key from-end (start 0) end)
  (when (null end) (setf end (length sequence)))
  (let ((data (utf8-string-data sequence)))
    (if from-end
        ;; FIXME: Probably broken at the edges
        (values (char-index data (1- end))
                (char-index data (1- start))
                t)
        (values (char-index data start)
                (char-index data end)
                nil))))

(defmethod sequence:iterator-step
    ((sequence utf8-string) iterator from-end)
  (let ((data (utf8-string-data sequence)))
    (if from-end
        (prev-index data iterator)
        (next-index data iterator))))

;; iterator-endp is fine with default (= iterator limit)

(defmethod sequence:iterator-element
    ((sequence utf8-string) iterator)
  (get-char (utf8-string-data sequence) iterator))

;;; Also hella slow.
(defmethod (setf sequence:iterator-element)
    (new (sequence utf8-string) iterator)
  (let* ((data (utf8-string-data sequence))
         (byte (aref data iterator))
         (old-length (start-byte-length byte))
         (new-length (char-length new)))
    (unless (= old-length new-length)
      ;; Apocalyptically slow case: Resize.
      (setf data
            (expand-data data iterator new-length
                         (+ iterator old-length) (length data))
            (utf8-string-data sequence)
            data))
    ;; Now write in the codepoint.
    (set-char new data iterator)))

;;; iterator-index and iterator-copy are fine with defaults
;;; (i.e. returning the iterator)

;;; map, count, find, position are probably fine
;;; however, MAP /could/ allocate four bytes per character
;;; and then shrink at the end; this would save time if
;;; any character is more than one byte.

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

;;; fill, (n)substitute, replace will be fucky

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

;; concatenate will be fucky
;; reduce, mismatch, search is ok with default implementation
;; delete will be fucky
;; remove will be fucky but maybe less??
;; delete/remove-duplicates, same shit i guess
;; (stable-)sort, merge, probably bad
