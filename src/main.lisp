(defpackage crypto-pals
  (:use :cl :ironclad)
  (:local-nicknames (:ic :ironclad)))
(in-package :crypto-pals)

(defun byte-n (bytes n)
  (aref bytes n))

(defun str-to-bytes (str)
  (map 'vector (lambda (c) (char-code c)) str))

(defun bytes-to-str (bytes)
  (map 'string (lambda (c) (code-char c)) bytes))

(defun bytes-to-hex (bytes)
  (with-output-to-string (s)
    (map nil (lambda (b) (format s "~2,'0X" b)) bytes)))


(defun b2h (bytes)
  (ic:byte-array-to-hex-string bytes))

(defun h2b (hex-str)
  (ic:hex-string-to-byte-array hex-str))

(defvar *b64-lookup* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(defvar *b64-reverse-lookup*
  (cdr 
   (reduce (lambda (acc c)
             (let ((pos (car acc))
                   (table (cdr acc)))
               (setf (gethash c table) pos)
               (cons (+ 1 pos) table)))
           *b64-lookup*
           :initial-value (cons 0 (make-hash-table)))))

(defun try-aref (seq n)
  (if (< n (length seq))
      (if (aref seq n) (aref seq n) 0)
      0))


(defun get-n-bits (n byte pos)
  "Get n bits from byte, starting at pos"
  (when byte
    (ldb (byte n pos) byte)))

(defun combine-3 (bytes start end)
  "Combine an array of three bytes into a single integer"
  (let ((seq (subseq bytes start (min (length bytes) end))))
    (values 
     (when seq 
       (logior (ash (try-aref seq 0) 16)
               (ash (try-aref seq 1) 8)
               (ash (try-aref seq 2) 0)))
     (+ 1 (- (min (length bytes) end) start)))))

(defun b64-encode (bytes)
  "Base64 Encode an array of bytes into a string"
  (with-output-to-string (str)
    (do* ((start 0 (+ start 3))
          (end 3 (+ end 3)))
         ((>= start (length bytes)) str)
      (multiple-value-bind (combined byte-count) (combine-3 bytes start end)
        (dotimes (n byte-count)
          (princ (char *b64-lookup* (get-n-bits 6 combined (- 18 (* n 6))))))
        (dotimes (n (- 4 byte-count))
          (princ #\=))))))

(defun merge-6bit-group (bytes start)
  (values 
   (logior (ash (try-aref bytes start) 18)
           (ash (try-aref bytes (+ 1 start)) 12)
           (ash (try-aref bytes (+ 2 start)) 6)
           (ash (try-aref bytes (+ 3 start)) 0))
   (- (min (+ 3 start) (length bytes)) start)))


(defun b64-decode (str)
  "Base64 decode a base64 string into an array of bytes"
  (let ((out (make-array (/ (* (length str) 6) 8)))
        (pos -1)
        (bytes (map 'vector (lambda (c) (gethash c *b64-reverse-lookup*)) str)))
    (do* ((start 0 (+ start 4))
          (end (min (length bytes) 4) (min (length bytes) (+ end 4))))
         ((>= start (length bytes)) (subseq out 0 (+ 1 pos)))      
      (multiple-value-bind (combined byte-count) (merge-6bit-group bytes start)
        (dotimes (n byte-count)
          (let ((char (get-n-bits 8 combined (- 16 (* n 8)))))
            (when (not (zerop char)) (setf (aref out (incf pos)) char))))))))


(b64-encode (str-to-bytes "Base64i"))

(bytes-to-str (b64-decode "QmFz"))

(b64-encode (h2b "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
(bytes-to-str (b64-decode (str-to-bytes "QmFz")))
