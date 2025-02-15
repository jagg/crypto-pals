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

(defun try-aref (seq n)
  (if (< n (length seq))
    (aref seq n)
    0))


(defun get-6 (byte pos)
  (when byte
    (ldb (byte 6 pos) byte)))

(defun combine-3 (bytes start end)
  "Combine a sequence of three bytes into a single integer"
  (let ((seq (subseq bytes start (min (length bytes) end))))
    (values 
     (when seq 
       (logior (ash (try-aref seq 0) 16)
               (ash (try-aref seq 1) 8)
               (ash (try-aref seq 2) 0)))
     (+ 1 (- (min (length bytes) end) start)))))

(defun b64-encode (bytes)
  (with-output-to-string (str)
    (do* ((start 0 (+ start 3))
          (end 3 (+ end 3)))
         ((>= start (length bytes)) str)
      (multiple-value-bind (combined byte-count) (combine-3 bytes start end)
        (dotimes (n byte-count)
          (princ (char *b64-lookup* (get-6 combined (- 18 (* n 6))))))
        (dotimes (n (- 4 byte-count))
          (princ #\=))))))

(b64-encode (str-to-bytes "Base64i"))

(b64-encode (h2b "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
