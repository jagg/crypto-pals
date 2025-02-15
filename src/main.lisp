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


(defun merge-3 (bytes)
  (when bytes
    (logior (ash (try-aref bytes 0) 16)
            (ash (try-aref bytes 1) 8)
            (ash (try-aref bytes 2) 0))))

(defun get-6 (byte pos)
  (when byte
    (ldb (byte 6 pos) byte)))

(defun val-or-padding (byte n)
  (if (> n 0) #\=
      (char *b64-lookup* byte)))

;; https://b64encode.com/blog/base64-algorithm/
(defun b64-encode (bytes)
  (with-output-to-string (str)
    (do* ((start 0 (+ start 3))
          (end 3 (+ end 3))
          (padding (rem (length bytes) 3))
          (seq (subseq bytes start end) (when (< start (length bytes)) (subseq bytes start (min (length bytes) end))))
          (byte (merge-3 seq) (merge-3 seq))
          (b1 (get-6 byte 18) (get-6 byte 18))
          (b2 (get-6 byte 12) (get-6 byte 12))
          (b3 (get-6 byte 6) (get-6 byte 6))
          (b4 (get-6 byte 0) (get-6 byte 0)))
         ((>= start (length bytes)) str)
      (princ (val-or-padding b1 (- padding 4)) str)
      (princ (val-or-padding b2 (- padding 3)) str)
      (princ (val-or-padding b3 (- padding 2)) str)
      (princ (val-or-padding b4 (- padding 1)) str))))

(b64-encode (str-to-bytes "Base64i"))

(b64-encode (h2b "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
