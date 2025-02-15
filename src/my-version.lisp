
(defun h2b (byte)
  "Convert 2 hex digits into a byte"
  (parse-integer byte :radix 16))

(defun hex-to-bytes (hex-str)
  (let ((len (length hex-str)))
    (if (oddp len) (error "Hex string is malformed"))
    (let ((array (make-array (floor len 2) :element-type 'unsigned-byte :fill-pointer 0 :adjustable t)))
      (do* ((start 0 (+ start 2))
            (end 2 (+ end 2))
            (digit (subseq hex-str 0 2) (when (<= end len) (subseq hex-str start end))))
           ((> end len) array)
        (vector-push-extend (h2b digit) array)))))
