(defpackage crypto-pals
  (:use :cl :ironclad :encodings)
  (:local-nicknames (:ic :ironclad)
                    (:enc :encodings)))
(in-package :crypto-pals)

(defun test ()
  (enc:str-to-bytes "hi"))
