;;; data-size.el --- Summary -*- lexical-binding: t; -*-
;;

;;; Commentary:



;;; Code:

(defun kb (&optional count)
  "Return the number of bytes in a kilobyte multiplied by COUNT."
  (let ((count (or count 1)))
    (* 1024 count)))

(defun mb (&optional count)
  "Return the number of bytes in a megabyte multiplied by COUNT."
  (let ((count (or count 1)))
    (* (kb 1024) count)))

(defun gb (&optional count)
  "Return the number of bytes in a megabyte multiplied by COUNT."
  (let ((count (or count 1)))
    (* (mb 1024) count)))


(provide 'data-size)


;;; data-size.el ends here