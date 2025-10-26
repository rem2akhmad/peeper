;;;
;;;; SetBackgroundColor tag handler
;;;
;;
;; @created   "Sat Jan 12 16:01:38 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.swf.basic_types>)
(require <com.nodev01.peeper.utils.io_operations>)

(module-export set-background-color)

(define (set-background-color tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (color (read-rgb reader)))
    (make-tag 'set-background-color
              (list (make-field 'background-color color)))))


;;
;; Register
;; 

(register-swf-handler 9 set-background-color)