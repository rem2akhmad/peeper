;;;
;;;; Push actions
;;;
;;
;; @created   "Sun Jan 13 12:45:27 MSK 2008"
;;

(require <com.nodev01.peeper.swf.actions_handler>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.swf.basic_types>)

(module-export push-action)

(define (push-action id data)
  (let ((reader (makelist-bit/byte-reader data)))
    (make-action 'push
                 (read-until-of read-type-value-pair
                                (lambda (x) (null? x))
                                reader))))


(define (read-type-value-pair reader)
  (let ((type (read-UI8 reader)))
    (cond ((= type 0) (make-field 'string (read-string reader)))
          ((= type 1) (make-field 'float (read-float32 reader)))
          ((= type 4) (make-field 'register-number (read-UI8 reader)))
          ((= type 5) (make-field 'boolean (read-UI8 reader)))
          ((= type 6) (make-field 'double (read-float64 reader)))
          ((= type 7) (make-field 'integer (read-UI32 reader)))
          ((= type 8) (make-field 'constant8 (read-UI8 reader)))
          ((= type 9) (make-field 'constant19 (read-UI16 reader)))
          (else '()))))


;;
;; Register
;; 
(register-actions-handler #x96  push-action)