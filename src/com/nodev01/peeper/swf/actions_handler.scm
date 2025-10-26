;;;
;;;; Process list of bytes to list of actions
;;;
;;
;; @created   "Sat Jan 12 17:14:31 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.utils.bit_operations>)
(require <com.nodev01.peeper.swf.basic_types>)
(require 'srfi-1)
(require 'srfi-69)

(module-export read-actions)
(module-export register-actions-handler)

(define actions-handlers (make-hash-table))

;;
;; returns list of actions
;; 
(define (read-actions data)
  (let ((reader (makelist-bit/byte-reader data)))
    (unfold (lambda (x) (null? x))
            (lambda (x) x)
            (lambda (x) (read-action-record reader))
            (read-action-record reader))))

(define (read-action-record reader)
  (let ((action-code (read-UI8 reader)))
    (if (null? action-code)
        '()
        (let ((len (if (>= action-code #x80) (read-UI16 reader) 0)))
          (apply-actoin-handler action-code (read-n 'next-byte len reader))))))

(define (apply-actoin-handler acode data)
  ((hash-table-ref/default 
   actions-handlers
   acode
   (hash-table-ref actions-handlers 'unknown))
   acode data))

;;
;; Register function
;; 
(define (register-actions-handler id handler)
  (hash-table-set! actions-handlers id handler))