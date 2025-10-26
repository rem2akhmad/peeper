;;;
;;;; reader
;;;
;;
;; @created   "Tue Jan 08 17:20:25 MSK 2008"
;;

(require <com.nodev01.peeper.utils.bit_operations>)
(require <com.nodev01.peeper.swf.basic_types>)
(require <com.nodev01.peeper.utils.zlib_java_bridge>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.io_operations>)
(require 'srfi-1)
(require 'srfi-69)

(module-export read-swf-tags)
(module-export register-swf-handler)

(module-export test-zip1)
(module-export swf-header-reader)

;;
;; list of tag handlers 
;;
(define tag-handlers (make-hash-table))

;;
;; register handler 
;;
(define (register-swf-handler id handler)
  (hash-table-set! tag-handlers id handler))

;;
;; parse data with tag hanlder 
;;
(define (apply-handler id header data)
  ((hash-table-ref/default tag-handlers id (hash-table-ref tag-handlers 'default)) header data))

;;
;; read list of tags from reader
;; 
(define (read-swf-tags in-reader)
  (define (iter-read-swf-tags in-reader tags)
    (let ((tag (read-tag in-reader)))
      (if (not (null? tag))
          (iter-read-swf-tags in-reader (cons tag tags))
          tags)))
  (reverse! (iter-read-swf-tags in-reader '())))

;;
;; read one tag from reader
;; 
(define (read-tag in-reader)
  (let ((tag-header (read-tag-header in-reader)))
    (if (null? tag-header)
        '()
        (let ((tag-body-bytes (read-n 'next-byte (tag-header-length tag-header) in-reader)))
          ;; (make-tag)
          (apply-handler (tag-header-type tag-header) tag-header tag-body-bytes)))))

;; 
;; read tag-header
;; 
(define (read-tag-header in-reader)
  (let ((lowbyte 0)
        (hibyte 0)
        (len 0)
        (bytes (read-n 'next-byte 2 in-reader)))
    (if (null? bytes)
        '()
        (begin
          (set! lowbyte (car bytes))
          (set! hibyte  (cadr bytes))
          (set! len (short-header-length lowbyte hibyte))
          (if (= len #x3F)
              (begin
                (set! bytes (read-n 'next-byte 4 in-reader))
                (if (null? bytes)
                    '()
                    (begin
                      (set! len (car bytes))
                      (set! bytes  (cdr bytes))
                      (set! len (logior len (ash (car bytes) 8)))
                      (set! bytes  (cdr bytes))
                      (set! len (logior len (ash (car bytes) 16)))
                      (set! bytes  (cdr bytes))
                      (set! len (logior len (ash (car bytes) 24)))))))
          (make-tag-header (short-header-type lowbyte hibyte) len)))))
;;
;; calcs length of short-header 
;;
(define (short-header-length lowbyte hibyte)
  (logand lowbyte #x3F))

;;
;; calcs value of short-header type
;; 
(define (short-header-type lowbyte hibyte)
  (logior (logand (ash hibyte 2) #x3FC) 
          (logand (ash lowbyte -6) #x03)))

;;
;; Reads swf header
;; 
(define (swf-header-reader in-reader)
  (begin 
    (set! sig1 ((in-reader 'next-byte)))
    (set! sig2 ((in-reader 'next-byte)))
    (set! sig3 ((in-reader 'next-byte)))
    (set! version ((in-reader 'next-byte)))
    (set! file-length (bytes->integer (read-n 'next-byte 4 in-reader)))
    (if (eq? sig1 #x43) (in-reader 'switch-to-zip))
    (set! frame-size (read-struct-rect in-reader))
    (set! frame-rate (read-n 'next-byte 2 in-reader))
    (set! frame-count (bytes->integer (read-n 'next-byte 2 in-reader)))
    (make-header (= sig1 #x43)
                 version
                 file-length
                 frame-size
                 frame-rate
                 frame-count)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests
(define (test1)
  (let ((in-file (open-binary-input-file "test-data/test1.swf")))
    (let ((reader (make-bit/byte-reader in-file)))
      (begin
        (display (swf-header-reader reader))
        (display (read-swf-tags reader))))))
;; тестирование архивированного файла
(define (test-zip1)
  (let ((in-file (open-binary-input-file "../test-data/a.swf")))
    (let ((reader (make-bit/byte-reader in-file)))
      (begin
        (display (swf-header-reader reader))
        (display 'tags)
        (display (newline))
        (for-each (lambda (x)
                    (begin (display x) (newline))) 
                  (read-swf-tags reader))
        (close-input-port in-file)))))

(define (test-reader)
  (let ((in-port (fluid-let ((port-char-encoding #f))(open-input-string (string #\252 #\02 #\360 #\04 #\05 #\06 #\07)))))
    (let ((reader (make-bit/byte-reader in-port)))
      (begin
        (display (read-n 'next-bit 8 reader))
        (display (read-n 'next-bit 8 reader))
        (display (read-n 'next-bit 4 reader))
        (display (read-n 'next-byte 2 reader))
        ))))

(define (test-bit-reader)
  (let ((in-port (fluid-let ((port-char-encoding #f)) (open-input-string (string #\252 #\02 #\360 #\04 #\05 #\06 #\07)))))
    (let ((reader (make-bit/byte-reader in-port)))
      (begin
        (display (string #\200 #\02 #\03 #\04 #\05 #\06 #\07))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-byte)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-bit)))
        (display ((reader 'next-byte)))
        (display ((reader 'next-byte)))
        (display ((reader 'next-byte)))
        (display ((reader 'next-byte)))
        (display ((reader 'next-bit)))))))


