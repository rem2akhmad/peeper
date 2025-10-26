;;;
;;;; IO operations
;;;
;;
;; @created   "Tue Jan 08 11:39:23 MSK 2008"
;;
(require <com.nodev01.peeper.utils.bit_operations>)
(require <com.nodev01.peeper.utils.zlib_java_bridge>)
(require 'srfi-1)

(module-export read-n)
(module-export make-bit/byte-reader)
(module-export makelist-bit/byte-reader)
(module-export open-binary-input-file)
(module-export read-array-of)
(module-export read-rest)
(module-export read-until)
(module-export read-until-of)

;; Reads n bytes/bits from reader
;; mes - if 'next-byte reads bytes
;;       if 'next-bit  reads bits
;; n   - count
;; in-reader - reader
 (define (read-n mes n in-reader)
   (define (read-n-inter n itms)
     (cond ((= n 0) itms)
           (else
            (let ((byte ((in-reader mes))))
              (if (and (not (eof-object? byte))
                       (not (null? byte)))
                  (read-n-inter (- n 1) (cons byte itms))
                  itms)))))
   (let ((items (read-n-inter n '())))
     (if (< (length items) n)
         '()
         (reverse! items))))

;;(define (read-n mes n in-reader)
;;  (read-array-of (lambda (r) ((r mes)))
;;                 n
;;                 in-reader))

;; Создает ридер, которые может представлять поток как в виде байт так и ввиде бит
;; Читается всегда следующий байт, даже если текущий байт при битовых операциях прочитан
;; не полностью.
;; При окончании потока возвращается eof-object
(define (make-bit/byte-reader in-port)
  ;; reads byte from binary port
  ;; return integer value or eof-object
  (define (read-byte in-port)
    (let ((val (read-char in-port)))
      (if (eof-object? val)
          val
          (char->integer val))))
  ;; just create reader
  (define (make-simple-reader in-port)
    (lambda () 
      (read-byte in-port)))
  ;; creates zip-reader
  (define (make-zip-reader in-port)
    (let ((inf-reader (inflater-reader in-port)))
      (lambda ()
        (let ((byte (*:read inf-reader)))
          (if (< byte 0)
              #!eof
              byte)))))
  
  (define buffer 0)
  (define bit-counter 0)
  (define loc-reader (make-simple-reader in-port))
  ;; return next byte from port
  (define (read-next-byte)
    (set! bit-counter 0)
    (loc-reader))
  ;; return next bit from port
  (define (read-next-bit)
    (let ((hi-bit 0))
      (cond 
       ((eof-object? buffer) buffer)
       ((> bit-counter 0)
        (begin
          (set! hi-bit (ash (logand buffer #x80) -7))
          (set! buffer (ash buffer 1))
          (set! bit-counter (- bit-counter 1))
          hi-bit))
       (else 
        (begin
          (set! buffer (loc-reader))
          (set! bit-counter 8)
          (read-next-bit))))))
  (define (byte-align)
    (set! buffer 0)
    (set! bit-counter 0))
  
  (define (dispatch m)
    (cond ((eq? m 'next-bit) read-next-bit)
          ((eq? m 'next-byte) read-next-byte)
          ((eq? m 'byte-align) (byte-align))
          ((eq? m 'switch-to-zip) (set! loc-reader (make-zip-reader in-port)))
          ((eq? m 'switch-to-simple) (set! loc-reader (make-simple-reader in-port)))
          (else 
           (error "bit/byte reader" "unknown message " m))))
  dispatch)

;;
;; binary input
;; 
(define (open-binary-input-file name)
  (fluid-let ((port-char-encoding #f)) (open-input-file name)))

;;
;; Creates reader to read list of bytes 
;;
(define (makelist-bit/byte-reader lst)
  (define buffer-list lst)
  (define bit-counter 0)
  (define buffer 0)
  (define (read-next-byte)
    (if (null? buffer-list)
        '()
        (let ((b (car buffer-list)))
          (set! bit-counter 0)
          (set! buffer-list (cdr buffer-list))
          b)))
  (define (read-next-bit)
    (let ((hi-bit 0))
      (cond 
       ((null? buffer) buffer)
       ((> bit-counter 0)
        (begin
          (set! hi-bit (ash (logand buffer #x80) -7))
          (set! buffer (ash buffer 1))
          (set! bit-counter (- bit-counter 1))
          hi-bit))
       (else 
        (begin
          (set! buffer (read-next-byte))
          (set! bit-counter 8)
          (read-next-bit))))))
  
  (define (byte-align)
    (set! buffer 0)
    (set! bit-counter 0))
  
  (define (dispatch m)
    (cond ((eq? m 'next-bit) read-next-bit)
          ((eq? m 'next-byte) read-next-byte)
          ((eq? m 'byte-align) (byte-align))
          (else
           (error "makelist-bit/byte-reader" "unknown message" m))))
  dispatch)

;;
;; reads array of structures
;; func - reads structure and it arg is reader
;; 
(define (read-array-of func size reader)
  (unfold (lambda (x) (> x size))
          (lambda (x) (func reader))
          (lambda (x) (+ x 1))
          1))
;;
;; reads bytes until p has false value
;; 
(define (read-until p reader)
  (unfold p
          (lambda (x) x)
          (lambda (x) ((reader 'next-byte)))
          ((reader 'next-byte))))
;;
;; reads structures (described func) until p has false value
;; 
(define (read-until-of func p reader)
  (unfold p
          (lambda (x) x)
          (lambda (x) (func reader))
          (func reader)))
;;
;; reads rest of bytes from reader
;; 
(define (read-rest reader)
  (read-until (lambda (x) (or (null? x) (eof-object? x))) reader))
