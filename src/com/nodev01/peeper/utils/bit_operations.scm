;; (provide 'bit-operations)
(require 'list-lib)

;; exports
(module-static #t)
(module-export bits->integer integer->bits)
(module-export bytes->integer integer->bytes)
(module-export sbits->integer integer->sbits)
(module-export fbits->real real->fbits)
(module-export bytes->float32 float32->bytes)
(module-export bytes->float64 float64->bytes)
(module-export listbits->listbytes)
;; -----------------------------------------

;; converts list of bits to integer
(define (bits->integer list-of-bits)
  (fold (lambda(x y) 
          (+ x (* 2 y))) 
        0 list-of-bits))

(define (integer->bits int-value)
  (unfold-right zero?
                (lambda (s) (modulo s 2))
                (lambda (s) (quotient s 2))
                int-value))

;; converts list of singned bits to integer
(define (sbits->integer list-of-bits)
  (if (null? list-of-bits)
      0
      (let ((len (length list-of-bits))
            (val (bits->integer list-of-bits)))
        (if (= 1 (car list-of-bits))
            (- val (ash 1 len))
            val))))

(define (integer->sbits int-value)
  (if (< int-value 0)
      (cons 1 (bitadd1 (invert (integer->bits int-value))))
      (cons 0 (integer->bits int-value))))

(define (bitadd1 list-of-bits)
  (define (bitadd lob c)
    (if (null? lob)
        (if (= c 1)
            (cons 1 '())
            '())
        (let ((s (+ (car lob) c)))
          (cons (modulo s 2) (bitadd (cdr lob) (quotient s 2))))))
  (reverse! (bitadd (reverse list-of-bits) 1)))

(define (invert list-of-bits)
  (map (lambda (x) 
         (if (= x 1) 0 1))
       list-of-bits))

;; converts fixed-bit value to real
(define (fbits->real list-of-bits frac-size)
  (let ((val (sbits->integer list-of-bits)))
    (/ val (ash 1 frac-size))))

(define (real->fbits real-value frac-size)
  (integer->sbits 
   (inexact->exact
    (floor
     (* real-value (ash 1 frac-size))))))

;; converts list of bytes to integer (list of bytes in little-endian order)
(define (bytes->integer list-of-bytes)
  (fold-right (lambda(x y)
                (+ x (* 256 y)))
              0 list-of-bytes))

(define (integer->bytes int-value)
  (unfold zero?
          (lambda (s) (modulo s 256))
          (lambda (s) (quotient s 256))
          int-value))

;; converts list of bytes to float (bytes in little-endian order)
(define (bytes->float32 list-of-bytes)
  (java.lang.Float:intBitsToFloat (bytes->integer list-of-bytes)))

(define (float32->bytes float-value)
  (integer->bytes (java.lang.Float:floatToIntBits float-value)))

(define (bytes->float64 list-of-bytes)
  (java.lang.Double:longBitsToDouble (bytes->integer list-of-bytes)))

(define (float64->bytes float-value)
  (integer->bytes (java.lang.Double:doubleToLongBits float-value)))

;; converts list of bits to list of bytes
(define (listbits->listbytes lst)
  (define (add-drop n lzt)
    (append lzt (make-list (- n (remainder (length lzt) n)) 0)))
  
  (define (split-by n lzt)
    (if (null? lzt)
        '()
        (cons (my-take lzt n)
              (split-by n (my-drop lzt n)))))
  (map bits->integer (split-by 8 (add-drop 8 lst))))

(define (my-take lis k)
  (let recur ((lis lis) (k k))
    (if (or (zero? k) (null? lis)) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (my-drop lis k)
  (let iter ((lis lis) (k k))
    (if (or (zero? k) (null? lis)) lis (iter (cdr lis) (- k 1)))))
