;;;
;;;; Basic types reader
;;;
;;
;; @created   "Sat Jan 12 14:11:19 MSK 2008"
;;

(require <com.nodev01.peeper.utils.bit_operations>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.utils.io_operations>)
(require 'srfi-1)

;; exports
(module-export read-struct-rect)
(module-export make-tag-header)
(module-export tag-header-length)
(module-export tag-header-type)
(module-export read-matrix)
(module-export read-cxform)
(module-export read-cxform-with-alpha)
(module-export read-string)
(module-export read-rgb rgb->bytes)
(module-export read-rgba)
(module-export read-float32)
(module-export read-float64)
(module-export read-fixed)
(module-export read-fixed8)
(module-export read-UI16 read-UI8 read-FB16 read-UI32)
(module-export UI16->bytes)

;;
;; Reads RECT structure
;; 
(define (read-struct-rect in-reader)
  (begin
    (set! nbits (bits->integer (read-n 'next-bit 5 in-reader)))
    (set! xmin  (bits->integer (read-n 'next-bit nbits in-reader)))
    (set! xmax  (bits->integer (read-n 'next-bit nbits in-reader)))
    (set! ymin  (bits->integer (read-n 'next-bit nbits in-reader)))
    (set! ymax  (bits->integer (read-n 'next-bit nbits in-reader)))
    (make-rect xmin xmax ymin ymax)))

;;
;; creates tag header
;; 
(define (make-tag-header type-code len)
  (cons type-code len))

;; 
;; calcs header length
;; 
(define (tag-header-length tag-header)
  (cdr tag-header))

;;
;; returns type of tag header
;; 
(define (tag-header-type tag-header)
  (car tag-header))

;;
;; parse rgb struct 
;; 
(define (read-rgb in-reader)
  (let* ((red (read-UI8 in-reader))
         (green (read-UI8 in-reader))
         (blue (read-UI8 in-reader)))
    (make-rgb red green blue)))

;;
;; Represents rgb structure as a list of bytes
;; 
(define (rgb->bytes rgb)
  (list (get-red rgb) (get-green rgb) (get-blue rgb)))

;;
;; parse rgba struct
;; 
(define (read-rgba in-reader)
  (let* ((red (read-UI8 in-reader))
         (green (read-UI8 in-reader))
         (blue (read-UI8 in-reader))
         (alpha (read-UI8 in-reader)))
    (make-rgba red green blue alpha)))

;;
;; parse matrix struct
;; 
(define (read-matrix in-reader)
  (let* ((has-scale ((in-reader 'next-bit)))
         (nscale-bits (if (= has-scale 1) (bits->integer (read-n 'next-bit 5 in-reader)) '()))
         (scale-x (if (= has-scale 1) (read-FB16 nscale-bits in-reader) '()))
         (scale-y (if (= has-scale 1) (read-FB16 nscale-bits in-reader) '()))
         (has-rotate ((in-reader 'next-bit)))
         (nrotate-bits (if (= has-rotate 1) (bits->integer (read-n 'next-bit 5 in-reader)) '()))
         (rotate-skew0 (if (= has-rotate 1) (read-FB16 nrotate-bits in-reader) '()))
         (rotate-skew1 (if (= has-rotate 1) (read-FB16 nrotate-bits in-reader) '()))
         (ntranslate-bits (bits->integer (read-n 'next-bit 5 in-reader)))
         (translate-x (sbits->integer (read-n 'next-bit ntranslate-bits in-reader)))
         (translate-y (sbits->integer (read-n 'next-bit ntranslate-bits in-reader))))
    (make-matrix scale-x scale-y rotate-skew0 rotate-skew1 translate-x translate-y)))

;;
;; parse cxform
;; 
(define (read-cxform in-reader)
  (let* ((has-add-terms ((in-reader 'next-bit)))
         (has-mult-terms ((in-reader 'next-bit)))
         (nbits (bits->integer (read-n 'next-bit 4 in-reader)))
         (red-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (green-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (blue-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (red-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (green-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (blue-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '())))
    (make-cxform (make-rgb red-mult-term
                           green-mult-term
                           blue-mult-term)
                 (make-rgb red-add-term
                           green-add-term
                           blue-add-term))))
;;
;; parse cxformwithalpha
;; 
(define (read-cxform-with-alpha in-reader)
  (let* ((has-add-terms ((in-reader 'next-bit)))
         (has-mult-terms ((in-reader 'next-bit)))
         (nbits (bits->integer (read-n 'next-bit 4 in-reader)))
         (red-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (green-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (blue-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (alpha-mult-term (if (= has-mult-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (red-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (green-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (blue-add-term (if (= has-add-terms 1) (sbits->integer (read-n 'next-bit nbits in-reader)) '()))
         (alpha-add-term (if (= has-add-terms 1) (sbits-integer (read-n 'next-bit nbits in-reader)) '())))
    (make-cxform-with-alpha (make-rgba red-mult-term
                                       green-mult-term
                                       blue-mult-term
                                       alpha-mult-term)
                            (make-rgba red-add-term
                                       green-add-term
                                       blue-add-term
                                       alpha-add-term))))
;;
;; parse string structure
;; TODO convert to string
(define (read-string in-reader)
 (symbol->string
  (bytes->symbol
   (read-until (lambda (x) (= x 0)) in-reader))))

(define (bytes->symbol bytes)
  (let ((barr (make <byte[]> length: (length bytes)))
        (it 0))
    (for-each (lambda (x)
                (begin
                  (set! (barr it) x)
                  (set! it (+ it 1))))
                bytes)
    (<java.lang.String> barr "UTF-8")))

(define (read-UI16 reader)
  (bytes->integer (read-n 'next-byte 2 reader)))

(define (UI16->bytes val)
  (let ((l (integer->bytes val)))
    (if (< (length l) 2)
        (list (car l) 0)
        l)))

(define (read-UI8 reader)
  ((reader 'next-byte)))

(define (read-FB16 n reader)
  (fbits->real (read-n 'next-bit n reader) 16))

(define (read-UI32 reader)
  (bytes->integer (read-n 'next-byte 4 reader)))


;;
;; parse float value 32-bit IEEE single-precision little-endian
;; 
(define (read-float32 in-reader)
  (bytes->float32 (read-n 'next-byte 4 in-reader)))

(define (read-float64 in-reader)
  (bytes->float64 (read-n 'next-byte 8 in-reader)))

;;
;; parse fixed values
(define (read-fixed8 in-reader)
  (let* ((low (read-n 'next-bit 8 in-reader))
         (hi (read-n 'next-bit 8 in-reader)))
    (fbits->real (append hi low) 8)))

(define (read-fixed in-reader)
  (let* ((l1 (read-n 'next-bit 8 in-reader))
         (l2 (read-n 'next-bit 8 in-reader))
         (l3 (read-n 'next-bit 8 in-reader))
         (l4 (read-n 'next-bit 8 in-reader)))
    (fbits->real (append l4 l3 l2 l1) 16)))


