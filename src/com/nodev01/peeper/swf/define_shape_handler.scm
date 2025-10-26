;;;
;;;; Define shape tag parser
;;;
;;
;; @created   "Fri Jan 18 13:18:49 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.swf.basic_types>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.utils.bit_operations>)
(require 'srfi-1)

(module-export define-shape-handler)

(define shape-version 1)
;;
;; main tag
;; 
(define (define-shape-handler tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (shape-id (read-UI16 reader))
         (shape-bounds (read-struct-rect reader))
         (shapes (shape-with-style reader)))
    (display 'shape-handler)
    (make-tag 'define-shape
              (list
               (make-field 'shape-id shape-id)
               (make-field 'shape-bounds shape-bounds)
               (make-field 'shapes shapes)))))

;;
;; Register
;;
(register-swf-handler 2 define-shape-handler)


;;
;; parse shape with style structure
;; 
(define (shape-with-style reader)
  (let* ((fillstyle-array (fill-style-array reader))
         (linestyle-array (line-style-array reader))
         (numfill-bits (bits->integer (read-n 'next-bit 4 reader)))
         (numline-bits (bits->integer (read-n 'next-bit 4 reader)))
         (shape-records (read-until-of 
                         (lambda (x) (read-shape-record x numfill-bits numline-bits))
                         (lambda (tag) (cond ((null? tag) #t)
                                             ((eq? (get-tag-id tag) 'end-shape-record) #t)
                                             (else #f)))
                         reader)))
    (display 'shape-with-style)
    (make-tag 'shape-with-style
              (list
               (make-field 'fill-styles fillstyle-array)
               (make-field 'line-styles linestyle-array)
               (make-field 'num-fill-bits numfill-bits)
               (make-field 'numline-bits numline-bits)))))

;;;
;;;; --
;;;; Fill styles
;;;
;;
;; parse fillstylearray
;; 
(define (fill-style-array reader)
  (let* ((fillstyle-count (read-UI8 reader))
         (fillstyle-count-ex (if (= fillstyle-count #xFF) (read-UI16 reader) '()))
         (s (display fillstyle-count))
         (fill-styles (read-array-of fill-style 
                                     (if (= fillstyle-count #xFF) fillstyle-count-ex fillstyle-count)
                                     reader)))
    (display 'fill-style-array)
    (make-tag 'fill-style-array
              (list
               (make-field 'fill-styles fill-styles)))))

;;
;; fill style element 
;;
(define (fill-style reader)
  (let* ((fillstyle-type (read-UI8 reader))
         (color (if (= fillstyle-type 0) (read-rgb reader)))
         (gradient-matrix (if (or (= fillstyle-type #x10)
                                  (= fillstyle-type #x12)) (read-matrix reader) '()))
         (gradient (cond ((or (= fillstyle-type #x10) (= fillstyle-type #x12)) 
                          (gradient reader))
                         ((= fillstyle-type #x13) (focal-gradient reader))
                         (else '())))
         (bitmap-id (if (or (= fillstyle-type #x40)
                               (= fillstyle-type #x41)
                               (= fillstyle-type #x42)
                               (= fillstyle-type #x43))
                        (read-UI16 reader)
                        '()))
         (bitmap-matrix (if (or (= fillstyle-type #x40)
                               (= fillstyle-type #x41)
                               (= fillstyle-type #x42)
                               (= fillstyle-type #x43))
                        (read-matrix reader)
                        '())))
    (display 'fill-style)
    (make-tag 'fill-style
              (list
               (make-field 'color color)
               (make-field 'gradient-matrix gradient-matrix)
               (make-field 'gradient gradient)
               (make-field 'bitmap-id bitmap-id)
               (make-field 'bitmap-matrix bitmap-matrix)))))
 
;;;
;;;; --
;;;; Line styles
;;;

;;
;; parse linestyle array
;; 
(define (line-style-array reader)
  (let* ((line-style-count (read-UI8 reader))
         (line-style-count (if (= line-style-count #xFF) (read-UI16 reader) line-style-count))
         (s (display line-style-count))
         (line-styles (if (or (= shape-version 1) (= shape-version 2) (= shape-version 3))
                          (read-array-of line-style line-style-count reader)
                          (read-array-of line-style2 line-style-count reader))))
    (display 'line-style-array)
    (make-tag 'line-style-array
              (list
               (make-field 'line-styles line-styles)))))

(define (line-style reader)
  (let* ((width (read-UI16 reader))
         (color (if (or (= shape-version 1) (= shape-version 2))
                    (read-rgb reader)
                    (read-rgba reader))))
    (display 'line-style)
    (make-tag 'line-style
              (list
               (make-field 'width width)
               (make-field 'color color)))))

(define (line-style2 reader)
  (let* ((width (read-UI16 reader))
         (start-cap-style (bits->integer (read-n 'next-bit 2 reader)))
         (join-style (bits->integer (read-n 'next-bit 2 reader)))
         (has-fill (bits->integer (read-n 'next-bit 1 reader)))
         (no-hscale (bits->integer (read-n 'next-bit 1 reader)))
         (no-vscale (bits->integer (read-n 'next-bit 1 reader)))
         (pixel-hinting (bits->integer (read-n 'next-bit 1 reader)))
         (reserved (read-n 'next-bit 5 reader))
         (no-close (bits->integer (read-n 'next-bit 1 reader)))
         (end-cap-style (bits->integer (read-n 'next-bit 2 reader)))
         (miter-limit-factor (if (= join-style 2) (read-fixed8 reader) '()))
         (color (if (= has-fill 0) (read-rgba reader) '()))
         (fill-type (if (= has-fill 1) (fill-style reader) '())))
    (display 'line-style2)
    (make-tag 'line-style2
              (list
               (make-field 'width width)
               (make-field 'start-cap-style start-cap-style)
               (make-field 'join-style join-style)
               (make-field 'no-hscale no-hscale)
               (make-field 'no-vscale no-vscale)
               (make-field 'pixel-hinting pixel-hinting)
               (make-field 'reserved reserved)
               (make-field 'no-close no-close)
               (make-field 'end-cap-style end-cap-style)
               (make-field 'miter-limit-factor miter-limit-factor)
               (make-field 'color color)
               (make-field 'fill-type fill-type)))))

(define (test-line-style)
  (line-style
   (makelist-bit/byte-reader
    (append
     (UI16->bytes 100)
     (rgb->bytes (make-rgb 100 101 102))))))

(define (test-line-style2)
  (line-style2
   (makelist-bit/byte-reader
    (append
     (UI16->bytes 100)
     (listbits->listbytes
      (append
       (list 0 0 1 0)
       (list 0        ;; fill flag
             0        ;; noV-scale
             0        ;; noH-scale
             1        ;; pixel-hinting
             0 0 0 0 0 ;; reserved
             1        ;; noclose
             1 0      ;; endCapStyle             
             )))
     (UI16->bytes 1)
     (rgb->bytes (make-rgb 100 101 102))))))

;;;
;;;; --
;;;; Gradient structures
;;;

;;
;; parse Gradient
;; 
(define (gradient reader)
  (let* ((spread-mode (bits->integer (read-n 'next-bit 2 reader)))
         (interpolation-mode (bits->integer (read-n 'next-bit 2 reader)))
         (num-gradients (bits->integer (read-n 'next-bit 4 reader)))
         (gradient-records (read-array-of gradient-record num-gradients reader)))
    (display 'gradient)
    (make-tag 'gradient
              (list
               (make-field 'spread-mode spread-mode)
               (make-field 'interpolation-mode interpolation-mode)
               (make-field 'gradient-records gradient-records)))))

(define (gradient-record reader)
  (let* ((ratio (read-UI8 reader))
         (color (read-rgb reader)))
    (display 'gradient-record)
    (make-tag 'gradient-record
              (list
               (make-field 'ratio ratio)
               (make-field 'color color)))))

(define (focal-gradient reader)
  (let* ((spread-mode (bits->integer (read-n 'next-bit 2 reader)))
         (interpolation-mode (bits->integer (read-n 'next-bit 2 reader)))
         (num-gradients (bits->integer (read-n 'next-bit 4 reader)))
         (gradient-records (read-array-of gradient-record num-gradients reader))
         (focal-point (read-fixed8 reader)))
    (display 'focal-gradient)
    (make-tag 'focal-gradient
              (list
               (make-field 'spread-mode spread-mode)
               (make-field 'interpolation-mode interpolation-mode)
               (make-field 'num-gradients num-gradients)
               (make-field 'gradient-records gradient-records)
               (make-field 'focal-point focal-point)))))

;;;
;;;; --
;;;; Shape records
;;;

(define (read-shape-record reader num-fill-bits num-line-bits)
  (let ((b (read-n 'next-bit 1 reader)))
    (display 'read-shape-record)
    (display b)
    (if (null? b)
        '()
        (let ((type-flag (bits->integer b)))
          (if (= type-flag 0)
              (let ((f (read-n 'next-bit 5 reader)))
                (display 'a)
                (display f)
                (if (= (bits->integer f) 0)
                    (make-tag 'end-shape-record '())
                    (style-change-record reader num-fill-bits num-line-bits f)))
              (let ((sf (read-n 'next-bit 1 reader)))
                (display 'b)
                (if (= sf 0)
                    (curved-edge-record reader)
                    (straight-edge-record reader))))))))

(define (style-change-record reader num-fill-bits num-line-bits f)
  (let* ((state-new-styles (first f))
         (state-line-style (second f))
         (state-fill-style1 (third f))
         (state-fill-style0 (fourth f))
         (state-move-to (fifth f))
         (move-bits (if (= state-move-to 1) (bits->integer (read-n 'next-bit 5 reader)) '()))
         (move-delta-x (if (= state-move-to 1) (sbits->integer (read-n 'next-bit move-bits reader)) '()))
         (move-delta-y (if (= state-move-to 1) (sbits->integer (read-n 'next-bit move-bits reader)) '()))
         (fill-style0 (if (= state-fill-style0 1) (bits->integer (read-n 'next-bit num-fill-bits reader)) '()))
         (fill-style1 (if (= state-fill-style1 1) (bits->integer (read-n 'next-bit num-fill-bits reader)) '()))
         (line-style (if (= state-line-style 1) (bits->integer (read-n 'next-bit num-line-bits reader)) '()))
         (fill-styles (if (= state-new-styles 1) (fill-style-array reader) '()))
         (line-styles (if (= state-new-styles 1) (line-style-array reader) '()))
         (num-fill-bits (if (= state-new-styles 1) (bits->integer (reader-n 'next-bit 4 reader)) '()))
         (num-line-bits (if (= state-new-styles 1) (bits->integer (reader-n 'next-bit 4 reader)) '())))
    (display 'style-change-record)
    ;(reader 'byte-align)
    (make-tag 'style-change-record
              (list
               (make-field 'move-delta-x move-delta-x)
               (make-field 'move-delta-y move-delta-y)
               (make-field 'fill-style0 fill-style0)
               (make-field 'fill-style1 fill-style1)
               (make-field 'line-style line-style)
               (make-field 'fill-styles fill-styles)
               (make-field 'line-styles line-styles)
               (make-field 'num-fill-bits num-fill-bits)
               (make-field 'num-line-bits num-line-bits)))))

(define (test-style-change-record)
  (read-shape-record
   (makelist-bit/byte-reader
    (listbits->listbytes
     (append
      (list 0 0 1 1 1 1)
      (list 0 1 0 1 0) ;; movebits
      (list 0 0 0 0 0 0 0 0 0 1) ;; deltaX
      (list 0 0 0 0 0 0 0 0 1 0) ;; deltaY
      (list 0 0 0 0 0 0 0 0 0 0) ;; fillStyle0
      (list 0 0 0 0 0 0 0 0 0 1) ;; fillStyle1
      (list 0 0 0 1 0) ;; lineStyle
      )))
   10 5))

;;;
;;;; --
;;;; Edge records
;;;

(define (straight-edge-record reader)
  (let* ((num-bits (+ 2 (bits->integer (read-n 'next-bit 4 reader))))
         (general-line-flag (bits->integer (read-n 'next-bit 1 reader)))
         (vert-line-flag (if (= general-line-flag 0) (bits->integer (read-n 'next-bit 1 reader)) '()))
         (delta-x (if (or (= general-line-flag 1)
                          (= vert-line-flag 0))
                      (sbits->integer (read-n 'next-bit num-bits reader))
                      '()))
         (delta-y (if (or (= general-line-flag 1)
                          (= vert-line-flag 1))
                      (sbits->integer (read-n 'next-bit num-bits reader))
                      '())))
    (display 'straight-edge-record)
    ;;(reader 'byte-align)
    (make-tag 'straight-edge-record
              (list
               (make-field 'delta-x delta-x)
               (make-field 'delta-y delta-y)))))

(define (curved-edge-record reader)
  (let* ((num-bits (+ 2 (bits->integer (read-n 'next-bit 4 reader))))
         (control-delta-x (sbits->integer (read-n 'next-bit num-bits reader)))
         (control-delta-y (sbits->integer (read-n 'next-bit num-bits reader)))
         (anchor-delta-x (sbits->integer (read-n 'next-bit num-bits reader)))
         (anchor-delta-y (sbits->integer (read-n 'next-bit num-bits reader))))
    (display 'curved-edge-record)
    ;;(reader 'byte-align)
    (make-tag 'curved-edge-record
              (list
               (make-field 'control-deltax control-delta-x)
               (make-field 'control-deltay control-delta-y)
               (make-field 'anchor-delta-x anchor-delta-x)
               (make-field 'anchor-delta-y anchor-delta-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-curved)
  (curved-edge-record 
   (makelist-bit/byte-reader 
    (listbits->listbytes 
     (append
      (list 0 0 1 1) ;bits
      (list 0 1 1 1 1)
      (list 0 1 1 1 0)
      (list 0 1 1 0 1)
      (list 0 1 1 0 0))))))

(define (test-straight)
  (straight-edge-record 
;; (+ 2 (bits->integer (read-n 'next-bit 4
   (makelist-bit/byte-reader
    (listbits->listbytes
     (append
      (list 0 0 1 1)
      (list 1)
      (list 0 1 1 0 0)
      (list 0 1 0 1 0))))))
