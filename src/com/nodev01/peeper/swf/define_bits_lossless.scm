;;;
;;;; DefineBitsLossless tag handler
;;;
;;
;; @created   "Wed Jan 09 16:18:13 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.utils.bit_operations>)
(require 'srfi-1)

(module-export define-bitslossless)

;;
;; 
;; 
(define (define-bitslossless tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (character-id  (read-UI16 reader))
         (bitmap-format (read-UI8 reader))
         (bitmap-width  (read-UI16 reader))
         (bitmap-height (read-UI16 reader))
         (bitmap-colortable-size (if (= bitmap-format 3)
                                     (read-UI8 reader)
                                     '()))
         ;; TODO parse data
         (zlib-bitmap-data (cond ((= bitmap-format 3) (cons 'colormap-data (read-rest reader)))
                                 ((or (= bitamp-format 4) (= bitmap-format 5) 'bitmap-data))
                                 (else 'unknown-data))))
    (make-tag 'define-bits-lossless
              (list (make-field 'character-id  character-id)
                    (make-field 'bitmap-format bitmap-format)
                    (make-field 'bitmap-width  bitmap-width)
                    (make-field 'bitmap-height bitmap-height)
                    (if (not (null? bitmap-format)) (make-field 'bitmap-colortable-size bitmap-colortable-size))
                    (make-field 'zlib-bitmap-data zlib-bitmap-data)))))


;;
;; COLORMAPDATA parser
;; 
(define (parse-colormapdata reader colortable-size image-data-size)
  (let* ((color-table-rgb (make-field 'color-table-rgb
                                      (read-array-of 
                                       (lambda (r)
                                         (begin
                                           (set! red ((r 'next-byte)))
                                           (set! green ((r 'next-byte)))
                                           (set! blue ((r 'next-byte)))
                                           (make-rgb red green blue)))
                                       colortable-size
                                       reader)))
         (colormap-pixel-data (make-field 'colormap-pixel-data
                                          (read-array-of read-UI8
                                                         image-data-size
                                                         reader))))
    (make-tag 'color-map-data
              (list color-table-rgb colormap-pixel-data))))



;;(define (parse-bitmap-data reader ))

;;;
;;;; Registration
;;;

(register-swf-handler 20 define-bitslossless)

