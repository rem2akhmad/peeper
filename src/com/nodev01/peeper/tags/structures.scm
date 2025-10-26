;;;
;;;; Basic structures used by sfw tags
;;;
;;
;; @created   "Wed Jan 09 19:08:34 MSK 2008"
;;


(module-export make-rect rect?)
(module-export make-point point?)
(module-export get-x)
(module-export get-y)
(module-export make-rgb get-red get-green get-blue rgb?)
(module-export make-matrix matrix?)
(module-export make-cxform-with-alpha cxform-with-alpha?)
(module-export make-rgba get-alpha rgba?)
(module-export make-cxform cxform?)
(module-export make-raw-data raw-data? get-raw-data)
;; gradients
;;(module-export make-gradient-record gradient-color gradient-ratio)
;;(module-export make-gradient gradient-interpolation-mode gradient-spread-mode gradient-records)

(module-export attach-type-tag)
(module-export type-tag)
(module-export contents)

;;
;; Raw data
;; 
(define (make-raw-data data)
  (attach-type-tag 'rawdata data))

(define (raw-data? data)
  (eq? 'rawdata (type-tag data)))

(define (get-raw-data data)
  (contents data))

;;
;; Rectangle record
;; 
(define (make-rect xmin xmax ymin ymax)
  (attach-type-tag 'rect 
                   (cons (make-point xmin ymin)
                         (make-point xmax ymax))))

;;
;; rect predicate
;; 
(define (rect? data)
  (eq? (type-tag data) 'rect))

;; Point
(define (make-point x y)
  (attach-type-tag 'point (cons x y)))

;;
;; Point predicate
;; 
(define (point? data)
  (eq? (type-tag data) 'point))

(define (get-x point)
  (car (contents point)))

(define (get-y point)
  (cdr (contents point)))

;;
;; RGB
;; 
(define (make-rgb red green blue)
  (attach-type-tag 'rgb (list red green blue)))

(define (rgb? data)
  (eq? (type-tag data) 'rgb))

(define (get-red rgb)
  (car (contents rgb)))

(define (get-green rgb)
  (cadr (contents rgb)))

(define (get-blue rgb)
  (caddr (contents rgb)))


;;
;; RGBA
;;
(define (make-rgba red green blue alpha)
  (attach-type-tag 'rgba (list red green blue alpha)))

(define (rgba? data)
  (eq? 'rgba (type-tag data)))

(define (get-alpha rgba)
  (cadddr (contents rgba)))


;;
;; MATRIX struct
;; 
(define (make-matrix scale-x scale-y rotate-skew0 rotate-skew1 translate-x translate-y)
  (attach-type-tag 'matrix (list scale-x scale-y rotate-skew0 rotate-skew1 translate-x translate-y)))

(define (matrix? data)
  (eq? 'matrix (type-tag data)))

;;
;; CXFORM struct
;; 
(define (make-cxform multi-trans-rgb add-trans-rgb)
  (attach-type-tag 'cxform (list multi-trans-rgb add-trans-rgb)))

(define (cxform? data)
  (eq? 'cxform (type-tag data)))

;;
;; CXFORMWITHALPHA struct
;; 
(define (make-cxform-with-alpha multi-trans-rgba add-trans-rgba)
  (attach-type-tag 'cxform-with-alpha (list multi-trans-rgba add-trans-rgba)))

(define (cxform-with-alpha? data)
  (eq? 'cxform-with-alpha (type-tag data)))

;;;
;;;; --
;;;; Gradients
;;;

(define (make-gradient-record ratio color)
  (attach-type-tag 'gradient-record (list ratio color)))

(define (gradient-ratio gradient-record)
  (car (contents gradient-record)))

(define (gradient-color gradient-record)
  (cadr (contents gradient-record)))

;;
;; Gradint rec
;; 
(define (make-gradient spread-mode interpolation-mode grad-records)
  (attach-type-tag 'gradient 
                   (list spread-mode
                         interpolation-mode
                         grad-records)))

(define (gradient-spread-mode gradient)
  (car (contents gradient)))

(define (gradient-interpolation-mode gradient)
  (cadr (contents gradient)))

(define (gradient-records gradient)
  (caddr (contents gradient)))

;;;
;;;; --
;;;; types
;;;

(define (attach-type-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag data)
  (if (pair? data)
      (car data)
      '()))

(define (contents data)
  (if (pair? data)
      (cdr data)
      '()))
