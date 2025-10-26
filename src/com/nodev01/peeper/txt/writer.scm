;;;
;;;; Write formatted text
;;;
;;
;; @created   "Mon Jan 14 13:53:15 MSK 2008"
;;

(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.bit_operations>)
(require 'srfi-1)
(require 'srfi-69)

(module-export txt-formatter)

;;
;; Format swf-tags structure
;; 
(define (txt-formatter list-of-tags indent)
  (for-each 
   (lambda (tag) (generic-formatter tag indent))
   list-of-tags))

(define (generic-formatter tag indent)
  (cond ((null? tag) (display ""))
        ((tag? tag) (tag-formatter tag indent))
        ((action? tag) (action-formatter tag indent))
        ((tag-field? tag) (field-formatter tag indent))
        ((list? tag) (txt-formatter tag (string-append "  " indent)))
        (else (display 
               (format "~a~a " indent tag)))))

(define (action-formatter action indent)
  (let ((tag (action->tag action)))
    (begin
      (display
       (format "~aaction ~a~%" indent (get-tag-id tag)))
      (generic-formatter (get-fields tag) indent))))

(define (tag-formatter tag indent)
  (begin
    (if (eq? (get-tag-id tag) 'unknown)
        (display (format "~atag ~s~%" indent tag))
        (begin
          (display (format "~atag ~a~%" indent (get-tag-id tag)))
          (generic-formatter (get-fields tag) indent)))))

(define (field-formatter fld indent)
  (begin
    (display
     (format "~afield ~a: " indent (get-field-name fld)))
    (let ((val (get-field-value fld)))
      (cond ((null? val) (newline))
            ((raw-data? val) (display (format "~s~%" val)))
            ((rgb? val) (display (format "~a~%" val)))
            ((rgba? val) (display (format "~a~%" val)))
            ((matrix? val) (display (format "~a~%" val)))
            ((cxform? val) (display (format "~a~%" val)))
            ((cxform-with-alpha? val) (display (format "~a~%" val)))
            ((list? val) (newline)
                         (generic-formatter val indent))
            (else (display (format "~a~%" val)))))))

;;;; tests
(define (test1)
  (generic-formatter 
   (make-tag 'my-tag 
             (list 
              (make-field 'field1 1)
              (make-field 'field2 2)
              (make-field 'field2 (list (make-action 'abc '()))))) ""))
