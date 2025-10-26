;; reqs
(require <com.nodev01.peeper.tags.structures>)
(require 'srfi-1)

(module-export make-header)
(module-export get-swf-version)
(module-export compressed?)
(module-export make-tag)
(module-export make-field)
(module-export get-tag-id)
(module-export make-action)
(module-export get-fields)
(module-export get-field-name)
(module-export get-field-value)
(module-export action->tag)
(module-export tag?)
(module-export action?)
(module-export tag-field?)

;;;
;;;; --
;;;; SWF Header
;;;


;; Creates header tag.
(define (make-header compressed version file-length frame-size frame-rate frame-count)
  (list compressed version file-length frame-size frame-rate frame-count))

;; if compressed then true else false
(define (compressed? header)
  (car header))

;; Returns version of swf
(define (get-swf-version header)
  (cadr header))

;;;; --
;;;; tags
;;;

;; 
;; creates empty-tag
;; field-list is alist
(define (make-tag tag-id fields-list)
  (attach-type-tag 'tag (cons tag-id fields-list)))

;; returns tag-id
(define (get-tag-id tag)
  (car (contents tag)))

;; returns field by name
(define (get-field tag field-name)
  (assq field-name (get-fields tag)))

(define (get-fields tag)
  (cdr (contents tag)))

;; put field to tag
;; TODO работает не верно исправить
(define (put-field tag fld)
  (cons fld (cdr (contents tag))))

;; creates action record
(define (make-action action-type fields-list)
  (attach-type-tag 'action (make-tag action-type fields-list)))

(define (action->tag act)
  (contents act))

(define (action? data)
  (eq? (type-tag data) 'action))

;; test data to tag-type
(define (tag? data)
  (eq? (type-tag data) 'tag))

;;----------------------- fields -------------------------
;;;
;;;; --
;;;; fields
;;;

;; creates tag field
(define (make-field field-name value)
  (attach-type-tag 'field (cons field-name value)))

;; returns field name
(define (get-field-name field)
  (car (contents field)))

;; returns field value
(define (get-field-value field)
  (cdr (contents field)))

(define (tag-field? data)
  (eq? (type-tag data) 'field))

