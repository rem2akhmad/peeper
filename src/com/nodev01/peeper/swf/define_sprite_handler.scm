;;;
;;;; DefineSprite tag handler
;;;
;;
;; @created   "Thu Jan 10 18:00:41 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.swf.basic_types>)

(module-export define-sprite-handler)

(define (define-sprite-handler tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (sprite-id (read-UI16 reader))
         (frame-count (read-UI16 reader))
         (control-tags (read-swf-tags reader)))
    (make-tag 'define-sprite
              (list (make-field 'sprite-id sprite-id)
                    (make-field 'frame-count frame-count)
                    (make-field 'control-tags control-tags)))))

;;;
;;;; Registration
;;;

(register-swf-handler 39 define-sprite-handler)