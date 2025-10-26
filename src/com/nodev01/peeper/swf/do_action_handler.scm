;;;
;;;; DoAction handler
;;;
;;
;; @created   "Sat Jan 12 16:53:05 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.swf.basic_types>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.swf.actions_handler>)

(module-export do-action-handler)

(define (do-action-handler tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (actions (read-actions 
                   (read-n 'next-byte (- (tag-header-length tag-header) 1) reader)))
         (end-flag (read-UI8 reader)))
    (make-tag 'do-action
              (list (make-field 'actions actions)))))


;;
;; Register
;; 
(register-swf-handler 12 do-action-handler)