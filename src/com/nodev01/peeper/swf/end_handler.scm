;;;
;;;; End tag handler
;;;
;;
;; @created   "Wed Jan 09 14:43:07 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(module-export end_handler)

(define (end_handler tag-header data)
  (make-tag 'end-tag '()))

(register-swf-handler 0 end_handler)