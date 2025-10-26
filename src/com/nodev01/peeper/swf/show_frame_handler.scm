;;;
;;;; ShowFrame tag handler
;;;
;;
;; @created   "Wed Jan 09 13:55:46 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(module-export show-frame-handler)

(define (show-frame-handler tag-header data)
  (make-tag 'show-frame '()))

(register-swf-handler 1 show-frame-handler)