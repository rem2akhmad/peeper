;;;
;;;; Unknown action handler
;;;
;;
;; @created   "Sat Jan 12 19:20:14 MSK 2008"
;;

(require <com.nodev01.peeper.swf.actions_handler>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)

(module-export unknown-acton-handler)

(define (unknown-acton-handler id data)
  (make-action 'unknown 
               (make-field 'raw (make-raw-data data))))

(register-actions-handler 'unknown unknown-acton-handler)