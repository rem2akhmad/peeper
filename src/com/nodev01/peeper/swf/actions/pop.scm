;;;
;;;; Pop action handler
;;;
;;
;; @created   "Sun Jan 13 22:34:46 MSK 2008"
;;

(require <com.nodev01.peeper.swf.actions_handler>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.utils.io_operations>)
(require <com.nodev01.peeper.swf.basic_types>)

(module-export pop-action)

(define (pop-action id data)
  (make-action 'pop '()))

;;
;; Register
;;
(register-actions-handler #x17 pop-action)
