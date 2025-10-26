;;;
;;;; Unknown tag handler
;;;
;;
;; @created   "Wed Jan 09 13:55:12 MSK 2008"
;;


(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(module-export unknown-hander)

(define (unknown-hander tag-header data)
  (make-tag 'unknown 
            (list 
             (make-field 'raw (make-raw-data (cons tag-header data))))))

;; register handler
(register-swf-handler 'default unknown-hander)