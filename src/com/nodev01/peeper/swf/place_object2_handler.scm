;;;
;;;; PlaceObject2 handler
;;;
;;
;; @created   "Thu Jan 10 22:25:07 MSK 2008"
;;

(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.tags.tags>)
(require <com.nodev01.peeper.tags.structures>)
(require <com.nodev01.peeper.swf.basic_types>)
(require <com.nodev01.peeper.utils.io_operations>)

(module-export place-object2-handler)

(define (place-object2-handler tag-header data)
  (let* ((reader (makelist-bit/byte-reader data))
         (has-clip-actions ((reader 'next-bit)))
         (has-clip-depth ((reader 'next-bit)))
         (has-name ((reader 'next-bit)))
         (has-ratio ((reader 'next-bit)))
         (has-color-trans ((reader 'next-bit)))
         (has-matrix ((reader 'next-bit)))
         (has-character ((reader 'next-bit)))
         (flag-move ((reader 'next-bit)))
         (depth (read-UI16 reader))
         (character-id (if (= has-character 1) (read-UI16 reader) '()))
         (matrix (if (= has-matrix 1) (read-matrix reader) '()))
         (color-transform (if (= has-color-trans 1) (read-cxform-with-alpha reader) '()))
         (ratio (if (= has-ratio 1) (read-UI16 reader) '()))
         (name (if (= has-name 1) (read-string reader) '()))
         (clip-depth (if (= has-clip-depth 1) (read-UI16 reader) '()))
         (clip-actoins (if (= has-clip-actions 1) 'clip-actoins '())))
    (make-tag 'place-object2
              (list
               (make-field 'flag-move flag-move)
               (make-field 'depth depth)
               (make-field 'character-id character-id)
               (make-field 'matrix matrix)
               (make-field 'color-transform color-transform)
               (make-field 'ratio ratio)
               (make-field 'name name)
               (make-field 'clip-depth clip-depth)
               (make-field 'clip-actions clip-actoins)))))

;;
;; register
;; 
(register-swf-handler 26 place-object2-handler)