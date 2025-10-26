;;;
;;;; Main
;;;
;;
;; @created   "Sat Jan 12 19:25:29 MSK 2008"
;;
;;;
;;;; --
;;;; Tags
;;;


(require <com.nodev01.peeper.swf.reader>)
(require <com.nodev01.peeper.swf.unknown_handler>)
(require <com.nodev01.peeper.swf.show_frame_handler>)
(require <com.nodev01.peeper.swf.end_handler>)
(require <com.nodev01.peeper.swf.define_bits_lossless>)
(require <com.nodev01.peeper.swf.define_sprite_handler>)
(require <com.nodev01.peeper.swf.place_object2_handler>)
(require <com.nodev01.peeper.swf.set_background_color_handler>)
(require <com.nodev01.peeper.swf.do_action_handler>)
(require <com.nodev01.peeper.swf.define_shape_handler>)

;;;
;;;; --
;;;; Actions
;;;
(require <com.nodev01.peeper.swf.actions.unknown_action>)
(require <com.nodev01.peeper.swf.actions.push>)
(require <com.nodev01.peeper.swf.actions.pop>)

;;;
;;;; --
;;;; TXT formatters
;;;
(require <com.nodev01.peeper.txt.writer>)
;(require <com.nodev01.peeper.txt.generic_formatter>)
;(require <com.nodev01.peeper.txt.unknown_formatter>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require <com.nodev01.peeper.utils.io_operations>)
(define (txt-out)
  (let ((in-file (open-binary-input-file "../test-data/a.swf")))
    (let ((reader (make-bit/byte-reader in-file)))
      (begin
        (display (swf-header-reader reader))
        (display 'tags)
        (display (newline))
        (txt-formatter (read-swf-tags reader) "")
        (close-input-port in-file)))))

(txt-out)