;; Provide bridge to java package java.util.zip
(module-export inflater-reader)

;; decompress data from in-port
(define (inflater-reader in-port)
  (<java.util.zip.InflaterInputStream> (port->java-inputstream in-port)))

;; maps port to java interface InputStream
(define (port->java-inputstream in-port)
  (<com.nodev01.peeper.utils.PortToInputStream> in-port))

;; reads byte from binary port
;; return integer value or eof-object
(define (read-byte in-port)
  (let ((val (read-char in-port)))
    (if (eof-object? val)
        val
        (char->integer val))))