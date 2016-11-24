#lang racket
 (require racket/tcp)
(define lstr (tcp-listen 8003 4 #f "127.0.0.1"))


(define IN '())
(define OUT '())
(let-values ([(x y) (tcp-accept lstr)])
  (set! IN x)
  (set! OUT y))





;(define x )
;(when (tcp-accept-ready? lstr)
 ; (display "ready"))

;(tcp-close lstr)
;(tcp-addresses lstr)