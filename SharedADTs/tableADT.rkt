#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Table ADT
;;
;; It is just an allias for hash table
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide make-table
         table-get-element
         table-add-element
         table-all-values)


(define make-table make-hash)

(define (table-get-element table key)
  (if (hash-has-key? table key)
      (hash-ref table key)
      #f))

(define table-add-element hash-set!)

(define table-all-values hash-values)