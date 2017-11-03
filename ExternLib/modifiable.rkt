#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                        Modifiable Heaps                         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (heap)
 (export new from-scheme-vector full? empty? insert! delete! peek-at touch-at! peek length)
 (import (except (rnrs base) length)
         (srfi :9)
         (rnrs mutable-pairs))
 
  (define-record-type heap
   (make v s l)
   heap?
   (v storage storage!)
   (s size size!)
   (l lesser))
 
 (define (sift-up heap idx notify)
   (let 
       ((vector-ref 
         (lambda (v i)
           (vector-ref v (- i 1))))
        (vector-set!
         (lambda (v i a)
           (vector-set! v (- i 1) a)
           (notify i a)))
        (vector (storage heap))
        (size (length heap))
        (<<? (lesser heap)))
     (let sift-iter
       ((child idx)
        (element (vector-ref vector idx)))
       (let ((parent (div child 2)))
         (cond ((= parent 0)
                (vector-set! vector child element))
               ((<<? element (vector-ref vector parent))
                (vector-set! vector child (vector-ref vector parent))
                (sift-iter parent element))
               (else
                (vector-set! vector child element)))))))
 
 (define (sift-down heap idx notify)
   (let 
       ((vector-ref 
         (lambda (v i)
           (vector-ref v (- i 1))))
        (vector-set!
         (lambda (v i a)
           (vector-set! v (- i 1) a)
           (notify i a)))
        (vector (storage heap))
        (size (length heap))
        (<<? (lesser heap)))
     (let sift-iter
       ((parent idx)
        (element (vector-ref vector idx)))
       (let* 
           ((childL (* 2 parent))
            (childR (+ (* 2 parent) 1))
            (smallest
             (cond 
               ((< childL size)
                (if (<<? (vector-ref vector childL) 
                         (vector-ref vector childR))
                  (if (<<? element (vector-ref vector childL))
                    parent
                    childL)
                  (if (<<? element (vector-ref vector childR))
                    parent
                    childR)))
               ((= childL size)
                (if (<<? element (vector-ref vector childL))
                  parent
                  childL))
               (else parent))))
         (if (not (= smallest parent))
           (begin (vector-set! vector parent (vector-ref vector smallest))
                  (sift-iter smallest element))
           (vector-set! vector parent element))))))
 
 (define (from-scheme-vector vector <<?)
   (define size (vector-length vector))
   (define heap (make vector size <<?))
   (define (iter index)
     (sift-down heap index)
     (if (> index 1)
       (iter (- index 1))))
   (iter (div size 2))
   heap)
 
 (define (new capacity <<?)
   (make (make-vector capacity) 0 <<?))
 
 (define (full? heap)
   (= (vector-length (storage heap))
      (size heap)))
 
 (define (empty? heap)
   (= (size heap) 0))
 
 (define (peek-at heap index)
   (define vector (storage heap))
   (vector-ref vector (- index 1)))
 
 (define (touch-at! heap index notify)
   (define <<? (lesser heap))
   (define vector (storage heap))
   (define parent (div index 2))
   (define size (length heap))
   (cond ((= index 1)
          (sift-down heap index notify))
         ((= index size)
          (sift-up heap index notify))
         ((<<? (vector-ref vector (- parent 1))
               (vector-ref vector (- index 1)))
          (sift-down heap index notify))
         (else
          (sift-up heap index notify))))
 
 (define (insert! heap item notify)
   (if (full? heap)
     (error "heap full" heap))
   (let* ((vector (storage heap))
          (size (length heap)))
     (vector-set! vector size item)
     (if (> size 0)
       (sift-up heap (+ size 1) notify)
       (notify 1 item))
     (size! heap (+ size 1))))
 
 (define (delete! heap notify)
   (if (empty? heap)
     (error "heap empty" heap))
   (let* ((vector (storage heap))
          (size (length heap))
          (first (vector-ref vector 0))
          (last (vector-ref vector (- size 1))))
     (size! heap (- size 1))
     (if (> size 1)
       (begin 
         (vector-set! vector 0 last)
         (sift-down heap 1 notify))
       (notify 1 last))
     first))
 
 (define (peek heap)
   (if (empty? heap)
     (error "heap empty" heap))
   (vector-ref (storage heap) 0))
 
  (define (length heap)
   (size heap)))
