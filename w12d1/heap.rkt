#lang racket

; initial code from w12d1 assignment post
; implemented generate-id, read-item, create-item
;             update-item, delete-item

(require "util.rkt")

(define
    heap
    '()
)

(define
    heap_counter 0
  )

(define (key->number key)
  (let ((skey (symbol->string key)))
    (if (string-prefix? skey "#")
        (string->number (substring skey 1))
        (error (string-append "malformed key " skey)))))

;function generating new id based on heap_counter
(define (generate-id)
  (set! heap_counter (+ heap_counter 1))
  (string->symbol (format "#~a" heap_counter)))

;function read-item, return the value based on the id
(define (read-item key)
  (if (> (key->number key) heap_counter)
      (error (format "key ~a is larger than heap_counter." key))
      (let iter ((lheap heap))
        (cond
          [(eq? lheap null)]
          [(eq? (caar lheap) key)
            (if (eq? (cadar lheap) '|#empty|)
                (error (format "key ~a has been deleted." key))
                (cadar lheap))]
          [else (iter (cdr lheap))]))))

;function create-item.
;1. generate a new id.
;2 link the id with the value
(define (create-item pair)
  (let ((id (generate-id)))
    (set! heap (cons (list id pair) heap))
    id))

;function update-item.
;1. create a the pair with old id and the new value.
;2. assign the new heap including with the updates
(define (update-item key val)
  (if (> (key->number key) heap_counter)
      (error (format "key ~a is larger than heap_counter." key))
      (set! heap
            (let iter ((lheap heap))
              (if (eq? (caar lheap) key)
                  (if (eq? (cadar lheap) '|#empty|)
                      (error (format "key ~a has been deleted." key))
                      (cons (list key val) (cdr lheap)))
                  (cons (car lheap) (iter (cdr lheap))))))))

;function delete-item.
;1. create the pair with old id, and '|empty|.
;2 set the heap
(define (delete-item key)
  (update-item key '|#empty|))

(provide (all-defined-out))
