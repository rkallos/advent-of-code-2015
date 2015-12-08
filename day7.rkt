#lang racket

;; Read lines into a mutable hash table
;; (key = wire, value = number? or listof string?)
(define (read-lines ht lines)
  (cond [(empty? lines) ht]
        [else (let* ([tokens (string-split (car lines) " ")]
                     [val (reverse (drop (reverse tokens) 2))])
                (begin (hash-set! ht (last tokens)
                                  (if (= (length val) 1)
                                      (if (string->number (car val))
                                          (string->number (car val))
                                          (car val))
                                      val))
                       (read-lines ht (cdr lines))))]))

;; Recursively process wire a's dependencies using pattern matching
(define (process-key hash key)
  (if (string->number key)
      (string->number key)
      (match (hash-ref hash key)
        [(? number? a) a]
        [(? string? a)
         (begin (hash-set! hash key (process-key hash a))
                (hash-ref hash key))]
        [(list "NOT" a)
         (begin (hash-set! hash key (bitwise-not (process-key hash a)))
                (hash-ref hash key))]
        [(list a "AND" b)
         (begin (hash-set! hash key (bitwise-and (process-key hash a) (process-key hash b)))
                (hash-ref hash key))]
        [(list a "OR" b)
         (begin (hash-set! hash key (bitwise-ior (process-key hash a) (process-key hash b)))
                (hash-ref hash key))]
        [(list a "LSHIFT" b)
         (begin (hash-set! hash key (arithmetic-shift (process-key hash a)
                                                      (string->number b)))
                (hash-ref hash key))]
        [(list a "RSHIFT" b)
         (begin (hash-set! hash key (arithmetic-shift (process-key hash a)
                                                      (- 0 (string->number b))))
                (hash-ref hash key))])))

(define (day-7-part-1 input wire)
  (process-key (read-lines (make-hash) (file->lines input)) wire))

(define (day-7-part-2 input)
  (let* ([lines (file->lines input)]
         [result (process-key (read-lines (make-hash) lines) "a")]
         [ht (read-lines (make-hash) lines)])
    (begin
      (hash-set! ht "b" result)
      (process-key ht "a"))))