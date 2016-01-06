#lang racket

;; rs is a hash of registers
;; lines is the full program
;; pc is a program-counter, representing an offset from the beginning of lines
(struct sys (rs lines pc))

(define (make-sys file [part2 #f])
  (sys (hash "a" (if part2 1 0) "b" 0)
       (list->vector (file->lines file)) 0))

;; sys -> sys
;; Evaluates the lines in the sys, functionally updating registers a and b
(define (process s)
  (if (= (vector-length (sys-lines s)) (sys-pc s))
      s
      (process
       (match (vector-ref (sys-lines s) (sys-pc s))
         [(regexp "hlf ([ab])" (list _ r))
          (sys (hash-update (sys-rs s) r (λ (v) (/ v 2)))
               (sys-lines s)
               (add1 (sys-pc s)))]
         [(regexp "tpl ([ab])" (list _ r))
          (sys (hash-update (sys-rs s) r (λ (v) (* v 3)))
               (sys-lines s)
               (add1 (sys-pc s)))]
         [(regexp "inc ([ab])" (list _ r))
          (sys (hash-update (sys-rs s) r add1)
               (sys-lines s)
               (add1 (sys-pc s)))]
         [(regexp "jmp ([+-][0-9]+)" (list _ o))
          (sys (sys-rs s)
               (sys-lines s)
               (+ (sys-pc s) (string->number o)))]
         [(regexp "ji([eo]) ([ab]), ([+-][0-9]+)" (list _ p r o))
          (sys (sys-rs s)
               (sys-lines s)
               (if ((if (equal? "e" p) even? (λ (r) (= 1 r)))
                    (hash-ref (sys-rs s) r))
                   (+ (sys-pc s) (string->number o))
                   (add1 (sys-pc s))))]))))

(define (solve input [part2 #f])
  (hash-ref (sys-rs (process (make-sys input part2))) "b"))

; Part 1:
;(solve "/path/to/file")
; Part 2:
;(solve "/path/to/file" #t)