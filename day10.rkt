#lang racket

;; Much nicer implementation than mine, taken gratefully from Rosetta Code
(define (look-and-say str)
  (regexp-replace* #px"(.)\\1*" str (lambda (m c) (~a (string-length m) c))))

(define (day-10-part-1 input n)
  (cond [(= 0 n) (string-length input)]
        [else (day-10-part-1 (look-and-say input) (sub1 n))]))