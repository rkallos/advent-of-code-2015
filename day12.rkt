#lang racket

(require json)

;; Simple regex parsing
(define (day-12-part-1 input)
  (apply + (map string->number (regexp-match* #rx"-?[0-9]+" (file->string input)))))

;; Recursive traversal of a jsexpr
(define (day-12-part-2 input)
  (define (ignore-red e)
    (cond
      [(number? e) e]
      [(list? e) (apply + (map ignore-red e))]
      [(hash? e) (if (member "red" (hash-values e))
                       0
                       (apply + (map ignore-red (hash-values e))))]
      [else 0]))
  (ignore-red (read-json (open-input-file input))))