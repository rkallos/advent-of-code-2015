#lang racket
(require racket/hash)

(define (process-file input)
  (let loop ([f (file->lines input)]
             [repls '()])
    (cond [(not (member "=>" (string-split (car f)))) (values repls (last f))]
          [else (loop (cdr f) (cons (remove "=>" (string-split (car f))) repls))])))

(define (apply-replacement chem repl [start-pos 0])
  (string-append (substring chem 0 start-pos)
                 (regexp-replace (regexp (first repl)) (substring chem start-pos) (second repl))))

(define (replace-molecules chem repl)
  (let loop ([h (hash)]
             [matches (regexp-match-positions* (regexp (first repl)) chem)])
    (cond [(or (empty? matches)
               (equal? matches #f)) h]
          [else (let ([r (apply-replacement chem repl (car (car matches)))])
                  (loop (hash-update h r add1 0)
                        (cdr matches)))])))

(define (day-19-part-1 input)
  (define-values (repls chem) (process-file input))
  (let loop ([r repls]
             [h (hash)])
    (cond [(empty? r) (hash-count h)]
          [else (loop (cdr r)
                      (hash-union (replace-molecules chem (car r)) h
                                  #:combine/key (lambda (k a b) (+ a b))))])))

(define (day-19-part-2 input)
  (define-values (repls chem) (process-file input))
  (- (length (regexp-match* #rx"[A-Z][a-z]?" chem))
     (length (regexp-match* #rx"Rn|Ar" chem))
     (* 2 (length (regexp-match* #rx"Y" chem)))
     1))