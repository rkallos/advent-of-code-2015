#lang racket

(define (read-seating-prefs input)
  (let loop ([lines (file->lines input)]
             [ht (hash)]
             [names '()])
    (cond
      [(empty? lines) (values ht names)]
      [else (loop (cdr lines)
                  (hash-set ht
                            (get-names (car lines))
                            (get-happiness (car lines)))
                  (if (member (car (get-names (car lines))) names)
                      names
                      (cons (car (get-names (car lines))) names)))])))

(define (get-names line)
  (regexp-match* #rx"[A-Z][a-z]+" line))

(define (get-happiness line)
  (let ([match (car (regexp-match #rx"(gain|lose) [0-9]+" line))])
    (if (regexp-match #rx"gain" match)
        (string->number (car (regexp-match #rx"[0-9]+" match)))
        (- (string->number (car (regexp-match #rx"[0-9]+" match)))))))

(define (calculate-arrangement ht names)
  (cond
    [(= (length names) 2) (+ (hash-ref ht names)
                             (hash-ref ht (reverse names)))]
    [else (+ (hash-ref ht (take names 2))
             (hash-ref ht (reverse (take names 2)))
             (calculate-arrangement ht (cdr names)))]))

(define (day-13-part-1 input)
  (define-values (ht names) (read-seating-prefs input))
  (define result 0)
  (for ([perm (in-permutations names)])
       (set! result (max result
                         (calculate-arrangement ht (append perm (list (car perm)))))))
  result)

(define (day-13-part-2 input)
  (define-values (ht names) (read-seating-prefs input))
  (set! names (cons "Me" names))
  (set! ht (foldl (lambda (x result)
                    (hash-set (hash-set result x 0)
                              (reverse x) 0))
                  ht
                  (map (lambda (x) (list "Me" x)) names)))
  (define result 0)
  (for ([perm (in-permutations names)])
       (set! result (max result
                         (calculate-arrangement ht (append perm (list (car perm)))))))
  result)