#lang racket

(define (list-small-factors n [m 1])
  (cond [(> m (sqrt n)) '()]
        [(= 0 (remainder n m))
         (cons m (list-small-factors n (add1 m)))]
        [else (list-small-factors n (add1 m))]))

(define (list-factors n)
  (let ([sf (list-small-factors n)])
    (set->list (list->set
     (append sf (map (lambda (x) (/ n x))
                     (reverse sf)))))))

(define (count-presents house)
  (apply + (map (lambda (x) (* 10 x))
                (list-factors house))))

(define (count-presents2 house)
  (apply + (map (lambda (x) (* 11 x))
                (filter (lambda (y) (> (* y 50) house))
                        (list-factors house)))))

(define (day-20 input [part2 #f])
  (let loop ([house 1])
    (if (> ((if part2 count-presents2 count-presents) house) input)
        house
        (loop (add1 house)))))