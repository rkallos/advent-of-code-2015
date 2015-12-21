#lang racket

(require math/number-theory)

(define (count-presents house [part2 #f])
  (apply + (map (lambda (x) (* 10 x))
                (if part2
                    (filter (lambda (y) (> (* y 50) house))
                            (divisors house))
                    (divisors house)))))

(define (day-20 input [part2 #f])
  (let loop ([house 1])
    (if (> (count-presents house part2) input)
        house
        (loop (add1 house)))))