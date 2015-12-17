#lang racket

(define (count-combinations litres containers)
  (cc litres containers))

(define (cc litres containers)
  (cond [(= litres 0) 1]
        [(or (< litres 0) (= (length containers) 0)) 0]
        [else (+ (cc litres
                     (cdr containers))
                 (cc (- litres
                        (car containers))
                     (cdr containers)))]))

;; Inspired by Rosetta Code
;; Spits out a list of combinations of k elements from xs with no repetition
(define (combinations xs k)
  (cond [(= k 0) '(())]
        [(empty? xs) '()]
        [(append (combinations (cdr xs) k)
                 (map (lambda (x) (cons (first xs) x))
                      (combinations (cdr xs) (- k 1))))]))

(define (min-containers litres containers)
  (let loop ([k 0])
    (if (empty? (filter (lambda (l) (= 150 (apply + l)))
                        (combinations containers k)))
        (loop (add1 k))
        k)))

(define (day-17-part-1 input litres)
  (count-combinations litres (map string->number (file->lines input))))

(define (day-17-part-2 input litres)
  (let* ([containers (map string->number (file->lines input))]
         [min (min-containers litres containers)])
    (apply + (map
              (lambda (cs) (count-combinations litres cs))
              (combinations containers
                            (min-containers litres containers))))))