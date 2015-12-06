#lang racket

;; Use a bit vector to represent the christmas lights
(require data/bit-vector)

(define (parse-command str part)
  (define range (map string->number (regexp-match* #px"\\d+" str)))
  (define cmd (car (regexp-match #rx"on|off|toggle" str)))
  (cond
    [(string=? cmd "on")
     (lambda (bv)
       (range-set! bv range #t part))]
    [(string=? cmd "off")
     (lambda (bv)
       (range-set! bv range #f part))]
    [(string=? cmd "toggle")
     (lambda (bv)
       (range-set! bv range 'toggle part))]))

(define (range-set! bv range val part)
  (define func
    (cond [(equal? val 'toggle)
           (if (equal? part 'part1)
               (lambda (index) (bit-vector-set! bv index (not (bit-vector-ref bv index))))
               (lambda (index) (vector-set! bv index (+ 2 (vector-ref bv index)))))]
          [(not val)
           (if (equal? part 'part1)
               (lambda (index) (bit-vector-set! bv index #f))
               (lambda (index) (vector-set! bv index (max 0 (sub1 (vector-ref bv index))))))]
          [else
           (if (equal? part 'part1)
               (lambda (index) (bit-vector-set! bv index #t))
               (lambda (index) (vector-set! bv index (add1 (vector-ref bv index)))))]))
  (for* ([row (in-range (first range) (add1 (third range)))]
         [col (in-range (second range) (add1 (fourth range)))])
    (func (+ (* row 1000) col))))

(define (day-6-part-1 input)
  (call-with-input-file input
    (lambda (in)
      (let line-loop ([str (read-line in)]
                      [bv (make-bit-vector (* 1000 1000))])
        (if (eof-object? str)
            (count identity (bit-vector->list bv))
            (begin ((parse-command str 'part1) bv)
                   (line-loop (read-line in) bv)))))))

(define (day-6-part-2 input)
  (call-with-input-file input
    (lambda (in)
      (let line-loop ([str (read-line in)]
                      [bv (make-vector (* 1000 1000) 0)])
        (if (eof-object? str)
            (foldl + 0 (vector->list bv))
            (begin ((parse-command str 'part2) bv)
                   (line-loop (read-line in) bv)))))))