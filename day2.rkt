#lang racket

(define (day-2-part-1 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((line (read-line in))
                 (gift-wrap 0))
        (cond
          ((eof-object? line) gift-wrap)
          (else
           (let*-values ([(l w h) (apply values (map string->number (string-split line "x")))]
                         [(slack) (min (* l w)
                                       (* w h)
                                       (* h l))])
             (loop (read-line in)
                   (+ gift-wrap
                      (* 2 l w)
                      (* 2 w h)
                      (* 2 h l)
                      slack)))))))))

(define (day-2-part-2 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((line (read-line in))
                 (ribbon 0))
        (cond
          ((eof-object? line) ribbon)
          (else
           (let*-values ([(l w h) (apply values (map string->number (string-split line "x")))]
                         [(perimeter) (min (+ l l w w)
                                           (+ w w h h)
                                           (+ h h l l))])
             (loop (read-line in)
                   (+ ribbon
                      (* l w h)
                      perimeter)))))))))