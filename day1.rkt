#lang racket

(define (day-1-part-1 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((ch (read-char in))
                 (floor 0))
        (cond
          ((eof-object? ch) floor)
          ((char=? ch #\))
           (loop (read-char in) (sub1 floor)))
          ((char=? ch #\()
           (loop (read-char in) (add1 floor))))))))

(define (day-1-part-2 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((ch (read-char in))
                 (floor 0)
                 (position 1))
        (cond
          ((< floor 0) position)
          ((char=? ch #\))
           (loop (read-char in) (sub1 floor) (add1 position)))
          ((char=? ch #\()
           (loop (read-char in) (add1 floor) (add1 position))))))))