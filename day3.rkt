#lang racket

;; A posn has a x coordinate and a y coordinate
(struct posn (x y) #:transparent)

(define (day-3-part-1 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((ch (read-char in))
                 (pos (posn 0 0))
                 (houses (hash (posn 0 0) 1)))
        (cond
          ((eof-object? ch)
           (length (filter (lambda (h)
                             (> h 0)) (hash-values houses))))
          (else
           (let* ((dx (cond ((char=? ch #\>) 1)
                            ((char=? ch #\<) -1)
                            (else 0)))
                  (dy (cond ((char=? ch #\v) 1)
                            ((char=? ch #\^) -1)
                            (else 0)))
                  (new-pos (posn
                            (+ dx (posn-x pos))
                            (+ dy (posn-y pos)))))
             (loop (read-char in)
                   new-pos
                   (hash-update houses
                                new-pos
                                add1
                                0)))))))))

(define (day-3-part-2 input)
  (call-with-input-file input
    (lambda (in)
      (let loop ((ch (read-char in))
                 (santa-pos (posn 0 0))
                 (robo-pos (posn 0 0))
                 (turn #t)
                 (houses (hash (posn 0 0) 1)))
        (cond
          ((eof-object? ch)
           (length (filter (lambda (h)
                             (> h 0)) (hash-values houses))))
          (else
           (let* ((dx (cond ((char=? ch #\>) 1)
                            ((char=? ch #\<) -1)
                            (else 0)))
                  (dy (cond ((char=? ch #\v) 1)
                            ((char=? ch #\^) -1)
                            (else 0)))
                  (new-pos (posn
                            (+ dx (posn-x (if turn santa-pos robo-pos)))
                            (+ dy (posn-y (if turn santa-pos robo-pos))))))
             (loop (read-char in)
                   (if turn new-pos santa-pos)
                   (if turn robo-pos new-pos)
                   (not turn)
                   (hash-update houses
                                new-pos
                                add1
                                0)))))))))
