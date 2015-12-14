#lang racket

(define (process-reindeer lines)
  (cond [(empty? lines) '()]
        [else (cons (append (regexp-match #px"^\\w+" (car lines))
                            (map string->number (regexp-match* #rx"[0-9]+" (car lines))))
                    (process-reindeer (cdr lines)))]))

(define (reindeer-distance deer time)
  (cond [(> 0 time) 0]
        [(< time (caddr deer))
         (* (cadr deer) time)]
        [else (+ (* (cadr deer) (caddr deer))
                 (reindeer-distance deer (- time (apply + (cddr deer)))))]))

(define (reindeer-points all-deer time)
  (let loop ([t 1]
             [distances (map (lambda (x) (cons (reindeer-distance x 1) (car x))) all-deer)]
             [scores (apply hash (foldl (lambda (x result) (append result (list x 0)))
                                        '()
                                        (map car all-deer)))])
    (cond [(= t time) scores]
          [else (loop (add1 t)
                      (map (lambda (x) (cons (reindeer-distance x t) (car x)))
                           all-deer)
                      (hash-update scores
                                   (cdr (assoc (apply max (map car distances))
                                               distances))
                                   add1))])))

(define (day-14-part-1 input time)
  (let ([reindeer (map (lambda (x) (cons (reindeer-distance x time) (car x)))
                       (process-reindeer (file->lines input)))])
    (apply max (map car reindeer))))

(define (day-14-part-2 input time)
  (let ([reindeer (process-reindeer (file->lines input))])
    (apply max (hash-values (reindeer-points reindeer time)))))