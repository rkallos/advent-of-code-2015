#lang racket

(define (process-input input [part2 #f])
  (let [(result (list->vector
                 (map (lambda (l)
                        (list->vector (map (lambda (c) (equal? c #\#)) l)))
                      (map string->list (file->lines input)))))]
    (when part2
      (let [(len (sub1 (vector-length result)))]
        (vector-set! (vector-ref result 0) 0 #t)
        (vector-set! (vector-ref result 0) len #t)
        (vector-set! (vector-ref result len) 0 #t)
        (vector-set! (vector-ref result len) len #t)))
    result))

(define (neighbors gr r c)
  (map (lambda (x) (vector-ref (vector-ref gr (first x)) (second x)))
       (filter (lambda (pair)
                 (and (>= (first pair) 0)
                      (>= (second pair) 0)
                      (< (first pair) (vector-length gr))
                      (< (second pair) (vector-length gr))))
               (list (list (sub1 r) (sub1 c))
                     (list (sub1 r) c)
                     (list (sub1 r) (add1 c))
                     (list r (sub1 c))
                     (list r (add1 c))
                     (list (add1 r) (sub1 c))
                     (list (add1 r) c)
                     (list (add1 r) (add1 c))))))

(define (process-cell gr r c [part2 #f])
  (let ([cell (vector-ref (vector-ref gr r) c)]
        [on-neighbors (count identity (neighbors gr r c))])
    (if (and part2
             (or (equal? (list r c) `(0 0))
                 (equal? (list r c) `(0 ,(sub1 (vector-length gr))))
                 (equal? (list r c) `(,(sub1 (vector-length gr)) 0))
                 (equal? (list r c) `(,(sub1 (vector-length gr)) ,(sub1 (vector-length gr))))))
        #t
        (cond [(and cell (or (= 2 on-neighbors)
                             (= 3 on-neighbors)))
               #t]
              [(and (not cell)
                    (= 3 on-neighbors))
               #t]
              [else #f]))))

(define (round gr [part2 #f])
  (for/vector ([r (in-range (vector-length gr))])
              (for/vector ([c (in-range (vector-length gr))])
                          (process-cell gr r c part2))))

(define (lights-on gr)
  (apply + (map (lambda (v)
                  (count identity (vector->list v)))
                (vector->list gr))))

(define (day-18 input rounds [part2 #f])
  (let loop ([grid (process-input input part2)]
             [steps 0])
    (if (= steps rounds)
        (lights-on grid)
        (loop (round grid part2)
              (add1 steps)))))