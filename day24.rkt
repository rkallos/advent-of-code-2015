#lang racket

(define (list-groups input [part2 #f])
  (define weight (/ (apply + input) (if part2 4 3)))
  (define ts '())
  (let loop ([t '()]
             [l (sort input <)])
    (call/ec
     (lambda (c)
       (cond
         [(= weight (apply + t))
          (begin
            (set! ts (cons t ts))
            (loop (cdr t) l))]
         [(empty? l) c]
         [(<= (car l) (- weight (apply + t)))
          (loop (cons (car l) t)
                (cdr l))
          (loop t (cdr l))]
         [else c]))))
  ;; Return groups sorted by length and quantum entanglement
  (sort ts
        (lambda (x y)
          (if (= (length x) (length y))
              (< (apply * x) (apply * y))
              (< (length x) (length y))))))

(define (solve input [part2 #f])
  (let ([gs (list-groups (map string->number (file->lines input)) part2)])
    (apply * (car gs))))

;; part 1: (solve "/path/to/file")
;; part 2: (solve "/path/to/file" #t)