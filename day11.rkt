#lang racket

(define (has-straight? str)
  (let loop ([lst (map char->integer (string->list str))]
             [fst (char->integer (string-ref str 0))])
    (match lst
      [(app length 2) #f]
      [(list fst (? (curry = (+ fst 1))) (? (curry = (+ fst 2))) _ ...) #t]
      [ _ (loop (cdr lst) (cadr lst))])))

;; Use pattern matching and recursion to count the number of
;; different, non-overlapping pairs of letters
(define (count-pairs str)
  (let loop ([lst (map char->integer (string->list str))]
             [pairs '()]
             [n 0])
    (match lst
      ['() n]
      [(list (and x (not (? (curryr member pairs)))) x _ ...)
       (loop (cddr lst) (cons x pairs) (add1 n))]
      [ _ (loop (cdr lst) pairs n)])))

(define (valid-password? str)
  (and (has-straight? str)
       (>= (count-pairs str) 2)
       (not (regexp-match? #rx"[iol]" str))))

(define (increment-letter ch)
  (cond
    [(char>=? ch #\z) #\a]
    ;; Optimization: Skip over i, o, l altogether
    [(ormap (lambda (x) (equal? ch x)) '(#\h #\n #\k))
     (integer->char (+ 2 (char->integer ch)))]
    [else (integer->char (add1 (char->integer ch)))]))

(define (increment-password str)
  (let ([end (string-ref str (sub1 (string-length str)))]
        [iol (regexp-match-positions #rx"[iol]" str)])
    (cond
      [iol (string-append (substring str 0 (caar iol))
                          (string (increment-letter (string-ref str (caar iol))))
                          (make-string (- (sub1 (string-length str)) (caar iol)) #\a))]
      [(char>? (increment-letter end) end)
       (string-append (substring str 0 (sub1 (string-length str)))
                      (string (increment-letter end)))]
      ;; Strange way of implementing carrying
      [else (string-append (increment-password (substring str 0 (sub1 (string-length str))))
                           (string (increment-letter end)))])))

(define (day-11-part-1 input)
  (if (valid-password? input)
      input
      (day-11-part-1 (increment-password input))))

;; Part 1
(day-11-part-1 "hxbxwxba")
;; Part 2
(day-11-part-1 (increment-password (day-11-part-1 "hxbxwxba")))