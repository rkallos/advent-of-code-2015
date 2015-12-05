#lang racket

;; Predicate function for determining if a string is naughty or nice: part 1
(define (part-1 str)
  (define number-of-vowels 0)
  (define double-letter? #f)
  (define forbidden-strings (list "ab" "cd" "pq" "xy"))
  (define traversal
    (for/fold ([n 0])
              ([i (in-range 0 (string-length str))]
               ;; Check for forbidden string
               #:break (and (> i 0)
                            (ormap (lambda (x)
                                (string=? x (substring str (sub1 i) (add1 i))))
                              forbidden-strings)))
      ;; Check for double letter
      (when (and (> i 0)
                 (not double-letter?)
                 (equal? (string-ref str (sub1 i)) (string-ref str i)))
        (set! double-letter? #t))
      ;; Check for vowel
      (when (and (< number-of-vowels 3)
                 (string-contains? "aeiou" (string (string-ref str i))))
        (set! number-of-vowels (add1 number-of-vowels)))
      (add1 n)))
  (and (= traversal (string-length str))
       (>= number-of-vowels 3)
       double-letter?))

;; Predicate function for determining if a string is naughty or nice: part 2
(define (part-2 str)
  (define twice-appearing-double-letters? #f)
  (define xyx-pattern? #f)
  (for ([i (in-range 0 (string-length str))]
        #:break (and twice-appearing-double-letters?
                     xyx-pattern?))
    (when (and (> i 2)
               (not xyx-pattern?)
               (equal? (string-ref str i) (string-ref str (- i 2))))
      (set! xyx-pattern? #t))
    (when (and (>= i 3)
               (not twice-appearing-double-letters?)
               (string-contains? (substring str 0 (sub1 i))
                                 (substring str (sub1 i) (add1 i))))
      (set! twice-appearing-double-letters? #t)))
  (and twice-appearing-double-letters?
       xyx-pattern?))

;; day-5 takes a predicate function and a file
(define (day-5 nice-string? input)
  (call-with-input-file input
    (lambda (in)
      (let line-loop ([str (read-line in)]
                      [nice 0])
        (if (eof-object? str) nice
            (line-loop (read-line in)
                       (if (nice-string? str)
                           (add1 nice)
                           nice)))))))