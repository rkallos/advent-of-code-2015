#lang racket

(define (day-9 input pred)
  (define ht
    (make-hash
     (map (lambda (line)
            (let [(tokens (string-split line #rx"( = )| to "))]
              (cons (take tokens 2)
                    (string->number (last tokens)))))
          (file->lines input))))
  (define nodes
    (let loop [(lines (file->lines input))
               (result '())]
      (cond [(empty? lines) result]
            [(not (member (car (string-split (car lines) #rx"( = )| to ")) result))
             (loop (cdr lines) (cons (car (string-split (car lines) #rx"( = )| to ")) result))]
            [(not (member (second (string-split (car lines) #rx"( = )| to ")) result))
             (loop (cdr lines) (cons (second (string-split (car lines) #rx"( = )| to ")) result))]
            [else (loop (cdr lines) result)])))
  (process-paths nodes ht pred))

(define (process-path perm ht)
  (cond [(or (empty? (cdr perm))
             (empty? perm)) 0]
        [(hash-has-key? ht (take perm 2))
         (+ (hash-ref ht (take perm 2))
            (process-path (drop perm 1) ht))]
        [(hash-has-key? ht (reverse (take perm 2)))
         (+ (hash-ref ht (reverse (take perm 2)))
            (process-path (drop perm 1) ht))]
        [else 99999]))

;; Brute-force search through all possible paths
(define (process-paths nodes ht pred)
  (define to-beat #f)
  (for ([path (in-permutations nodes)])
    (let ([dist (process-path path ht)])
      (unless to-beat
        (set! to-beat dist))
      (when (pred dist to-beat)
        (set! to-beat dist))))
  to-beat)

;; Part 1
(day-9 "/path/to/input" <)
;; Part 2
(day-9 "/path/to/input" >)