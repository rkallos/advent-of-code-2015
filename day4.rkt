#lang racket

(require openssl/md5)

(define (day-4 input match)
  (define (md5-it n in)
    (md5 (open-input-string (string-append in (number->string n)))))
  (let loop ([i 0]
             [hash (md5-it 0 input)])
    (if (string=? (substring hash 0 (string-length match)) match)
        i
        (loop (add1 i) (md5-it (add1 i) input)))))

(day-4 "bgvyzdsv" "00000")
(day-4 "bgvyzdsv" "000000")