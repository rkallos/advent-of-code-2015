#lang racket

(define (day-8-part-1 input)
  (let ([lines (file->lines input #:mode 'binary #:line-mode 'linefeed)])
    (apply + (map (lambda (line)
                    (let ([len (string-length line)]
                          [ch-in-mem (chars-in-memory line)])
                      (begin (println (list len ch-in-mem line))
                             (- len ch-in-mem))))
                  lines))))

(define (chars-in-memory str)
  (let ([len (string-length str)]
        [hexes (regexp-match* #px"\\\\x[0-9a-f]{2}" str)]
        [slashes-and-quotes (regexp-match* #px"\\\\[\"\\\\]" str)])
    (- len
       (* 3 (length hexes))
       (length slashes-and-quotes)
       2)))

(define (newly-encoded str)
  (let ([len (string-length str)]
        [hexes (regexp-match* #px"\\\\x[0-9a-f]{2}" str)]
        [slashes-and-quotes (regexp-match* #px"\\\\[\"\\\\]" str)])
    (string-length (regexp-replace #px"[\\\"]" str "\\&"))))

(define (day-8-part-test lst)
  (println (map (lambda (n)
                    (- (string-length n)
                       (- (string-length n)
                          (chars-in-memory n))))
                  lst)))
