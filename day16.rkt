#lang racket

(define (process-aunts lines)
  (cond [(empty? lines) '()]
        [else (cons (process-aunt (car lines))
                    (process-aunts (cdr lines)))]))

(define (process-aunt line)
  (cons (string->number (last (string-split (car (regexp-match #px"^Sue [0-9]+" line)))))
        (map (lambda (x)
               (cons (substring (car x) 0 (sub1 (string-length (car x))))
                     (string->number (cadr x))))
             (map string-split (regexp-match* #px"\\w+: [0-9]" line)))))

(define ticker-tape '(("children" . 3)
                      ("cats" . 7)
                      ("samoyeds" . 2)
                      ("pomeranians" . 3)
                      ("akitas" . 0)
                      ("vizslas" . 0)
                      ("goldfish" . 5)
                      ("trees" . 3)
                      ("cars" . 2)
                      ("perfumes" . 1)))
(define ticker-ops '(("children" . =)
                     ("cats" . >)
                     ("samoyeds" . =)
                     ("pomeranians" . <)
                     ("akitas" . =)
                     ("vizslas" . =)
                     ("goldfish" . <)
                     ("trees" . >)
                     ("cars" . =)
                     ("perfumes" . =)))

(define (intersect-aunt aunt)
  (let loop ([props (cdr aunt)])
    (cond [(empty? props) 0]
          [(= (cdr (car props))
              (cdr (assoc (car (car props)) ticker-tape)))
           (add1 (loop (cdr props)))]
          [else (loop (cdr props))])))

(define (intersect-aunt2 aunt)
  (let loop ([props (cdr aunt)])
    (cond [(empty? props) 0]
          [((eval (cdr (assoc (car (car props)) ticker-ops))) ;;lolwut
            (cdr (car props))
            (cdr (assoc (car (car props)) ticker-tape)))
           (add1 (loop (cdr props)))]
          [else (loop (cdr props))])))


(define (day-16-part-1 input)
  (filter (lambda (x) (= (cdr x) 3))
          (map (lambda (x)
                 (cons (car x)
                       (intersect-aunt x)))
               (process-aunts (file->lines input)))))

(define (day-16-part-2 input)
  (filter (lambda (x) (= (cdr x) 3))
          (map (lambda (x)
                 (cons (car x)
                       (intersect-aunt2 x)))
               (process-aunts (file->lines input)))))