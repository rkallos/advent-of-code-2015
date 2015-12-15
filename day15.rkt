#lang racket

(define (process-ingredients lines)
  (cond [(empty? lines) (hash)]
        [else
         (hash-set (process-ingredients (cdr lines))
                   (car (regexp-match #px"^\\w+" (car lines)))
                   (apply hash
                          (flatten (map (lambda (x)
                                          (cons (car x)
                                                (string->number (cadr x))))
                                        (map string-split
                                             (regexp-match* #px"\\w+ -?[0-9]+" (car lines)))))))]))

;; Composition is (list (integer . string)), where string is a key in ingredients
(define (cookie-score ingredients composition)
  (let ([factors (remove "calories" (hash-keys (cdr (car (hash->list ingredients)))))])
    (apply
     *
     (flatten
      (map (lambda (f)
             (max 0 (apply +
                           (map (lambda (i)
                                  (* (car i) (hash-ref (hash-ref ingredients (cadr i)) f)))
                                composition))))
           factors)))))

(define (cookie-score2 ingredients composition)
  (if (not (= 500 (apply +
                         (map (lambda (c)
                                (* (car c) (hash-ref (hash-ref ingredients (cadr c))
                                                     "calories")))
                              composition))))
      0
      (let ([factors (remove "calories" (hash-keys (cdr (car (hash->list ingredients)))))])
        (apply
         *
         (flatten
          (map (lambda (f)
                 (max 0 (apply +
                               (map (lambda (i)
                                      (* (car i) (hash-ref (hash-ref ingredients (cadr i)) f)))
                                    composition))))
               factors))))))

(define (build-recipe-tree tsp ingredients)
  (for*/list ([a (in-range 0 (add1 tsp))]
              [b (in-range 0 (add1 (- tsp a)))]
              [c (in-range 0 (add1 (- tsp a b)))]
              [d (in-range 0 (add1 (- tsp a b c)))])
             (list (list a (list-ref ingredients 0))
                   (list b (list-ref ingredients 1))
                   (list c (list-ref ingredients 2))
                   (list d (list-ref ingredients 3)))))

(define (day-15-part-1 input)
  (let ([ingredients (process-ingredients (file->lines input))])
    (apply max (map (curry cookie-score ingredients)
                    (build-recipe-tree 100 (hash-keys ingredients))))))

(define (day-15-part-2 input)
  (let ([ingredients (process-ingredients (file->lines input))])
    (apply max (map (curry cookie-score2 ingredients)
                    (build-recipe-tree 100 (hash-keys ingredients))))))