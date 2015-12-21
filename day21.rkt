#lang racket

(require racket/hash)

(struct char (name damage armor hp items) #:transparent)
(struct item (name cost damage armor) #:transparent)

(define boss (char "Boss" 8 2 100 '()))
(define player (char "Player" 0 0 100 '()))

(define (attack ch1 ch2)
  (char (char-name ch2)
        (char-damage ch2)
        (char-armor ch2)
        (- (char-hp ch2)
           (- (char-damage ch1)
              (char-armor ch2)))
        (char-items ch2)))

(define (combat ch1 ch2 [turns 0])
  (cond [(>= 0 (char-hp ch2)) #t]
        [(>= 0 (char-hp ch1)) #f]
        [else (combat (attack ch2 ch1)
                      (attack ch1 ch2)
                      (add1 turns))]))

(define (equip ch str)
  (if (list? str) (equip (equip ch (first str)) (second str))
      (let ([item (hash-ref items str)])
        (char (char-name ch)
              (+ (item-damage item)
                 (char-damage ch))
              (+ (item-armor item)
                 (char-armor ch))
              (char-hp ch)
              (cons item (char-items ch))))))

(define (total-cost ch)
  (apply + (map item-cost (char-items ch))))

(define weapons
  (let loop ([out (hash)]
             [lst (string-split (regexp-replace* #rx"Weapons|:|Cost|Damage|Armor|Rings"
                                                 "Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0" ""))])
    (cond [(empty? lst) out]
          [else
           (loop
            (hash-set out (first lst) (apply item (cons (first lst) (map string->number (take (cdr lst) 3)))))
            (drop lst 4))])))

(define armor
  (let loop ([out (hash "Empty" (item "Empty" 0 0 0))]
             [lst (string-split (regexp-replace* #rx"Weapons|:|Cost|Damage|Armor|Rings"
                                                 "Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5" ""))])
    (cond [(empty? lst) out]
          [else
           (loop
            (hash-set out (first lst) (apply item (cons (first lst) (map string->number (take (cdr lst) 3)))))
            (drop lst 4))])))

(define rings
  (let loop ([out (hash "Empty" (item "Empty" 0 0 0))]
             [lst (string-split (regexp-replace* #rx"Weapons|:|Cost|Damage |Armor|Rings"
                                                 "Damage+1    25     1       0
Damage+2    50     2       0
Damage+3   100     3       0
Defense+1   20     0       1
Defense+2   40     0       2
Defense+3   80     0       3" ""))])
    (cond [(empty? lst) out]
          [else
           (loop
            (hash-set out (first lst) (apply item (cons (first lst) (map string->number (take (cdr lst) 3)))))
            (drop lst 4))])))

(define items (hash-union (hash-union weapons armor
                                      #:combine/key (lambda (k v1 v2) v1))
                          rings
                          #:combine/key (lambda (k v1 v2) v1)))

(define (build-player p items)
  (cond [(empty? items) p]
        [else (build-player (equip p (car items))
                            (cdr items))]))

(define (all-players)
  (sort (map (curry build-player player)
             (cartesian-product
              (hash-keys weapons)
              (hash-keys armor)
              (filter (lambda (p) (if (member "Empty" p) #t (not (equal? (first p) (second p)))))
                      (cartesian-product (hash-keys rings) (hash-keys rings)))))
        (lambda (v1 v2) (< (total-cost v1) (total-cost v2)))))

(define (day-21-part-1)
  (for/last ([p (all-players)]
             #:break (combat p boss))
    (list p (total-cost p))))

(define (day-21-part-2)
  (for/last ([p (reverse (all-players))]
              #:break (not (combat p boss)))
    (list p (total-cost p))))