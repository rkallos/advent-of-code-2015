#lang racket

(struct char (name hp mana damage) #:transparent)
(struct spell (name cost damage turns) #:transparent)
(struct effect (name turns) #:mutable #:transparent)
(struct battle (player boss turn effects mana-spend) #:transparent)

(define player-spells
  (hash "Magic Missile"
        (spell "Magic Missile" 53 4 0)
        "Drain"
        (spell "Drain" 73 2 0)
        "Shield"
        (spell "Shield" 113 0 6)
        "Poison"
        (spell "Poison" 173 0 6)
        "Recharge"
        (spell "Recharge" 229 0 5)))

(define player (char "Player" 50 500 0))
(define boss (char "Boss" 55 0 8))

(define puzzle-battle (battle player boss 1 '() 0))

;; The state of a battle is a function of the sequence of spells cast, and the boss/player stats
(define (do-battle spells [b (battle player boss 1 '() 0)] [part2 #f])
  (call/ec
   (lambda (ec)
     (let ([spells spells]
           [player (battle-player b)]
           [boss (battle-boss b)]
           [turn (battle-turn b)]
           [effects (battle-effects b)]
           [mana-spend (battle-mana-spend b)])
       (if (or (>= 0 (char-mana player))
               (>= 0 (char-hp player))
               (>= 0 (char-hp boss))
               (and (= 1 (remainder turn 2)) (empty? spells))) ;; Player's turn and no spells left to cast
           (ec (battle player boss turn effects mana-spend))
           (begin
             (when (and part2 (= 1 (remainder turn 2))) ;; "Hard Mode"
               (set! player (struct-copy char player [hp (sub1 (char-hp player))])))
             ;; Apply effects
             (when (member "Poison" (map effect-name effects))
               (set! boss (struct-copy char boss [hp (- (char-hp boss) 3)])))
             (when (member "Recharge" (map effect-name effects))
               (set! player (struct-copy char player [mana (+ (char-mana player) 101)])))
             ;; Decrement effect timers, and remove expired effects
             (set! effects (filter (lambda (e) (< 0 (effect-turns e)))
                                   (map (lambda (e) (effect (effect-name e) (sub1 (effect-turns e))))
                                        effects)))
             ;; Determine turn. Odd means player, even means boss
             (if (= 1 (remainder turn 2))
                 ;; Cast new spell
                 (begin
                   (let ([name (car spells)]
                         [spell (hash-ref player-spells (car spells))])
                     (case (car spells)
                       [("Magic Missile") (set! boss (struct-copy char boss [hp (- (char-hp boss) (spell-damage spell))]))]
                       [("Drain") (begin (set! boss (struct-copy char boss [hp (- (char-hp boss) (spell-damage spell))]))
                                         (set! player (struct-copy char player [hp (+ (char-hp player) (spell-damage spell))])))]
                       [("Shield" "Poison" "Recharge") (set! effects (cons (effect (car spells)
                                                                                   (spell-turns spell)) effects))])
                     (set! player (struct-copy char player [mana (- (char-mana player) (spell-cost spell))])))
                   (do-battle (cdr spells)
                              (battle player boss (add1 turn) effects
                                      (+ mana-spend (spell-cost (hash-ref player-spells (car spells)))))))
                 ;; Boss attacks
                 (begin
                   (when (>= 0 (char-hp boss)) ;; Check for death (from poison)
                     (ec (battle player boss turn effects mana-spend)))
                   (do-battle spells
                              (battle
                               (char (char-name player)
                                     (- (char-hp player)
                                        (if (member "Shield" (map effect-name effects))
                                            (- (char-damage boss) 7)
                                            (char-damage boss)))
                                     (char-mana player)
                                     (char-damage player))
                               boss (add1 turn) effects mana-spend))))))))))

;; Use memoization on lists of spells and battle states
(define battle-states (make-hash))
(define (battle-state spells)
  (let ([b (do-battle spells)])
    (hash-set! battle-states
               spells
               (do-battle spells)) b))

;; Battle -> (list spell)
;; Based on the state of the battle, List castable spells
(define (spell-choices b)
  (filter (lambda (s)
            (and (not (member (spell-name s)
                              (map effect-name (battle-effects b))))
                 (>= (char-mana (battle-player b))
                     (spell-cost s))))
          (hash-values player-spells)))

(define (day22 b [part2 #f])
  (let ([min-mana +inf.0]
        [spells-cast '()])
    (let loop ([b b]
               [sps '()]
               [chs (spell-choices b)])
      (cond
        [(or (>= 0 (char-hp (battle-player b)))
             (>= (battle-mana-spend b) min-mana))
         #f]
        [(>= 0 (char-hp (battle-boss b)))
         (begin (set! min-mana (battle-mana-spend b))
                (set! spells-cast sps))]
        [(empty? chs) #f] ;; Cannot cast any more spells
        [else (let ([db (do-battle (list (spell-name (car chs))) b part2)])
                (begin (loop db ;; Depth
                             (cons (car chs) sps)
                             (spell-choices db))
                       (loop b ;; Breadth
                             sps
                             (cdr chs))))]))
    (let ([spell-names (map spell-name (reverse spells-cast))])
      (values min-mana
              spell-names
              (do-battle spell-names b part2)))))

(day22 (battle (char "Player" 10 250 0) (char "Boss" 13 0 8) 1 '() 0))
(day22 (battle (char "Player" 10 250 0) (char "Boss" 14 0 8) 1 '() 0))
(day22 puzzle-battle)
(day22 puzzle-battle #t)
