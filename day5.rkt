#lang racket
(define input (file->string "day5.txt"))
(define inputlines (string-split input #rx"\r?\n"))
;; nicer output after some criticism of previous days entries
;;(println "Puzzle input:")
;;(for ([i inputlines]) (println i))
;; split into moves and initial state
;; get the first part
(define initialstate (let getstate ([in inputlines] [state '()])
                       (cond
                         [(string-ci=? "" (car in)) state]
                         [else (getstate (cdr in) (cons (car in) state))])))
;;(println "")
(println "Initial state:")
(for ([i initialstate]) (println i))
;; let's address those blanks
;; we know that every 3 chars a cell, keep that in mind
;; let's look at what that looks like for the first line of state
;;(println (string-split (car initialstate) #rx".{3}"))
;; split the initial state into a hash table of lists
(define table (make-hash))
;; now we need to go line by line through our initial state
;; define a way to load a single line into the table
(define (addstate line state)
    ;; split the liine and get the length
    (let* ([linelist (split-line (string->list line))]
          [linelength (length linelist)])
      (let go ([line linelist] [i 1])
        (cond
          [(not (empty? line)) (begin
                                 ;;(printf "Adding \"~a\" to ~a\n" (list->string (car line)) i)
                                 (hash-set! state (number->string i) (cons (list->string (car line)) (hash-ref state (number->string i) '())))
                                 (go (cdr line) (add1 i)))]))))
;; make a definition for splitting the string into groups of 3
(define (split-line line)
  (if (empty? line) '()
      (let ([chunk (take line 3)]
            [restof (if (> (length line) 3) (drop line 4) '())])
            (cons chunk (split-line restof)))))
(for ([line (cdr initialstate)])
  (begin
    ;;(printf "Line: \"~a\"\n" (string->list line))
    ;;(println (split-line (string->list line)))
    (addstate line table)))
;; strip blanks from the fronts of our lists
(for ([key (hash-keys table)])
  (let ([r (hash-ref table key)])
    (hash-set! table key (reverse (let go ([l r] [output '()])
      (cond
        [(empty? l) output]
        [else (let ([i (string-trim (car l))])
                (if (string=? i "") (go (cdr l) output) (go (cdr l) (cons (car l) output))))]))))))
(println (hash-keys table))
(println "Table of positions")
(let go ([i 1])
  (if (<= i (length (hash-keys table))) (begin (printf "~a: ~a\n" i (hash-ref table (number->string i))) (go (add1 i))) #f))
;; let's parse the movements
;; should be easier
(define rawmoves (let getmoves ([in inputlines] [state '()] [discard #t])
                       (cond
                         [(empty? in) state]
                         [(string-ci=? "" (car in)) (getmoves (cdr in) state #f)]
                         [discard (getmoves (cdr in) state #t)]
                         [else (getmoves (cdr in) (cons (car in) state) #f)])))
;; need to flip our list now
(define moves (reverse rawmoves))
;; (for ([l moves]) (println l))
;; for each line we want to grab follow the pattern of "move NUMBER from NUMBER to NUMBER"
;; so we can split on spaces and then take item 1, 3, and 5
(for ([move moves])
  (let* ([movelist (string-split move " ")]
         [amount (list-ref movelist 1)]
         [numberamount (string->number amount)]
         [from (list-ref movelist 3)]
         [to (list-ref movelist 5)]
         [get (hash-ref table from)]
         [got (take get numberamount)]
         [newget (drop get numberamount)]
         [set (hash-ref table to)]
         [newset (append got set)])
    (begin
      ;; lil print debug as a treat of course
      (printf "Moving ~a from ~a to ~a\n" amount from to)
      ;; take and drop!!!
      (hash-set! table from newget)
      (hash-set! table to newset))))
(println "Table of positions")
(let go ([i 1])
  (if (<= i (length (hash-keys table))) (begin (printf "~a: ~a\n" i (hash-ref table (number->string i))) (go (add1 i))) #f))
(printf "Part 1 result: ~a\n" (let go ([s ""] [i 1]) (if (> i (length (hash-keys table))) s (go (string-append s (substring (car (hash-ref table (number->string i))) 1 2)) (add1 i)))))
;; funny enough part 2 was solved by an error in my initial attempt so I just modified this code
;; if you want part 1, add a reverse on the take above on line 77