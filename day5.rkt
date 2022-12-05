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
;; split the first line of the state up as initial lists
(for ([i (string-split (car initialstate) #rx".{3}")]) (hash-set! table (string-trim i) '()))
;;(println "")
;;(println "Should be an empty state table")
;;(println table)
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
(println "Table of positions")
(printf "1: ~a\n" (hash-ref table "1"))
(printf "2: ~a\n" (hash-ref table "2"))
(printf "3: ~a\n" (hash-ref table "3"))
(printf "4: ~a\n" (hash-ref table "4"))
(printf "5: ~a\n" (hash-ref table "5"))
(printf "6: ~a\n" (hash-ref table "6"))
(printf "7: ~a\n" (hash-ref table "7"))
(printf "8: ~a\n" (hash-ref table "8"))
(printf "9: ~a\n" (hash-ref table "9"))
;; let's parse the movements
;; should be easier
