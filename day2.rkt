#lang racket
;; imports!!!
(require rackunit)
;; define our input
(define input (file->string "day2.txt"))
;; make a function for scoring a line
;; given 3 * 3 combinations = 9 totals it's most efficient to just skip the custom comparator and make a matcher
(define score (lambda (line)
                (cond
                  [(string-ci=? "A X" line) 4]
                  [(string-ci=? "A Y" line) 8]
                  [(string-ci=? "A Z" line) 3]
                  [(string-ci=? "B X" line) 1]
                  [(string-ci=? "B Y" line) 5]
                  [(string-ci=? "B Z" line) 9]
                  [(string-ci=? "C X" line) 7]
                  [(string-ci=? "C Y" line) 2]
                  [(string-ci=? "C Z" line) 6])))
;; let's confirm the scores and show off racket's testing
(check-eq? (score "A X") 4)
(check-eq? (score "A Y") 8)
(check-eq? (score "A Z") 3)
(check-eq? (score "B X") 1)
(check-eq? (score "B Y") 5)
(check-eq? (score "B Z") 9)
(check-eq? (score "C X") 7)
(check-eq? (score "C Y") 2)
(check-eq? (score "C Z") 6)
;; now let's run through the entire input after splitting it into lines
(define inlines (string-split input #rx"\r?\n"))
(define linescores (map score inlines))
;; sum our scores
(define totalscore (apply + linescores))
(println totalscore)
;; part 2
;; so now we just need another function to address the same issue...
(define cheating (lambda (line)
                (cond
                  [(string-ci=? "A X" line) 3]
                  [(string-ci=? "A Y" line) 4]
                  [(string-ci=? "A Z" line) 8]
                  [(string-ci=? "B X" line) 1]
                  [(string-ci=? "B Y" line) 5]
                  [(string-ci=? "B Z" line) 9]
                  [(string-ci=? "C X" line) 2]
                  [(string-ci=? "C Y" line) 6]
                  [(string-ci=? "C Z" line) 7])))
;; let's one-line this since it's practically the same
(define totalpart2 (apply + (map cheating inlines)))
(println totalpart2)