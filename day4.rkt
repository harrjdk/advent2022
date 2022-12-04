#lang racket
(require rebellion/base/range)
(define input (file->string "day4.txt"))
;; (println input)
(define inlines (string-split input #rx"\r?\n"))
;; (println inlines)
(define inpairs (map (lambda (x)
                       (let* ([splitstring (string-split x ",")]
                              [splitnumbers (map (lambda (y) (map string->number (string-split y "-"))) splitstring)]
                              [ranges (map (lambda (x) (range (inclusive-bound (car x)) (inclusive-bound (cadr x)))) splitnumbers)])
                         ranges)) inlines))
;; (println inpairs)
;; let's assume pairs only for now just like the problem statement
(define (contains ranges)
  (let ([a (car ranges)]
        [b (cadr ranges)])
    (or (range-encloses? a b) (range-encloses? b a))))
(define containslist (map contains inpairs))
;; (println containslist)
(define totalcontains (apply + (map (lambda (x) (if x 1 0)) containslist)))
(println totalcontains)
;; define an overlap check, again rebellion makes this easy
(define (overlaps ranges)
  (let ([a (car ranges)]
        [b (cadr ranges)])
    (range-connected? a b)))
(define overlaplist (map overlaps inpairs))
(define totaloverlap (apply + (map (lambda (x) (if x 1 0)) overlaplist)))
(println totaloverlap)