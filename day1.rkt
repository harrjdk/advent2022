#lang racket
(define input (file->string "day1.txt"))
;; Let's confirm our input
(println input)
;; define our lists by splitting the groups
(define ingroups (string-split input #rx"\r?\n\r?\n"))
(println ingroups)
;; let's split these groups of calories up into lists of lists by using map to apply a function to each item in our list
(define inlists (map (lambda (in) (string-split in #rx"\r?\n")) ingroups))
(println inlists)
;; Here we're going to make a function which applies to every item in every list provided a string to number conversion
(define inNumbers (map (lambda (in) (map (lambda (in2) (string->number in2)) in)) inlists))
(println inNumbers)
;; now we will sum up the contents of every list in the provided inputs
(define totals (map (lambda (in) (apply + in)) inNumbers))
(println totals)
;; let's define a function which gets the biggest number in a list
(define max (lambda (inputs)
              ;; we will want a function to compare the first item of the list and the next item
              (define (largest a b)
                (if (> a b) a b))
              ;; foldl traverses a list from left to right and takes an initial argument, a list, and a function/procedure to execute
              ;; so (foldl + 0 '(1 2 3 4)) will add 0 to 1, then 1 to 2, then 3 to 3, and then 4 to 6 returning 10
              (foldl largest (car inputs) (cdr inputs))))
;; let's use that to get our max
(define maximum (max totals))
(println maximum)
;; now we need the top 3 maximum inputs
;; we have 2 options for this
;; we can either sort the entire list or loop for the greatest three times, omitting the previously known option
;; let's use a quicksort sourced from https://www.cs.odu.edu/~zeil/cs355/Handouts/schemedoc/schemedoc/node16.html
(define (partition compare l1)
      (cond
         ((null? l1) '())
         ((compare (car l1)) (cons (car l1) (partition compare (cdr l1))))
         (else (partition compare (cdr l1)))))

   (define (quicksort l1)
      (cond
         ((null? l1) '())
         (else (let ((pivot (car l1)))
            (append (append (quicksort (partition (lambda (x) (< x pivot)) l1))
                       (partition (lambda (x) (= x pivot)) l1))
                    (quicksort (partition (lambda (x) (> x pivot)) l1)))))))
(define sortedInputs (quicksort totals))
;; let's take the right 3 elements
(define topthree (take-right sortedInputs 3))
(println topthree)
;; let's total those
(define topthreetotal (apply + topthree))
(println topthreetotal)