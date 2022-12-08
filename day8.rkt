#lang racket

(require math/array)
;; let's get our input parsed into numbers
(define input
  (for/list
      ([s (string-split (file->string "day8.txt") #rx"\r?\n")])
    (for/list
        ([c (string->list s)])
      (- (char->integer c) 48))))
;; let's store the shape of the input
(define height (length input))
(define width (length (car input)))
(define wvector (list->vector (list 0 (sub1 width))))
(define hvector (list->vector (list 0 height)))
(printf "Working with a ~a x ~a grid...\n" height width)
(define arrays (list*->array input number?))
;; (println arrays)
;; utilities
(define (row-cut row width)
  (list->array
   (for/list ([i (in-range 0 width 1)])
     (vector row i))))

(define (col-cut col height)
  (list->array
   (for/list ([i (in-range 0 height 1)])
     (vector i col))))

(define (get-row array row width)
  (array-indexes-ref array (row-cut row width)))

(define (get-column array col height)
  (array-indexes-ref array (col-cut col height)))

(define (get-index array row col)
  (car (array->list (array-indexes-ref array (list->array (list (vector row col)))))))

;; left of or above
(define (left-cut array idx)
  (array-indexes-ref array (list->array (for/list ([i (in-range 0 (add1 idx))]) (vector i)))))
;; right of or below
(define (right-cut array idx)
  (array-indexes-ref array (list->array (for/list ([i (in-range idx (array-size array))]) (vector i)))))

(define (make-test in) (lambda (x) (>= x in)))

;; testing is simple enough, is there more than 1 entry for the cut >= our value
(define (test-visible array x y width height)
  (let* ([i (get-index array x y)]
         [row (get-row array y width)]
         [column (get-column array x height)]
         [leftof (left-cut row x)]
         [rightof (right-cut row x)]
         [above (left-cut column y)]
         [below (right-cut column y)]
         [pred (make-test i)]
         [leftcount (car (array->list (array-axis-count leftof 0 pred)))]
         [rightcount (car (array->list (array-axis-count rightof 0 pred)))]
         [upcount (car (array->list (array-axis-count above 0 pred)))]
         [downcount (car (array->list (array-axis-count below 0 pred)))])
    (begin
      (printf "Testing cell ~a ~a value ~a " x y i)
      (if (or (= leftcount 1)
              (= rightcount 1)
              (= upcount 1)
              (= downcount 1)) (begin
                                 (println "... yes") 1)
                               (begin (println "... no") 0)))))

;;(test-index arrays 0 2 width height 6)
;; it would be better to do this from array shapes but frankly I'm exhausted with the state of array documentation
(define edgesum (+ (* 2 width) (* 2 (- height 2))))
(printf "Edges: ~a\n" edgesum)
(define internal (for/sum ([y (in-range 1 (sub1 height) 1)])
                   (for/sum ([x (in-range 1 (sub1 width) 1)])
                     (test-visible arrays x y width height))))
(printf "Internal visible: ~a\n" internal)
(printf "Part 1 total: ~a\n" (+ edgesum internal))