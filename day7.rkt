#lang racket
(define inputlines (string-split (file->string "day7.txt") #rx"\r?\n"))
(define (get-current-size lines)
  (let go ([in (cdr lines)] [size 0])
    (if (empty? in) (list size '())
        (begin
          (let* ([line (car in)]
                 [tokens (string-split line " ")]
                 [first (car tokens)]
                 [firstnumeric (string->number first)]
                 [second (cadr tokens)])
            (cond
              [firstnumeric (go (cdr in) (+ size firstnumeric))]
              [(and (string=? "cd" second) (string=? ".." (caddr tokens))) (list size (cdr in))]
              [(string=? "cd" second) (let* ([dirdive (get-current-size (cdr in))]
                                             [subsize (car dirdive)]
                                             [newin (cadr dirdive)])
                                             (go newin (+ size subsize)))]
              [else (go (cdr in) size)]))))))
(define dir-sizes (let go ([in inputlines] [sizes '()])
  (if (empty? in) sizes
        (let* ([line (car in)]
               [tokens (string-split line " ")]
               [first (car tokens)]
               [second (cadr tokens)])
          (cond
            [(and (string=? "cd" second) (not (string=? ".." (caddr tokens)))) (let* ([dirdive (get-current-size (cdr in))]
                                                                                      [subsize (car dirdive)]
                                                                                      [newin (cadr dirdive)])
                                                                                 (go (cdr in) (cons subsize sizes)))]
            [else (go (cdr in) sizes)])))))
(define (lte-100000 a) (<= a 100000))
(println dir-sizes)
;; part 1
(apply + (filter lte-100000 dir-sizes))
;; part 2
;; just sort the list, we need / (largest dir) and a very small one after filtering)
(define sorted-sizes (sort dir-sizes >))
(define needed-space (- 30000000 (- 70000000 (car sorted-sizes))))
(define possible-dirs (let go ([in sorted-sizes] [out '()])
                        (cond
                          [(empty? in) out]
                          [(>= (car in) needed-space) (go (cdr in) (cons (car in) out))]
                          [else (go (cdr in) out)])))
(car possible-dirs)