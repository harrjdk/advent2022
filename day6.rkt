#lang racket
(require rackunit)
(require srfi/1) ;; for drop-right
(define input (file->string "day6.txt"))
;; if the buffer is len 'numofchars' and has no dups? return index i
;; if the length is less than 'numofchars' add the last char to the buffer, shift the input and increment index i
;; otherwise drop the oldest char from the buffer and execute the above
(define (chars-from-front in numofchars)
  (let go ([str (string->list in)] [buffer '()] [i 0])
    (cond
      [(and (eq? (length buffer) numofchars) (not (check-duplicates buffer char=?))) i]
      [(< (length buffer) numofchars) (go (cdr str) (cons (car str) buffer) (add1 i))]
      [else (go (cdr str) (cons (car str) (drop-right buffer 1)) (add1 i))])))
;; part 1
(check-eq? (chars-from-front "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4) 7)
(check-eq? (chars-from-front "bvwbjplbgvbhsrlpgdmjqwftvncz" 4) 5)
(check-eq? (chars-from-front "nppdvjthqldpwncqszvftbrmjlhg" 4) 6)
(check-eq? (chars-from-front "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4) 10)
(check-eq? (chars-from-front "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4) 11)
(println (chars-from-front input 4))
;; part 2
(check-eq? (chars-from-front "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14) 19)
(check-eq? (chars-from-front "bvwbjplbgvbhsrlpgdmjqwftvncz" 14) 23)
(check-eq? (chars-from-front "nppdvjthqldpwncqszvftbrmjlhg" 14) 23)
(check-eq? (chars-from-front "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14) 29)
(check-eq? (chars-from-front "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14) 26)
(println (chars-from-front input 14))