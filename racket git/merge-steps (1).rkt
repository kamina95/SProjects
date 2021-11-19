#lang racket


(define dividir (λ (L)
                  (for/list ([i L])
                    (if   (> (length i) 1)
                          (list (take i (ceiling(/ (length  i) 2)))
                                (drop i (ceiling(/ (length  i) 2))) )
                          (list i)))))



(define takeout-list (λ (L)
                       (cond
                         ((empty? L) '())
                         (#t (append (first L) (takeout-list (rest L)))))))



(define to-junto (λ (L) (takeout-list (dividir L))))



(define checking-length (λ (L) (cond
                                 ((empty? L) #t)
                                 ((equal? (length (first L)) 1)
                                  (checking-length (rest L)))
                                 (#t #f))))




(define first-steps (λ (L) (cond
                             ((checking-length L)
                              (creoqueya L))
                             (#t (print (to-junto L))
                                 (printf "~n~n")
                                 (sleep 2)
                                 (first-steps (to-junto L))))))




(define first-steps1 (λ (L) (print L)
                       (printf"~n~n")
                       (sleep 2)
                       (first-steps (list L))
                       (printf "~n")
                       (sleep 2)))




(define sort-list (λ (fir sec)
                    (cond
                      ((and(empty? fir) (empty? sec))
                       '())
                      ((empty? fir)
                       (cons (first sec) (sort-list fir (rest sec))))
                      ((empty? sec)
                       (cons (first fir) (sort-list (rest fir) sec)))
                      ((< (first fir) (first sec))
                       (cons (first fir) (sort-list (rest fir) sec)))
                      ((> (first fir) (first sec))
                       (cons (first sec) (sort-list fir (rest sec))))
                      ((equal? (first fir) (first sec))
                       (cons (first sec) (cons (first fir) (sort-list (rest fir) (rest sec)))))
                      )))



(define (testing L)
  (let ([x (ceiling (/ (length L) 2))])
    (append (put-junto (take L x))
                   (put-junto (drop L x)))))



(define put-junto (λ (L)
                    (cond
                      ((empty? L) '())
                      ((>= (length L) 2)
                        (cons (sort-list (first L)(second L)) (put-junto (rest(rest L)))))
                      ((equal? (length L) 1) (cons (first L) (put-junto (rest L))))
                      )))



(define creoqueya (λ (L)
                    (cond
                      ((equal? (length L) 1) "stop")
                      ((equal? (length L) 2)
                       (print (sort-list (first L)(second L)))
                       (printf "~n")
                       (print "stop"))
                      (#t (print (testing L))
                                 (printf "~n~n")
                                 (sleep 2)
                                 (creoqueya (testing L))))))


(first-steps1 '(10 3 7 10 90 12)) 
;(first-steps1 '(8 4 30 10 9))
;(first-steps1 '(3 7))
(first-steps1 '(21 18 3 2 5 2 9 3 2 43 5 21 54 2 1 8))
(first-steps1 '(7 3 4 2 6 9 1))
