#lang racket
(require 2htdp/batch-io)

(define list-flats(read-words/line "data"))    ;;defining list-flats as the list of list that is going to be read it from "data"

;(read-words/line "data");;some examples
;(read-words "data")
;(read-file "data")

(struct flats (flat name phone))           ;;creating a struct of data

(define all-data-flat (λ (L) 
                        (cond                                     ;;use the input of the "data" for fill the struct
                          ((empty? L) '())                         
                          (#t (cons
                               (flats (caar L) (cadr (car L)) (caddr (car L)))
                               (all-data-flat (cdr L)))))))

;(map flats-flat (all-data-flat list-flats))

(define LData (all-data-flat list-flats))    ;;do a list of structs
(define looking-for-fact (λ (fact)
                           (for/fold             ;;data searcher
                            ([acc '()])
                            ([i LData] #:when (or                                 ;;use a fact and the itineration for search the 
                                           (equal?  fact (flats-flat i))          ;;necesary struct in the list and print it
                                           (equal?  fact (flats-name i))
                                           (equal?  fact (flats-phone i))))
                                     (cons (flats-flat i)(cons (flats-name i) (cons (flats-phone i) acc))))))

(looking-for-fact "4")
(looking-for-fact "451345145345")
(looking-for-fact "tomas")







;;the problem for that I didnt do it read and write from the same file is that I based
;;the last code in the read-words/line, that read from a list of rows
;;but this inbuild functions only writte from a one row string.


                      ;;to write

(define change-flat (λ (L1 flat new-name new-phone)
                      (cond                        
                        ((and (empty? L1) (number? (string->number flat)))   ;;if the flat dosent exist but is a number, add it to the final of the list
                         append L1 (list flat new-name new-phone))
                        ((empty? L1) '())
                        ((equal? flat (first L1))         ;;it looks for the flat and when is find it change the other two
                         (cons flat                       ;;facts
                               (cons new-name
                                   (cons new-phone (cdddr L1)))))
                        (#t (cons (first L1)
                                (change-flat (cdr L1) flat new-name new-phone))))))


(define (slist->string slst)                                 ;;the write function only read a unique string
  (cond ((empty? slst) "")                                   ;;this function is for create a unique string from a list of it
        ((empty? (rest slst)) (first slst))
        (else (string-append (first slst) " "
                             (slist->string (rest slst))))))


(define (write-in-file file flat new-name new-phone)            
  (write-file file (slist->string (change-flat (read-words file) flat new-name new-phone))))


;;Example of writte
;(write-in-file "dataprueba" "2" "dionisio" "87698768976")
