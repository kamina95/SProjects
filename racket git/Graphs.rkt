#lang racket/gui

;;Breadth-first search
;;comes from the book "Grokking algorithms"

(define testh                              ;;here I will set all the nodes with their neighbours
  (hash 1 (list 2 6 4)
        2 (list 4 3 "H1")                    
        3 (list "H1")
        4 (list 8)
        5 (list 1)
        6 (list 5 7)
        7 (list 1 8)
        8 (list "H2")
        "H1" '()
        "H2" '()
        ))

;;for do this method I need to have two thinks, a hash table with all the elements, and a queue
;;(like I didn't find in the inbuild funciton of queue of racket the enought applications,
;;I decide to do a function to do the same of the queue but in a list)


;;for this function I will ask for the graph, the first node and the end one
;;and as works this method, I will simple doing a list with the first node,
;;if is not equal to the end one, I will add the  neighbours of this one to the queue,
;;and then check again the fist element of it and like this until,finds the end or the list finish empty.
;;doing this it going to show us as well the end more close, because for the queue first is going to check , the node more close.


(define path? (λ (graph end L) 
                      (cond
                        ((empty?  L) "there is not path")                            ;;this is the function that check if the nodes are similar or the list is empty
                        ((equal? (first L) end)"there is a path")                    ;;or if not, search for add teh neighbours 
                        (#t (path? graph end (rest (add-final-queue L graph))))
                        )))



(define add-final-queue (λ (L graph)                                  ;;As is hash we only need to append the values of the keys
                       (append L (hash-ref graph (first L)))))


(define graph-path? (λ (graph start end) (path? graph end  (list start))))   ;;function of the function for leave it better


;;Examples
;(graph-path? testh 1 "H1")
;(graph-path? testh 3 "H2") 







;;this one is for check the closes hospital from a node,
;;use the same method plus, it adds a set where it will introduccing the nodes
;;that are check, like this is going to avoid loops,

(define H-close (λ (graph repe L)                           ;;add a set for check repetitive inputs
                  (cond
                    ((empty? L) "there isnt")
                    ((set-member? repe (first L))
                     (H-close graph repe (rest L)))         ;;if the element is in the set, skip it
                    ((string? (first L))
                     (string-append  "the hospital more close is: "  (first L)))
                    (#t (H-close graph
                                 (set-add repe (first L))
                                 (rest (add-final-queue L graph)))))))  ;;if is not what is looking, it will add his neigbourgs to the queue/list

(define H-close1 (λ (graph start) (H-close graph (set 0) (list start) )))

(H-close1 testh 1)

(H-close1 testh 6)






;;and this is a aprox using list of pairs instead of hash-table,
;;then to add the neighbours to the list it use for?fold to check all the grapg


(define GR '((1 2) (1 3) (2 4) (3 2) (3 5) (5 10) (4 10)))

(define (add-toL L elem)
  (foldr cons (list elem) L))   ;;for append elements to the final of the list

;(add-toL '(1 2 3 4) 5)

(define path-finder (λ (graph end L)
                      (cond
                        ((empty?  L) "there is not path")
                        ((equal? (first L) end)"there is a path")             ;;recusrsion adding elements to the list until find a end or not
                        (#t (path-finder graph end (rest (add-end L graph))))
                        )))


(define add-end (λ (L graph)
                  (for/fold ([acc L]) ([i graph])                            ;;this functions search teh neiborhuds of a node and added to the search
                    (cond
                      ((equal? (first i) (first L))
                       (add-toL acc (second i)) )
                      (#t  acc)))))



(define 1path? (λ (graph start end) (path-finder graph end (list start))))

;(1path? GR 1 10)
;(1path? GR 3 1)








