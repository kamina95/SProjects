#lang racket

;;this is a function to inset a node in a tree
(define insert-node (λ (node tree)
                      (cond 
                        ((empty?  tree) (cons  node (cons '() (cons '() tree))))                               ;;if the node is empty, insert the new dnode
                        ((equal? node (car tree))  (cons (car tree) (list (second tree)  (third tree))))        ;;if is equal the node to some existing node, keep the same Binary tree
                        ((> node (car tree)) (cons (car tree) (list (second tree) (insert-node node (third tree)))))    ;;if is biggher the node to the binary node go to the right keeping all the tree
                        ((< node (car tree)) (cons (car tree) (list (insert-node node (second tree)) (third tree))))    ;;if is biggher the node to the binary node go to the left keeping all the tree
                        (#t "algo no va")
                        )))



;;examples
;(insert-node 0 '(7(1()(2()()))(8()(9()()))))
;(insert-node 11 '(7(1()(2()()))(8()(9()()))))
;(insert-node 2 '(7(1()(2()()))(8()(9()()))))

;;-----------------------------Exercise B.12-----------------------------



(define binary-search (λ (tree N)                                        ;;in order to search a node in binary tree the function it will go 
                        (cond                                                  ;;looking if the node where is is bigger or smaller then the one that search
                          ((empty? tree) #f) 
                          ((> (first tree) N) (binary-search  (second tree) N))
                          ((< (first tree) N) (binary-search (third tree) N))
                          ((equal? (first tree) N)#t))))

 
;(binary-search '(7(1()(2()()))(8()(9()()))) 6)
;(binary-search '(7(1()(2()()))(8()(9()()))) 0)
;(binary-search '(7(1()(2()()))(8()(9()()))) 8)
;(binary-search '(7(1()(2()()))(8()(9()()))) 9)


;;-------------------------------Exercise B.13------------------------------

;;for this exercise I decided to create tree arguments, the first one the tree
;;second one a list in which I will go adding the list inside the list
;;and a third one, a counter of the nodes
(define tree-size (λ (tree L  n) (cond                                  
                                  ((and (empty? L) (empty? tree)) n)  ;;if the list and tree are empty, print the n
                                  ((empty? tree) (tree-size L '() n)) ;;if the tree is empty after teh count, do it again with the list as a tree
                                  ((number? (first tree))  (tree-size (rest tree) L (+ n 1)))     ;;if is a number + n 1
                                  ((list? (first tree)) (tree-size (rest tree) (append (first tree) L)   n))))) ;;if the element is a list add it to L for after check it



(define tree-size1 (λ (tree) (tree-size tree '() 0)))
;(tree-size1 '(1 2 (3 4)))
;(tree-size1 '())
;(tree-size1 '((((2) 1 (3)))))



;;-------------------------Exercise B.14-------------------------------

(define tree-sum (λ (tree L  n) (cond
                                  ((and (empty? L) (empty? tree)) n)
                                  ((empty? tree) (tree-sum L '() n))
                                  ((number? (first tree))  (tree-sum (rest tree) L (+ n (first tree))))
                                  ((list? (first tree)) (tree-sum (rest tree) (append (first tree) L)   n)))))

(define tree-sum1 (λ (tree) (tree-sum tree '() 0)))
;(tree-sum1 '(1 2 (3 4)))
;(tree-sum1 '())
;(tree-sum1 '((((2) 1 (3)))))
;(tree-sum1 '(1 (2 (3 4))))











;;---------binary tree with names---------------------

(define tree-name '("Fred" "Ahmed" "Bill" "Anna" "Sally" "Zijan"))


(define insert-node-name (λ (node tree)
                      (cond 
                        ((empty?  tree) (cons  node (cons '() (cons '() tree))))                               ;;if the node is empty, insert the new node
                        ((string=? node (car tree))  (cons (car tree) (list (second tree)  (third tree))))        ;;if is equal the node to some existing node, keep the same Binary tree
                        ((string<? node (car tree)) (cons (car tree) (list (second tree) (insert-node-name node (third tree)))))    ;;if is biggher the node to the binary node go to the right keeping all the tree
                        ((string>? node (car tree)) (cons (car tree) (list (insert-node-name node (second tree)) (third tree))))    ;;if is biggher the node to the binary node go to the left keeping all the tree
                        (#t "algo no va")
                        )))



;;this functions simple takes the function of above and introduce on it a list of names
(define tree-creator (λ (L tree) (cond
                       ((empty? L) tree)
                       (#t (tree-creator (rest L) (insert-node-name (first L) tree))))))

(define tree-creator1 (λ (L) (tree-creator L (cons (first L) '( () ())))))



;;here I change the first function a bit in order to leave it as a name adder in the tree
(define insert-node-name1 (λ (name tree) (insert-node-name name (tree-creator1 tree))))


;;and this one is for search if a name is in a tree
(define tree-searcher-name (λ (name tree)                      ;;in order to search a node in binary tree the function it will go 
                             (cond                             ;;looking if the node where is is bigger or smaller then the one that search
                               ((empty? tree) #f)
                               ((string=? name (car tree)) #t)
                               ((string<? name (car tree)) (tree-searcher-name name (third tree)))
                               ((string>? name (car tree)) (tree-searcher-name name (second tree))))))

(define tree-searcher-name1 (λ (name tree) (tree-searcher-name name (tree-creator1 tree))))




;;examples
(tree-creator1 tree-name)
(insert-node-name1 "Maria"  tree-name)
(tree-searcher-name1 "Ahmed" tree-name)
(tree-searcher-name1 "paco" tree-name)












