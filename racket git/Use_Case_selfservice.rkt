#lang racket/gui

(define self-service-secondary%
  (class object%
    (super-new)
    (field (list-items (list->set '())))
    (field (hash-items-price (make-hash)))
    (define/public (add item price)
      (cond ((and (number? price) (string? item))
             (set! list-items (set-add list-items item))
             (hash-set! hash-items-price item price))
            (#t print "Not a proper item")
            ))
    (define/public (get-list) list-items)
    (define/public (get-item-price) (hash->list hash-items-price))
    (define/public (get-hash) hash-items-price)
    )
  )



(define testing (new self-service-secondary%))
(send testing add "banana" 9)
(send testing add "chocolate" 2)
(send testing add "rice" 1)
;(send testing add 23 "color" )

;(set? (send testing get-list))
;(set-member? (send testing get-list)  "banana")
;(send testing get-item-price)
;(send testing get-hash)





(define self-service-primary%
  (class self-service-secondary%
    (super-new)
    (field (all-items '()))
    (field (item '()))
    (field (total-price 0))
    (define/public (add-product item)
      (cond
        ((and (string? item) (set-member? (send testing get-list) item))
         (set! all-items  (cons (cons  item (hash-ref (send testing get-hash) item))  all-items))
         (set! total-price (+ (hash-ref (send testing get-hash) item) total-price)))
        (#t "there is not such product"))
      )
    (define/public (take-out-product item )
      (set! all-items (for/fold ([acc '()]) ([i all-items])
        (cond ((not (equal? (car i) item)) (cons i acc))
              (#t  (set! total-price (- total-price (hash-ref (send testing get-hash) item))) acc)))))
    (define/public (see-ticket) (format "list of items: ~a Total price: ~a" all-items total-price) )
    (define/public (see-list) all-items)
    (define/public (see-total-price) total-price)
    ))



(define test-costumer (new self-service-primary%))
;(send test-costumer add-product "banana")
;(send test-costumer add-product "chocolate")
;(send test-costumer add-product "chocolate")
;(send test-costumer add-product "rice")
;(send test-costumer see-ticket)
;(send test-costumer take-out-product "banana")
;(send test-costumer see-ticket)
;(send test-costumer see-total-price)



(define payment%
  (class self-service-primary%
    (super-new)
    (field (type #t))
    (define/public (check-funds money)
      (if
       (>= money (send test-costumer see-total-price))
              (set! type #t)
              (set! type #f)) type)
    (define/public (type-payment)
      (printf "type of payment?~n")
      (let ([x (read)])
      (cond
        ((equal? x 'cash) (format "instert ~a$" (send test-costumer see-total-price)))
        ((equal? x 'card) (format "instert the card for ~a$" (send test-costumer see-total-price)))
        (#t (printf "decide between cash or card ~n") (type-payment)))))))

(define test-payment (new payment%))
;(send test-payment check-funds 1)
;(send test-payment type-payment)



;;------------------------GUI-------------------------------

;;--------------------principal window------------------------

(define welcome (new frame%
                     [label "Welcome"]
                     [width 400] [height 150]))

(send welcome show #t)
;;               principal panel

(define first-panel (new vertical-panel%
                         [parent welcome]
                         [alignment '(center center)]
                         [spacing 30]))

(define message-welcome(new message%
                            [parent first-panel]
                            [label "Push the buttom for start"]))

(define start-button (new button%
                    [label "start"]
                    [parent first-panel]
                    [callback (λ (o e)
                                (send adder show #t)
                                (send welcome show #f))]))




;;----------------------adder---------------------------------

(define adder (new frame%
                     [label "Add your products"]
                     [width 500] [height 200]))



(define adder-panel (new vertical-panel%
                         [parent adder]
                         [alignment '(center center)]
                         [spacing 30]
                         [horiz-margin 50]
                         [vert-margin 10]))

(define message-adder(new message%
                            [parent adder-panel]
                            [label "Write the product that you want to add to the basket"]))


(define add-field (new text-field%
                       [parent adder-panel]
                       [label "Product   "]
                       ))

(define message-error (new message%
                            [parent adder-panel]
                            [label "                                           "]))

(define secondary-panel (new horizontal-panel%
                             [parent adder]
                             [alignment '(center center)]
                             [spacing 20]
                             [horiz-margin 50]
                             [vert-margin 30]))

(define add-button (new button%
                    [label "add"]
                    [parent secondary-panel]
                    [callback (λ (o e)(let ([x (send add-field get-value)])
                                (cond
                                  ((set-member? (send testing get-list) x)
                                   (send test-costumer add-product x)
                                   (send add-field set-value "")
                                   (send message-error set-label ""))
                                  (#t (send message-error set-label "Incorrect Product")
                                      (send add-field set-value ""))
                                  )
                                ))]))


(define pay-button (new button%
                        [parent secondary-panel]
                        [label "Finish and Pay"]
                        [callback (λ (o e)
                                     (send type show #t)
                                     (send adder show #f))]))


;;------------------- Type of payment---------------------------

(define type (new frame%
                     [label "Type of payment"]
                     [width 300] [height 150]))

(define type-panel (new horizontal-panel%
                        [parent type]
                        [alignment '(center center)]
                        [spacing 20]
                        ))

(define cash-button (new button%
                        [parent type-panel]
                        [label "Cash"]
                        [callback (λ (o e)
                                     (send type show #f)
                                     (send cash-window show #t))]))

(define card-button (new button%
                        [parent type-panel]
                        [label "Card"]
                        [callback (λ (o e)
                                     (send type show #f)
                                     (send card-window show #t))]))


;;-------------------payments windows------------------

(define cash-window (new frame%
                     [label "Cash Payment"]
                     [width 400] [height 200]))

(define cash-panel (new vertical-panel%
                        [parent cash-window]
                        [alignment '(center center)]
                        [spacing 20]
                        (horiz-margin 30)))

(define m-total (new message%
                     [parent cash-panel]
                     [label (format "The total to pay is: ~a" (send test-costumer see-total-price))]))

(define quanty (new text-field%
                    [parent cash-panel]
                    [label "Introduce the money"]) )






(define card-window (new frame%
                     [label "Card Payment"]
                     [width 300] [height 150]))









