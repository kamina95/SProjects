#lang racket
;;pre-conditions
;;in line 20 and 34 the system it will wait for a number as a argument for both


;;post-conditions
;;depends of the input, there are differents outputs,
;;in line 36, if the price is less than the money, print print a string and (price - money)
;;in line 37, if the prince is more than the money,print a string and (money - price)
;;en line 38, if the price and money are the same, print a string


(define fast-payment%
  (class object%
    (super-new)
    (field (money 0))
    
    (define/public (put-money mo)
      (cond
        ((number? mo)(set! money mo))       
        (#t "introduce a quanty")))        
    (define/public (get-money) money)
    ))

(define testing (new fast-payment%))

  (define do-pay%
    (class fast-payment%
      (super-new)
      ;(field (payment 0))
      (define/public (pay payment)
        (let ([x (send testing get-money)])
          (cond
            ((not(number? payment))"please intoduce a quanty")
                                 
            ( (< x payment) (format "payment done, your change is ~a £" (- payment x)))
            ( (> x payment) (format "it is misising ~a £" (- x payment)) )
            ( (equal? x payment) (print "Payment done")))) 
        ))) 

(define testing-pay (new do-pay%))
(send testing put-money 100)
(send testing get-money)
(send testing-pay pay 40)
(send testing-pay pay 150)








