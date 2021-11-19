#lang racket
(require "oauth-single-user.rkt")
 (require json) 

(define myAuthenticationParameters (new oauth-single-user%
                                        [consumer-key "kAdo6x6SlCNeXXpsvqm1xAGEw"] ;; API key
                                        [consumer-secret
                                         "X1Dpjh484UsHCTqzWaALwTkEZuJHDsyPtZcxhAR4YO34JniIkt"] ;; API secret
                                        [access-token
                                         "1272850793717260288-h5xFWClNXr8uKD4XHEHeEguo0RBmk3"] ;; access token
                                        [access-token-secret
                                         "52CndVvU02eu3vZGyqEOOh6GgHdQ3cUPPGEGf3IOHVfZi"] ;; access token secret
                                        ))


;(define twitterResponse
;  (bytes->jsexpr
;   (first
;    (send myAuthenticationParameters
;          get-request
;          "https://api.twitter.com/1.1/search/tweets.json"
;          (list (cons 'q "middlesexuni") (cons 'since_id "434732003612778496") )
;          ) ;; end of send
;    ) ;; end of first
;   ) ;; end of bytes->jsexpr
;  ) ;; end of definition of twitterResponse
;
;(define statuses (hash-ref twitterResponse 'statuses))
;
;(for ([tweet statuses])
;  (printf "The ID is: ~a and the message is: ~a\n"
;          (hash-ref tweet 'id)
;          
;          (hash-ref tweet 'text)
;          )
;  )




;(define twitterResponse
;  (bytes->jsexpr
;   (first
;    (send myAuthenticationParameters
;          get-request
;          "https://api.twitter.com/1.1/search/tweets.json"
;          (list (cons 'q "middlesexuni") (cons 'since_id "1389288917115998209") )
;          ) ;; end of send
;    ) ;; end of first
;   ) ;; end of bytes->jsexpr
;  ) ;; end of definition of twitterResponse

;(define statuses (hash-ref twitterResponse 'statuses))

;(for ([tweet statuses])
;  (printf "The ID is: ~a and the message is: ~a\n"
;          (hash-ref tweet 'id)         
;          (hash-ref tweet 'text)
;          )
;  )


;(hash-ref (first statuses) 'id)

(define (twitterRetriver last-id)
  (bytes->jsexpr
   (first
    (send myAuthenticationParameters
          get-request
          "https://api.twitter.com/1.1/search/tweets.json"
          (list (cons 'q "middlesexuni") (cons 'since_id (number->string last-id)) )
          ) ;; end of send
    ) ;; end of first
   )) ;; end of bytes->jsexpr)


(define statuses (hash-ref twitterRetriver 'statuses))

(twitterRetriver (hash-ref (first statuses) 'id))

(for ([tweet statuses])
  (printf "The ID is: ~a and the message is: ~a\n"
          (hash-ref tweet 'id)         
          (hash-ref tweet 'text)
          )
  )


