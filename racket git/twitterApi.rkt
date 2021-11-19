#lang racket

(require "oauth-single-user.rkt")

(define myAuthenticationParameters (new oauth-single-user%
                                        [consumer-key "kAdo6x6SlCNeXXpsvqm1xAGEw"] ;; API key
                                        [consumer-secret
                                         "X1Dpjh484UsHCTqzWaALwTkEZuJHDsyPtZcxhAR4YO34JniIkt"] ;; API secret
                                        [access-token
                                         "1272850793717260288-h5xFWClNXr8uKD4XHEHeEguo0RBmk3"] ;; access token
                                        [access-token-secret
                                         "52CndVvU02eu3vZGyqEOOh6GgHdQ3cUPPGEGf3IOHVfZi"] ;; access token secret
                                        ))

(send myAuthenticationParameters
      post-request "https://api.twitter.com/1.1/statuses/update.json"
      (list (cons 'status "middlesexuni tes")))
