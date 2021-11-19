#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (myresponse request)
  ;; We extract the key/value pairs (if present):
  (define bindings (request-bindings request))


  ;; If there is a "name" key, we print "Hi (name)"
  (cond ((exists-binding? `name bindings)
         (define myname (extract-binding/single `name bindings))
         (define mysurname (extract-binding/single `surname bindings))
         (response/xexpr
          `(html (head (title "Simple Page"))
                 (body
                  (h1 "A Simple Dynamic page")
                  (div ((class "name"))
                       (p "Hi " ,myname "  ",mysurname))
                  
                  ))))
        (else
         ;; If there is no "name", we generate a form:
         (response/xexpr
          `(html (head (title "Simple Page"))
                 (body
                  (h1 "A Simple Dynamic page")
                  (form
                   (input ((name "name")))
                   (input ((name "surname")))
                   (input ((type "submit"))))))))
        )
  )



(serve/servlet myresponse)