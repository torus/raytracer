(use srfi-27)
(use gauche.parseopt)
(use gauche.threads)
(use makiki)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (atom '())))
  0)

(define-http-handler "/render.jpg"
  (^[req app]
    (random-source-randomize! default-random-source)
    (let ([param-size (request-param-ref req "size")]
          [param-frame (request-param-ref req "frame")])
      (let ([opt-size (if param-size #`"-s ,param-size" "")]
            [opt-frame (if param-frame #`"-f ,param-frame" "")]
            [outfile #`"out,(random-integer 1000000).jpg"])
        (sys-system
         #`"gosh raytracer.scm ,opt-size ,opt-frame | ppmtojpeg -quality 100 > ,outfile")
        (respond/ok req `(file ,outfile) :content-type "image/jpg")
      ))))

(define-http-handler "/exit"
  (^[req app]
    (sys-exit 0)))

(define-http-handler "/"
  (^[req app]
    (let ([width (x->number (or (request-param-ref req "width") 640))]
          [height (x->number (or (request-param-ref req "height") 480))]
          [rows (x->number (or (request-param-ref req "rows") 1))]
          [cols (x->number (or (request-param-ref req "cols") 1))])
      (define (img-src row col)
        #`"/render.jpg?size=,|width| ,|height|&frame=,(* col (/ width cols)) ,(* row (/ height rows)) ,(/ width cols) ,(/ height rows)"
        )
      (respond/ok
       req
       `(sxml (table
               ,@(map (^[row]
                        `(tr ,@(map (^[col]
                                      `(td (img (@ (src ,(img-src row col)))))
                                      ) (iota cols)))
                        ) (iota rows))
               )))

    )))
