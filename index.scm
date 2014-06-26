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
