(use srfi-27)
(use gauche.parseopt)
(use gauche.threads)
(use makiki)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (atom '())
                       :document-root "./public"))
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
        (let1 result (respond/ok req `(file ,outfile) :content-type "image/jpg")
          (sys-unlink outfile)
          result)
      ))))

(define raytracer-mod (make-module #f))
(load "./raytracer.scm" :environment raytracer-mod)

(define-http-handler "/render.ppm"
  (^[req app]
    (random-source-randomize! default-random-source)
    (let ([param-size (request-param-ref req "size")]
          [param-frame (request-param-ref req "frame")])
      (let ([opt-size (if param-size #`"-s ,param-size" "")]
            [opt-frame (if param-frame #`"-f ,param-frame" "")]
            [outfile #`"out,(random-integer 1000000).jpg"])
        (respond/ok req
                    (with-output-to-string
                      (lambda ()
                        ((global-variable-ref raytracer-mod 'main)
                         `(self ,@(string-split opt-size #/\s+/)
                                ,@(string-split opt-frame #/\s+/)))))
                    :content-type "image/x-portable-pixmap")))))

(define-http-handler "/exit"
  (^[req app]
    (sys-exit 0)))

(define-http-handler "/sys"
  (^[req app]
    (sys-system "ls")
    (respond/ok req "ok")))

(define-http-handler #// (file-handler))
