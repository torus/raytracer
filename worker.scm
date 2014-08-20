;;; -*- mode: scheme; coding: utf-8 -*-

(use gauche.threads)
(use util.queue)

(use redis)
(use redis.async)

(define raytracer-mod (make-module #f))
(load "./raytracer.scm" :environment raytracer-mod)

(define spheres ((global-variable-ref raytracer-mod 'make-scene)))

; REDISCLOUD_URL=redis://rediscloud:lmzBksmjjRkVf2FI@pub-redis-10787.us-east-1-2.1.ec2.garantiadata.com:10787

(define *passwd* #f)

(define (connect-redis)
  (rxmatch-let (rxmatch #/(?:\w+):\/\/(?:\w+):(\w+)@([^:]+):(\d+)/
                        (sys-getenv "REDISCLOUD_URL"))
    (value passwd host port)
;    #?=passwd #?=host #?=port
    (set! *passwd* passwd)
    (redis-open-async host port)))

(define (gen-update-proc)
  (lambda (pub sub)
;    (print 100)
    (call/cc
     (lambda (yield)
       (define (wait-for-response proc)
;         (print 102)
         (call/cc (lambda (next)
;                    (print 103)
                    (call/cc (lambda (cont)
;                               (print 104)
                               (proc (lambda (res)
;                                       (print 105 res)
                                       (cont res)))
;                               (print 106)
                               (yield (lambda (pub sub) (next))))))))

       (worker-main wait-for-response pub sub)
;       (print 107)
       ))))

(define (worker-main yield pub sub)
  (print 0)
  (print "auth pub " (yield (redis-async-auth pub *passwd*)))
  (print 1)
  (print "auth sub " (yield (redis-async-auth sub *passwd*)))
  (print 2)

  (print (yield (redis-async-set pub "xxx" 123)))
  (print "yyyyyyy")
  (print (yield (redis-async-get pub "xxx")))

  (print 3)

  ;; #?=(yield (redis-async-subscribe sub "render-task"))

  ;; (let ((waiting #t))
  ;;   (redis-async-set-subscribe-handler!
  ;;    sub (lambda (res) #?=res (set! waiting #f)))

  ;;   (while waiting (yield values))

  ;;   (print (yield (redis-async-set pub "xxx" 123)))
  ;;   (print "yyyyyyy")
  ;;   (print (yield (redis-async-get pub "xxx")))
  ;;   )

  (sys-exit 0)
  )

#;(define update!
  (let ((proc (gen-update-proc)))
    (lambda (pub sub)
      (print proc)
      (set! proc (proc pub sub)))))

(define (main args)
  (let ((pub (connect-redis))
        (sub (connect-redis)))

    ((gen-update-proc) pub sub)

    (while #t
      ;; (until (queue-empty? (ref pub 'hndl-queue)) (redis-async-update! pub))
      ;; (until (queue-empty? (ref sub 'hndl-queue)) (redis-async-update! sub))
      (redis-async-update! pub)
      (redis-async-update! sub)
      ;(update! pub sub)
      (sys-nanosleep (expt 10 8)))      ; sleep 100ms
  ))
