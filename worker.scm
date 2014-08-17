;;; -*- mode: scheme; coding: utf-8 -*-

(use gauche.threads)
(use util.queue)

(use redis)
(use redis.async)

(define raytracer-mod (make-module #f))
(load "./raytracer.scm" :environment raytracer-mod)

(define spheres ((global-variable-ref raytracer-mod 'make-scene)))

; REDISCLOUD_URL=redis://rediscloud:lmzBksmjjRkVf2FI@pub-redis-10787.us-east-1-2.1.ec2.garantiadata.com:10787

(define (connect-redis)
  (rxmatch-let (rxmatch #/(?:\w+):\/\/(?:\w+):(\w+)@([^:]+):(\d+)/
                        (sys-getenv "REDISCLOUD_URL"))
    (value passwd host port)
;    #?=passwd #?=host #?=port
    (let1 redis (redis-open-async host port)
      ((redis-async-auth redis passwd) (lambda (res) (print #`"auth response: ,res")))
      redis)))

(define (gen-update-proc)
  (define (wait-for-response proc yield next)
    (call/cc (lambda (cont)
               (proc (lambda (res)
                       (cont res)))
               (yield (lambda (pub sub) (next))))))

  (lambda (pub sub)
    (call/cc
     (lambda (yield)
       (call/cc
        (lambda (next)
          (print (wait-for-response (redis-async-set pub "xxx" 123) yield next)
                 )))
       (print "yyyyyyy")
       (call/cc
        (lambda (next)
          (print (wait-for-response (redis-async-get pub "xxx") yield next)
                 )))
       ))))

(define update!
  (let ((proc (gen-update-proc)))
    (lambda (pub sub)
      (set! proc (proc pub sub)))))

(define (main args)
  (let ((pub (connect-redis))
        (sub (connect-redis)))

    (while #t
      (queue-length (ref pub 'hndl-queue))
      (until (queue-empty? (ref pub 'hndl-queue)) (redis-async-update! pub))
      (until (queue-empty? (ref sub 'hndl-queue)) (redis-async-update! sub))
      ;; (redis-async-update! pub)
      ;; (redis-async-update! sub)
      (update! pub sub)
      (sys-nanosleep (expt 10 6)))      ; sleep 1ms
  ))
