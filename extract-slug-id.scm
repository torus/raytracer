(use rfc.json)

(define (main . args)
  (let1 json (parse-json)
    (display (assoc-ref json "id")))
  0)
