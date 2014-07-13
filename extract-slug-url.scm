(use rfc.json)

(define (main . args)
  (let1 json (parse-json)
    (display (assoc-ref (assoc-ref json "blob") "url"))))
