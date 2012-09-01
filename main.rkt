#lang racket/base

(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [sl-read read]
                       [sl-read-syntax read-syntax]
                       [sl-get-info get-info]))

  (define (wrap-read p)
    (lambda (in . args)
      (apply p (filter-port in) args)))

  (define (wrap-read-syntax p)
    (lambda (n in . args)
      (apply p n (filter-port in) args)))

  (define <-byte 62)

  (define (filter-port in)
    (make-input-port 
     (cons 'semilit (object-name in))
     (let ([remain #f] [remain-start 0])
       (lambda (bs) 
         (define len (bytes-length bs))
         (eprintf ">>> bs-len ~a\n" len)
         (let outer ()
           (cond [(and remain (<= (- (bytes-length remain) remain-start) len))
                  (eprintf ">> more to go but it all fits\n")
                  (bytes-copy! bs 0 remain remain-start)
                  (begin0
                      (- (bytes-length remain) remain-start)
                    (set! remain-start 0) 
                    (set! remain #f))]
                 [remain
                  (eprintf ">> more to go and it won't fit\n")
                  (bytes-copy! bs 0 remain remain-start (+ remain-start len))
                  (set! remain-start (+ remain-start len)) 
                  len]
                 [else
                  (let inner ()
                    (define line (read-bytes-line in))
                    (eprintf ">>> line ~a\n" line)
                    (cond 
                      [(eof-object? line) line]
                      [(zero? (bytes-length line))
                       (bytes-copy! bs 0 (string->bytes/utf-8 "\n"))
                       1]
                      [(equal? (subbytes line 0 1) (string->bytes/utf-8 ">"))
                       (set! remain line) (set! remain-start 1)
                       (outer)]
                      [else (inner)]))]))))
     #f ;; peek
     (lambda () (close-input-port in))))

  (define-values (sl-read sl-read-syntax sl-get-info)
    (make-meta-reader
     'semilit
     "language path"
     (lambda (str)
       (let ([s (string->symbol
                 (string-append (bytes->string/utf-8 str)
                                "/lang/reader"))])
         (and (module-path? s) s)))
     wrap-read
     wrap-read-syntax
     (lambda (proc)
       (lambda (key defval)
         (case key
           [else (if proc (proc key defval) defval)]))))))
