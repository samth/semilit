#lang racket/base

(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [sl-read read]
                       [sl-read-syntax read-syntax]
                       [sl-get-info get-info]))

  (define (wrap-read p)
    (lambda (in)
      (p (filter-port in))))

  (define (wrap-read-syntax p)
    (lambda (n in)
      (p n (filter-port in))))

  (define (filter-port in)
    (make-input-port 
     (cons 'semilit (object-name in))
     (lambda (...) ...)
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
