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

  (define >-byte (bytes-ref #">" 0))
  (define nl-byte (bytes-ref #"\n" 0))

  (define (filter-port in)
    (let ([remain #f] [remain-start 0] [lines 1] [pos 1])
      (make-input-port
       (cons 'semilit (object-name in))
       (lambda (bs)
         (define len (bytes-length bs))
         (let loop ()
           (define remaining-bytes (and remain (- (bytes-length remain) remain-start)))
           (cond [(not remain)
                  (define line (read-bytes-line in))
                  (set! lines (add1 lines))
                  (cond ;; eof
                        [(eof-object? line) line]
                        ;; blank line, we read a
                        [(zero? (bytes-length line))
                         (bytes-set! bs 0 nl-byte)
                         (set! pos (add1 pos))
                         1]
                        ;; starts with >, put it in remain
                        [(equal? (bytes-ref line 0) >-byte)
                         (set! remain line) (set! remain-start 1)
                         (set! pos (add1 pos))
                         (loop)]
                        ;; starts with something else, treat like a blank line
                        [else
                         (bytes-set! bs 0 nl-byte)
                         (set! pos (+ pos 1 (bytes-length line)))
                         1])]
                 [(< remaining-bytes len)
                  (bytes-copy! bs 0 remain remain-start)
                  (set! pos (+ pos remaining-bytes))
                  (set! remain #f)
                  (set! remain-start 0)
                  remaining-bytes]
                 [else
                  (bytes-copy! bs 0 remain remain-start (+ remain-start len))
                  (set! pos (+ pos len))
                  (set! remain-start (+ remain-start len))
                  len])))
       #f ;; peek
       (λ () (close-input-port in))
       #f
       #f
       (λ () (values lines remain-start pos))
       (λ () (port-count-lines! in))
       7)))

  (define-values (sl-read sl-read-syntax sl-get-info)
    (make-meta-reader
     'semilit
     "language path"
     (λ (bstr)
       (let* ([str (bytes->string/utf-8 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-read
     wrap-read-syntax
     (λ (proc)
       (λ (key defval)
         (case key
           [else (if proc (proc key defval) defval)]))))))
