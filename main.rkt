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
    (let ([remain #f] [remain-start 0] [lines 0] [pos 0])     
      (make-input-port 
       (cons 'semilit (object-name in))
       (lambda (bs) 
         (define len (bytes-length bs))
         (let outer ()
           (cond [(and remain (< (- (bytes-length remain) remain-start) len))
                  (bytes-copy! bs 0 remain remain-start)
                  (bytes-set! bs (sub1 len) nl-byte)
                  (set! pos (+ pos (add1 (- (bytes-length remain) remain-start))))
                  (begin0 (add1 (- (bytes-length remain) remain-start))
                          (set! remain-start 0) 
                          (set! remain #f))]
                 [remain
                  (bytes-copy! bs 0 remain remain-start (+ remain-start len))
                  (set! remain-start (+ remain-start len))
                  (set! pos (+ pos len))
                  len]
                 [else
                  (let inner ()
                    (define line (read-bytes-line in))
                    (set! lines (add1 lines))
                    (cond 
                      [(eof-object? line) line]
                      [(zero? (bytes-length line))
                       (bytes-set! bs 0 nl-byte)
                       (set! pos (add1 pos))
                       1]
                      [(equal? (bytes-ref line 0) >-byte)
                       (set! remain line) (set! remain-start 1)
                       (set! pos (add1 pos))
                       (outer)]
                      [else (set! pos (+ pos 1 (bytes-length line))) (inner)]))])))
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
