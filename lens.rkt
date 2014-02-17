#lang racket/base

(require racket/function
         racket/list
         racket/dict)

(provide (struct-out lens)
         lens-get
         lens-mod
         lens-set
         lens-set!
         lens-setter
         compose-lens
         car-l
         cdr-l
         mapped
         struct-lens)

(struct lens (getter modifier))


(define (lens-get obj l)
  ((lens-getter l) obj))

(define (lens-mod obj l func)
  (((lens-modifier l) func) obj))

(define (lens-set obj l val)
  (lens-mod obj l (const val)))

(define-syntax-rule (lens-set! obj l val)
  (set! obj (lens-set obj l val)))

(define (((lens-setter l) val) obj)
  (lens-set obj l val))

(define (compose-lens . lenses)
  (define (cartesian-compose . fs)
    (define (cartesian-composable f)
      (λ args (apply values (append* (map (λ (arg) (call-with-values (λ () (f arg)) list)) args)))))
    (apply compose (map cartesian-composable fs)))
  (lens
   (if (andmap lens-getter lenses)
    (apply cartesian-compose (map lens-getter (reverse lenses)))
    #f)
   (if (andmap lens-modifier lenses)
    (apply compose (map lens-modifier lenses))
    #f)))

(define car-l
  (lens car
        (λ (f) (λ (o)
                 (cons (f (car o)) (cdr o))))))

(define cdr-l
  (lens cdr
        (λ (f) (λ (o)
                 (cons (car o) (f (cdr o)))))))

(define both-l
  (lens (λ (o) (values (car o) (cdr o)))
        (λ (f) (λ (o)
                 (cons (f (car o)) (f (cdr o)))))))

(define mapped
  (lens #f
        (curry map)))

#;(define (at key)
  (lens (λ (o) (dict-ref o key values))
        #;(λ (f) (λ (o) (dict-set ))) #f)) ;; Representational deficiency: Need to be able to take -no-value- as an argument

(define (at key)
  (lens (λ (o) (dict-ref o key values))
        (λ (f) (λ (o)
                 (define cur (let* ([uniq (gensym)]
                                    [res (dict-ref o key (λ () uniq))])
                               (if (eq? res uniq)
                                   (list)
                                   (list res))))
                 (call-with-values (λ () (apply f cur))
                                   (λ f-result
                                     (if (null? f-result)
                                         (dict-remove o key)
                                         (dict-set o key (car f-result)))))))))

(define-syntax struct-lens
  (syntax-rules ()
    [(_ struct-name struct-field)
     (lens
      struct-field
      (lambda (func)
        (lambda (obj) (struct-copy struct-name [struct-field (func (struct-field obj))]))))]))

