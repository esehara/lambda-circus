#lang racket

(provide beta->)
(provide beta1->)
(provide normal-order1->)

(define (normal-order1-> l-term)
  (if (beta1? l-term) (beta1-> l-term)
      (let* ([prev-body (take l-term 2)]
             [body (filter lambda-not-dot? (third l-term))]
             [body-beta1 (beta1-> body)])
        (append prev-body body-beta1))))
        
          
(define (beta-> l-term)
  (simple-loop-beta-check '() l-term))

(define (simple-loop-beta-check prev-term next-term)
  (if (equal? prev-term next-term)
      (raise (string-append
              "This beta reduction is infinity:"
              (list->string prev-term)))
      (with-handlers ([exn:fail? (lambda (x) next-term)])
        (simple-loop-beta-check next-term (beta1-> next-term)))))

(define (beta1? l-term)
  (and (list? (car l-term))
   (or (not (lambda-type? (car l-term)))
       (null? (car l-term)))))

(define (beta1-> l-term)
  (if (beta1? l-term) l-term
      (apply->lambda (car l-term) (cdr l-term))))

(define (lambda-type? l) (symbol=? '& (car l)))
(define (lambda-not-dot? x)
  (or (not (symbol? x))
      (not (symbol=? 'dot x))))

(define (apply->lambda l-term params)
  (if (null? params) l-term
      (let* ([param-name (second l-term)]
             [body       (third  l-term)]
             [replace-result (replace-node body param-name (car params))])
        (filter lambda-not-dot?
                (append
                 (if (symbol? replace-result)
                     (list replace-result) replace-result)
                  (cdr params))))))

(define (replace-node body param-name replace)
  (replace-node-process body '() param-name replace))

(define (replace-node-process list-process result param-name replace)
  (cond [(null? list-process) result]
        [(and
          (list? list-process)
          (list? (car list-process)))
         (let ([list-result 
                (list (replace-node (car list-process) param-name replace))])
           (replace-node-process (cdr list-process)
                                 (append result list-result)
                                 param-name replace))]
        [else (let* ([target (if
                              (list? list-process)
                              (car list-process)
                              list-process)]
                     [result-symbol
                      (if (symbol=? param-name target) replace target)])
                (if (list? list-process)
                    (replace-node-process (cdr list-process)
                                      (append result (list result-symbol))
                                      param-name replace)
                    result-symbol))]))


(module+ test
  (require rackunit)
  (require test-engine/racket-tests)
  
  ;; lambda-term check
  (check-true (lambda-type? '(& x (dot x))))

  ;; replace node valiables
  (check-equal? (replace-node '(a b c) 'a 'N) '(N b c))
  (check-equal? (replace-node '(a b (a b c) c) 'b 'M) '(a M (a M c) c)) 
  
  ;; beta-reduction step by step check

  ;; (λx.x) N -- beta1 -> N
  (check-equal? (beta1-> '((& x (dot x)) N)) '(N))

  ;; (λx.x(xy)N -- beta1 -> N(Ny)
  (check-equal? (beta1-> '((& x (dot x (x y))) N)) '(N (N y)))

  ;; (λx.(λy.yx)z)v -- beta1 --> (λy.yu)z -- beta1 --> (zu)
  (check-equal? (beta1-> '((& x ((& y (dot y x)) z)) u)) '((& y (dot y u)) z))
  (check-equal? (beta1-> (beta1-> '((& x ((& y (dot y x)) z)) u))) '(z u))

  ;; (λx.y)N -- beta1 --> y
  (check-equal? (beta1-> '((& x (dot y)) N)) '(y))

  ;; (λx.xx)(λx.xx) -- beta1 --> (λx.xx)(λx.xx)
  (check-equal? (beta1->
                 '((& x (dot x x)) (& x (dot x x))))
                 '((& x (dot x x)) (& x (dot x x)))) 

  ;; (λx.xxy)(λx.xxy) -- beta1 --> (λx.xxy)(λ.xxy)y
  (check-equal? (beta1->
                 '((& x (dot x x y)) (& x (dot x x y))))
                 '((& x (dot x x y)) (& x (dot x x y)) y))
  
  ;; (λx.xxy)(λx.xxy)y -- beta1 --> (λx.xxy)(λ.xxy)yy
  (check-equal? (beta1->
                 '((& x (dot x x y)) (& x (dot x x y)) y))
                 '((& x (dot x x y)) (& x (dot x x y)) y y))

  ;; beta directly step check
  (check-equal? (beta-> '((& x ((& y (dot y x)) z)) u)) '(z u))

  )