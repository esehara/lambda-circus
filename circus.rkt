#lang racket

(provide beta->)
(provide beta1->)
(provide normal-order1->)

(define (fix-reduction l-term)
  (if (and (list? (car l-term))
           (= 1 (length l-term)))
      (car l-term) l-term))

(define (normal-order1-> l-term)
  (if (not (beta1? l-term)) (fix-reduction (beta1-> l-term))
      (let* ([prev-body (take l-term 2)]
             [body (filter lambda-not-dot? (third l-term))]
             [body-beta1 (beta1-> body)])
            (append prev-body body-beta1))))
                  
(define (beta-> l-term)
  (simple-loop-beta-check '() l-term))

(define (simple-loop-beta-check prev-term next-term)
  (if (equal? prev-term next-term) prev-term
      (simple-loop-beta-check next-term (beta1-> next-term))))

(define (get-lambda-symbol l-term)
  (if (list? (car l-term)) (car l-term) l-term))

(define (beta1? l-term)
  (or
   (not (lambda-type? (get-lambda-symbol l-term)))
   (and (symbol? (car l-term)) (null? (drop l-term 3)))
   (null? (car l-term))))

(define (beta1-> l-term)
  (if (beta1? l-term) l-term
      (let ([result (apply->lambda (car l-term) (cdr l-term))])
        (if (and (list? (car result))
                 (list? (caar result))) (car result) result))))

(define (lambda-type? l)
  (and (symbol? (car l)) (symbol=? '& (car l))))

(define (lambda-param l)
  (cond [(and
          (symbol? (first l))
          (< 3 (length l))
          (symbol=? (first l) 'dot)) (third l)]
        [(and
          (symbol? (first l))
          (symbol=? (first l) '&)) (second l)]))

(define (lambda-not-dot? x)
  (or (not (symbol? x))
      (not (symbol=? 'dot x))))

(define (apply->lambda l-term params)
  (cond [(null? params) l-term]
        [else (let* ([param-name (second l-term)]
             [body       (third  l-term)]
             [replace-result (if (and (lambda-type? (cdr body))
                                      (symbol=? (lambda-param body) param-name))
                                 body
                                 (replace-node body param-name (car params)))])
                (println param-name)
                (filter lambda-not-dot?
                        (append
                         (if (symbol? replace-result)
                             (list replace-result) replace-result)
                         (cdr params))))]))

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
  )