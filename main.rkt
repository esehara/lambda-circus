#lang racket/base

(require "circus.rkt")
(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(module+ test
  
  ;; Tests to be run with raco test

  
  ;; Strict beta reduction (call by value) 
  ;; ======================================
  
  ;; step by step test
  ;; -----------------
  
  ;; (λx.x) N -- beta1 -> N
  (check-equal? (beta1-> '((& x (dot x)) N)) '(N))

  ;; (λx.(λx.x)) z -- beta1 -> λx.x
  (check-equal? (beta1-> '((& x (dot & x (dot x))) z)) '(& x (dot x)))
 
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

  ;; (λx.x)((λx.x)(λz.(λx.z) z)) -- beta1 --> (λx.x)(λz.(λx.x) z)
  (check-equal? (beta1-> '((& y (dot y)) ((& x (dot x)) (& z (dot ((& x (dot x)) z))))))
                '((& x (dot x)) (& z (dot (( & x (dot x)) z)))))
  
  (check-equal? (beta1-> (beta1-> '((& y (dot y)) ((& x (dot x)) (& z (dot ((& x (dot x)) z)))))))
                '(& z (dot (( & x (dot x)) z))))

  (check-equal? (beta1-> (beta1-> (beta1-> '((& y (dot y)) ((& x (dot x)) (& z (dot ((& x (dot x)) z))))))))
                '(& z (dot (( & x (dot x)) z))))

  (check-equal? (beta-> '((& y (dot y)) ((& x (dot x)) (& z (dot ((& x (dot x)) z))))))
                '(& z (dot (( & x (dot x)) z))))
  ;; beta directly step check
  ;; ------------------------
  (check-equal? (beta-> '((& x ((& y (dot y x)) z)) u)) '(z u))

  ;;
  ;; non strict beta reduction
  ;; =========================

  ;; normal order
  (check-equal? (normal-order1-> '(& z (dot (& x (dot x)) z))) '(& z z))
  (check-equal? (normal-order1-> (normal-order1-> '((& x (dot x)) (& z (dot (& x (dot x)) z))))) '(& z z))
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
