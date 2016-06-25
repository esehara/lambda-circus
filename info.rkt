#lang info
(define collection "lambda-circus")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/lambda-circus.scrbl" ())))
(define pkg-desc "Example lambda caluclation system about Beta Reduction System")
(define version "0.1")
(define pkg-authors '(esehara))
