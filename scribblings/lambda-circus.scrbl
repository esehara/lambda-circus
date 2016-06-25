#lang scribble/manual
@require[@for-label[lambda-circus
                    racket/base]]

@title{lambda-circus}
@author{esehara}

@defmodule[lambda-circus]

@section{ What's it? }

This package is "Example lambda caluclation system about Beta Reduction System".

@section{ Lambda syntax in Module }

@itaric{Warning: I'm not native english writer.then, this sentence is probably very odd. :( ) } 

In module, @bold{Lambda syntax} is:

@(racketblock
  ;; λx.x
  '(& x (dot x)))

or

@(rancketblock
  '(& x (x)))

In Packeage, @racket['&] is a substitute for lambda.

@defproc[(beta1-> [lambda-list list?]) beta-reducted-list]

Example:

@(racketblock
  (beta1-> '((& x (dot x)) N))
  ;; => '(N)
  )

@(racketblock
  (beta1-> '((& x (dot x)) N))
  ;; => '(N)
  )

@(racketblock
  (beta1-> (beta1-> '((& x ((& y (dot y x)) z)) u)))
  ;; => '(z u))
  )

@defproc[(beta-> [lambda-list list?]) beta-reducted-list]

Example:

@(racketblock
  (beta-> '((& x ((& y (dot y x)) z)) u))
  ;; => '(z u))
  )

@defproc[(normal-order1-> [lambda-list list?]) normal-order-reducted]

In this system, @packet[beta->] function adopt @bold{call-by-value} strategy. but, Reduction strategy has another strategy. Typycally @bold{normal order}.What's difference? See under code.

@(racketblock
  (normal-order1-> '(& z (dot (& x x) z)))
  ;;'(& z z)
  )

@bold{normal order} demand reduction @racket['(dot & x x) z]. but, @{call-by-value} has not to reduce it.

@(racketblock
  (beta1-> '(& z (dot (& x x) z)))
  ;;'(& z (dot (& x x) z))
  )