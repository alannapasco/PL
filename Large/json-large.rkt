#lang racket 

(require json)

(define (! n) (if (= n 0) 1 (* n (! (sub1 n)))))

(define x (! 10000))

(with-output-to-file "alanna.json" #:exists 'replace (λ () (write-json x)))