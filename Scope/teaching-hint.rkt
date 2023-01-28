#lang typed/racket

;; which x does this expression return?
;; -- two design choices: static type overlopading; name equality

;; how many `let declaratuons are between the occurrence of `x` and its declaration?
;; -- unique anser 

(let ([x : Integer 10])
  (let ([y : Boolean #true])
    (let ([z : Char #\newline])
      (let ([x : String "hello"])
        (let ([w : Real #i1.1])
          (let ([u : Complex #i1.1+0i])
            (let ([v : Port (open-input-string "world")])
              x)))))))


(let ([x : Integer 10])
  (let ([y : Boolean #true])
    (let ([z : Char #\newline])
      (let ([x : String "hello"])
        (let ([w : Real #i1.1])
          (let ([u : Complex #i1.1+0i])
            (let ([v : Port (open-input-string "world")])
              (+ x 42))))))))

(let ([x : Integer 10])
  (let ([y : Boolean #true])
    (let ([z : Char #\newline])
      (let ([x : String "hello"])
        (let ([w : Real #i1.1])
          (let ([u : Complex #i1.1+0i])
            (let ([v : Port (open-input-string "world")])
              (string-append "well" x "isn't this cute"))))))))
