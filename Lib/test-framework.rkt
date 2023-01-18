#lang typed/racket

(provide
 #; (test main in expected msg)
 ;; converts `in` to string, feeds it to main, convert output back to JSExpr,
 ;; checks the actual outcome against the expected result ("loud" if failed)
 ;; 
 ;; EFFECT record `in` in "*-in.json" file, output in "-out.json" (if any)
 test)


;; -----------------------------------------------------------------------------
(require typed/json)
(require typed/rackunit)

;; -----------------------------------------------------------------------------
(: test (->* [ (-> Any) JSExpr Any String ] [(-> Any Boolean)] Any))
(define (test main in expected msg [handle? (λ _ #false)])
  (with-handlers ([handle? void])
    (define-values [record-in record-out] (make-record))
    (define in-json (jsexpr->string (record-in in)))
    (define actual  (record-out (string->jsexpr (redirect main in-json))))
    (check-equal? actual expected msg)))

(: make-record (-> (Values (JSExpr -> JSExpr) (JSExpr -> JSExpr))))
;; make two recording functions, one for input and one for output (*counter)
;; EFFECT bump counter 
(define *counter 0)
(define (make-record)
  (define in-file  (~a *counter "-in.json"))
  (define out-file (~a *counter "-out.json"))
  (set! *counter (add1 *counter))
  (values (make-one in-file) (make-one out-file)))

(: make-one (String -> (-> JSExpr JSExpr)))
;; make a recording function that puts `j` into the file named `name` 
(define [(make-one name) j]
  (with-output-to-file name #:exists 'replace (λ () (write-json j)))
  j)
  
(: redirect ((-> Any) String -> String))
;; redirect string into `main` and catch output as string 
(define (redirect main in-json)
  (with-output-to-string (λ () (with-input-from-string in-json main))))