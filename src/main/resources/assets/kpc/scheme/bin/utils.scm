(define inc (lambda (n) (+ n 1)))
(define dec (lambda (n) (- n 1)))

(define-syntax dotimes
  (syntax-rules ()
    ((_ n body ...)
     (do ((i n (- i 1)))
         ((not (< 0 i)))
       body ...))))

;; threading macro from clojure :D
(defmacro -> (v #!rest forms)
  (if (eq? forms ())
      `,v
      `(-> (,(caar forms) ,v ,@(cdar forms))
           ,@(cdr forms))))
