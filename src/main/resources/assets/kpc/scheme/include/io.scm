(define get (lambda ()
                (define s (os:pull))
                (cond ((string=? (s:name) "char")
                       (cond ((string=? ((s:args) 0) "__enter__") '())
                             ((string=? ((s:args) 0) "__back__")
                              (term:backspace)
                              (term:setCursorPos (- (term:getCursorX) 1) (term:getCursorY)) -1)
                             (else
                                 (term:write ((s:args) 0))
                                 (term:setCursorPos (+ (term:getCursorX) 1) (term:getCursorY))
                                 ((s:args) 0))))
                      (else `()))))

(define gets (lambda (s)
                 (define c (get))
                 (cond ((null? c) s)
                       ((eq? c -1)
                        (gets (substring s 0 (- (string-length s) 1))))
                       (else
                           (gets (string-append s c))))))

(define print (lambda (s)
                  (term:write s)
                  (term:setCursorPos (+ (term:getCursorX) (string-length s)) (term:getCursorY))))

(define println (lambda (s)
                    (term:write s)
                    (term:setCursorPos 0 (+ (term:getCursorY) 1))))