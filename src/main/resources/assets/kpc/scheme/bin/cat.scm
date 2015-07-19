(define filePath (args 0))

(define consume (lambda (file)
                    (define line (file:readLine))
                    (cond ((= -1 line) '())
                          (else
                              (println line)
                              (term:setCursorPos (+ (term:getCursorX) 1) (term:getCursorY))
                              (consume file)))))

(cond ((fs:exists filePath)
       (define file (fs:read filePath))
       (consume file) '())
      (else (println (string-append "File not found: " filePath))))