;; the buffer thing
;;
;; point - a pair (row . column), maybe
;; lines - a linked list of strings...?
(define-record-type buffer
  (buffer-constructor name)
  buffer?
  (name buffer-name set-buffer-name!)
  (point buffer-point set-buffer-point!)
  (lines buffer-lines set-buffer-lines!))

(define make-buffer (lambda (name)
                      (let ((buf (buffer-constructor name))
                            (new-list (java.util.LinkedList)))
                        (new-list:add "")
                        (set-buffer-lines! buf new-list)
                        (set-buffer-point! buf (cons 0 0))
                        buf)))

(defmacro with-buf-dsl (buf #!rest body)
  `(let* ((point (buffer-point ,buf))
          (lines (buffer-lines ,buf))
          (px (car point))
          (py (cdr point))
          (line (lines:get py)))
     ,@body))

(define insert-char
  (lambda (buf c)
    (with-buf-dsl buf
                  (let ((str1 (substring line 0 px))
                        (str2 (substring line px (string-length line))))
                    (lines:set py (string-append str1 c str2)))
                  (set-buffer-point! buf (cons (inc px) py)))))

(define insert-newline
  (lambda (buf)
    (with-buf-dsl buf
                  (lines:set py (substring line 0 px))
                  (lines:add (inc py)
                             (substring line px (string-length line)))
                  (set-buffer-point! buf (cons 0 (inc py))))))

(define remove-char
  (lambda (buf)
    (with-buf-dsl buf
                  (lines:set py (string-append
                                 (substring line 0 px)
                                 (substring line (inc px)
                                            (string-length line)))))))

(define remove-newline
  (lambda (buf)
    (with-buf-dsl buf
                  (let* ((next-line-number::int (inc py))      ;; kludge
                         (t (lines:get next-line-number)))
                    (lines:set py (string-append line t))
                    (lines:remove next-line-number)
                    (set-buffer-point! buf (cons px py))))))

(define remove-at-point
  (lambda (buf)
    (with-buf-dsl buf
                  (if (= px (string-length line))
                      (if (< py (dec (lines:size)))
                          (remove-newline buf))
                      (remove-char buf)))))

(define remove-char-backward
  (lambda (buf)
    (with-buf-dsl buf
                  (if (not (and (= px 0) (= py 0)))
                      (begin
                        (lines:set py (string-append
                                       (substring line 0 (dec px))
                                       (substring line px (string-length line))))
                        (set-buffer-point! buf (cons (dec px) py)))))))

(define remove-newline-backward
  (lambda (buf)
    (with-buf-dsl buf
                  (if (> py 0)
                      (let* ((t (lines:get (dec py)))
                             (new-x (string-length t))
                             (line-number::int py))
                        (lines:set (dec py) (string-append line t))
                        (lines:remove line-number) ;; kludge
                        (set-buffer-point! buf (cons new-x (dec py))))))))

(define remove-at-point-backward
  (lambda (buf)
    (with-buf-dsl buf
                  (if (zero? px)
                      (remove-newline-backward buf)
                      (remove-char-backward buf)))))

(define move-forward
  (lambda (buf)
    (with-buf-dsl buf
                  (cond ((= px (string-length line))
                         (if (not (= py (dec (lines:size))))
                             (set-buffer-point! buf (cons 0 (inc py)))
                             ;; beep?
                             ))
                        (else (set-buffer-point! buf (cons (inc px) py)))))))

(define move-backward
  (lambda (buf)
    (with-buf-dsl buf
                  (cond ((= px 0)
                         (if (not (zero? py))
                             (let ((new-x (string-length (lines:get (dec py)))))
                               (set-buffer-point! buf (cons new-x (dec py))))
                             ;; beep?
                             ))
                        (else (set-buffer-point! buf (cons (dec px) py)))))))

(define move-line-forward
  (lambda (buf)
    (with-buf-dsl buf
                  (if (not (= py (dec (lines:size))))
                      (let ((new-x (min px
                                        (string-length (lines:get (inc py))))))
                        (set-buffer-point! buf (cons new-x (inc py))))
                      ;; beep?
                      ))))

(define move-line-backward
  (lambda (buf)
    (with-buf-dsl buf
                  (if (not (zero? py))
                      (let ((new-x (min px
                                        (string-length (lines:get (dec py))))))
                        (set-buffer-point! buf (cons new-x (dec py))))
                      ;; beep?
                      ))))

(define move-to-bol
  (lambda (buf)
    (with-buf-dsl buf
                  (if (not (zero? px))
                      (set-buffer-point! buf (cons 0 py))))))

(define move-to-eol
  (lambda (buf)
    (with-buf-dsl buf
                  (let ((eol (string-length line)))
                    (if (< px eol)
                        (set-buffer-point! buf (cons eol py)))))))

;;;;;;;;;; test code
(define make-test-buffer (lambda ()
                           (let ((b (make-buffer "test")))
                             (insert-char b "a")
                             (insert-char b "a")
                             (insert-char b "a")
                             (insert-newline b)
                             (insert-char b "b")
                             (insert-char b "b")
                             (insert-newline b)
                             (insert-char b "c")
                             (insert-char b "c")
                             (insert-char b "c")
                             (insert-char b "c")
                             (insert-char b "c")
                             b)))
