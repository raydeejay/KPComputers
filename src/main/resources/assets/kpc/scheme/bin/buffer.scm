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

(define insert-string (lambda (buf str)
                        (string-for-each (lambda (c) (insert-char buf (string c))) str)))

(define insert-line (lambda (buf str)
                      (insert-string buf str)
                      (insert-newline buf)))

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

(define *overwrite-mode* #f)
(define toggle-overwrite-mode (lambda (dummy)
                                (set! *overwrite-mode* (not *overwrite-mode*))))

(define replace-char-at-point
  (lambda (buf c)
    (with-buf-dsl buf
                  (if (< px (string-length line))
                      (remove-char buf))
                  (insert-char buf c))))

(define insert-or-replace-char
  (lambda (buf c)
    (if *overwrite-mode*
        (replace-char-at-point buf c)
        (insert-char buf c))))

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

;;;;;;;;;; scratch buffer
(define scratch-buffer-default-text
  '(";; Welcome to AGITE, which is meant to be A Gentle Introduction to Emacs."
    ";;"
    ";; This may eventually become a buffer for evaluating scheme expressions,"
    ";; but for now, it serves as a kickstart to the keys that AGITE uses."
    ";;"
    ";; Use the arrow keys the move the point (cursor). Type to enter text."
    ";; Delete and Backspace do what you would expect. Insert toggles overwriting."
    ";;"
    ";; C-m (Control+m) will open the menu. Use it to save your work, create a new"
    ";; file, load other files, or switch to the next buffer. Use the menu :D"
    ";;"
    ";; One interesting characteristic of Agite is that it's designed to run"
    ";; as well on desktop computers running Java, in either GUI or text"
    ";; environments, as inside computers simulated by the KPC mod for"
    ";; Minecraft (link here)."
    ";;"
    ";; Remember that AGITE is still in a very early stage."
    ""))

(define make-scratch-buffer (lambda ()
                              (let ((buf (make-buffer "*scratch*")))
                                (for-each (lambda (each)
                                            (insert-string buf each)
                                            (insert-newline buf))
                                          scratch-buffer-default-text)
                                buf)))
