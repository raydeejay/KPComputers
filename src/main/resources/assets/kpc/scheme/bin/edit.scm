;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w (term:getWidth))
(define h (term:getHeight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x 1)
(define y 1)
(define scrollX 0)
(define scrollY 0)
(define buffer (java.util.LinkedList))
(define filePath (args 0))
(define menu #f)
(define menu-item 0)
(define running #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inc (lambda (n) (+ n 1)))
(define dec (lambda (n) (- n 1)))

(define eof? (lambda (line) (= -1 line)))

(define clear-term (lambda ()
                     (term:clear)
                     (term:setCursorPos 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers (one, at least)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define point (cons 1 1))

(define point-x (lambda () (car point)))
(define point-y (lambda () (cdr point)))

(define set-point-x (lambda (x))
  (set-car! point x))

(define set-point-y (lambda (y))
  (set-cdr! point y))

(define set-point (lambda (x y))
  (set-point-x x)
  (set-point-y y))

(define idx->point (lambda (n) (inc n)))
(define point->idx (lambda (n) (dec n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading and saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define consume (lambda (file acc)
                  (let ((line (file:readLine)))
                    (if (eof? line)
                        acc
                        (do (acc:add line)
                            (consume file acc))))))

(define load (lambda (path)
               (when (fs:exists path)
                     (set! buffer (consume (fs:read path)
                                           (java.util.LinkedList))))))

(define produce (lambda (file index buf)
                  (unless (< index (buf:size))
                          (let ((line (buf:get index)))
                            (file:write (string-append line "\n"))
                            (produce file (inc index) buf)))))

(define save (lambda (path)
               (produce (fs:open path) 0 buffer)
               (set! menu #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing to the terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-lines (lambda (index buf)
                      (unless (or (= index (dec (point->idx h)))
                                  (= index (dec (buf:size))))
                              (term:setCursorPos 1 (idx->point index))
                              (term:clearLine)
                              (term:write (buf:get (+ index scrollY)))
                              (write-lines (inc index) buf))))

(define draw-text (lambda ()
                    (write-lines y buffer)
                    (term:setCursorPos (- x scrollX) (- y scrollY))))

(define draw-line (lambda (n)
                    (term:setCursorPos (- 1 scrollX) (- (inc n) scrollY))
                    (term:clearLine)
                    (term:write (buffer:get n))
                    (term:setCursorPos (- x scrollX) (- (inc n) scrollY))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define draw-item (lambda (item)
                    (term:write item)
                    (term:setCursorPos (+ (term:getCursorX) (string-length item))
                                       (term:getCursorY))))

(define draw-item-selected (lambda (item)
                             (draw-item (string-append "[" item "]"))))

(define draw-menu (lambda (m mi)
                    (cond (m
                           (if (zero? mi)
                               (draw-item-selected "Save")
                               (draw-item "Save"))
                           (if (= mi 1)
                               (draw-item-selected "Exit")
                               (draw-item "Exit")))
                          (else
                           (draw-item "Press Ctrl for menu")))))

(define draw-modeline (lambda ()
                        (let ((lnStr (string-append "Ln "
                                                    (java.lang.String:valueOf y))))
                          (term:setCursorPos 1 (dec h))
                          (term:clearLine)
                          (term:setCursorPos (- w (string-length lnStr)) (dec h))
                          (term:write lnStr))
                        (term:setCursorPos 1 (dec h))
                        (draw-menu menu menu-item)
                        (term:setCursorPos (- x scrollX) (- y scrollY))))

(define do-menu-function (lambda (func)
                           (cond ((zero? func)
                                  (save filePath))
                                 ((= func 1)
                                  (set! running #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define setCursorPos (lambda (sx sy)
                       (define screenX (- sx scrollX))
                       (define screenY (- sy scrollY))
                       (define redraw #f)
                       (cond ((< screenX 1)
                              (set! scrollX (dec sx))
                              (set! screenX 1)
                              (set! redraw #t))
                             ((> screenX w)
                              (set! scrollX (- sx w))
                              (set! screenX w)
                              (set! redraw #t)))
                       (cond ((< screenY 1)
                              (set! scrollY (dec sy))
                              (set! screenY 1)
                              (set! redraw #t))
                             ((> screenY (dec h))
                              (set! scrollY (- sy (dec h)))
                              (set! screenY (dec h))
                              (set! redraw #t)))
                       (cond (redraw
                              (draw-text)))
                       (term:setCursorPos screenX screenY)
                       (draw-modeline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter (lambda ()
                       (cond ((not menu)
                              (let* ((bufx (point->idx x))
                                     (bufy (point->idx y))
                                     (line (buffer:get bufy)))
                                (buffer:set bufy (str:sstring line 0 bufx))
                                (buffer:add (inc bufy)
                                            (str:sstring line (inc bufx)
                                                         (string-length line))))
                              (draw-text)
                              (set! x 1)
                              (set-point-x 1)
                              (set! y (inc y))
                              (set-point-y (inc y))
                              (setCursorPos x y))
                             (else
                              (do-menu-function menu-item)))
                       (draw-modeline)))

(define handle-back (lambda ()
                      (unless (= x y 1)
                              (cond ((> x 1)
                                     (define line (buffer:get (point->idx y)))
                                     (define newLine (str:combine (str:sstring line 0 (- x 2))
                                                                  (str:sstring line x)))
                                     (buffer:set (point->idx y) newLine)
                                     (draw-line (point->idx y))
                                     (set! x (dec x))
                                     (set-point-x (dec x))
                                     (setCursorPos x y))
                                    ((> y 1)
                                     (define prevLine (string-length (buffer:get (- y 2))))
                                     (define newLine (str:combine (buffer:get (- y 2))
                                                                  (buffer:get (dec y))))
                                     (buffer:set (dec y) newLine)
                                     (buffer:remove (dec y))
                                     (draw-text)
                                     (set! x (inc prevLine))
                                     (set-point-x (inc prevLine))
                                     (set! y (dec y))
                                     (set-point-y (dec y))
                                     (setCursorPos x y))))))

(define handle-up (lambda ()
                    (cond ((> y 1)
                           (set! y (dec y))
                           (set-point-y (dec y))
                           (setCursorPos x y)))))

(define handle-down (lambda ()
                      (cond ((< y (- (buffer:size) 1))
                             (set! y (inc y))
                             (set-point-y (inc y))
                             (setCursorPos x y)))))

(define handle-left (lambda ()
                      (cond ((not menu)
                             (cond ((> x 1)
                                    (set! x (dec x))
                                    (set-point-x (dec x)))
                                   ((> y 1)
                                    (set! x (string-length (buffer:get (dec (point->idx y)))))
                                    (set-point-x (string-length (buffer:get (dec (point->idx y)))))
                                    (set! y (dec y))
                                    (set-point-y (dec y)))))
                            (else
                             (set! menu-item (dec menu-item))
                             (cond ((< menu-item 0)
                                    (set! menu-item 1)))
                             (draw-modeline)))
                      (setCursorPos x y)))

(define handle-right (lambda ()
                       (cond ((not menu)
                              (cond ((< x (inc (string-length (buffer:get (point->idx y)))))
                                     (set! x (inc x))
                                     (set-point-x (inc x)))
                                    ((< y (buffer:size))
                                     (set! x 1)
                                     (set-point-x 1)
                                     (set! y (inc y))
                                     (set-point-y (inc y)))))
                             (else
                              (set! menu-item (inc menu-item))
                              (cond ((> menu-item 1)
                                     (set! menu-item 1)))
                              (draw-modeline)))
                       (setCursorPos x y)))

(define handle-tab (lambda ()
                     (define s (buffer:get (point->idx y)))
                     (buffer:set (point->idx y) (str:combine "  " s))
                     (set! x (+ x 2))
                     (set-point-x (+ x 2))
                     (setCursorPos x y)
                     (draw-line (point->idx y))))

(define handle-ctrl (lambda ()
                      (set! menu (not menu))
                      (draw-modeline)))

(define handle-key (lambda ()
                     (define s (buffer:get (point->idx y)))
                     (define newLine (str:combine (str:sstring s 0 (point->idx x))
                                                  ((e:args) 0)
                                                  (str:sstring s x)))
                     (buffer:set (point->idx y) newLine)
                     (draw-line (point->idx y))
                     (set! x (inc x))
                     (set-point-x (inc x))
                     (setCursorPos x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define run (lambda ()
              (cond (running
                     (define e (os:pull))
                     (cond ((string=? (e:name) "char")
                            (cond ((string=? ((e:args) 0) "__enter__")
                                   (handle-enter))
                                  ((string=? ((e:args) 0) "__back__")
                                   (handle-back))
                                  ((string=? ((e:args) 0) "__up__")
                                   (handle-up))
                                  ((string=? ((e:args) 0) "__down__")
                                   (handle-down))
                                  ((string=? ((e:args) 0) "__left__")
                                   (handle-left))
                                  ((string=? ((e:args) 0) "__right__")
                                   (handle-right))
                                  ((string=? ((e:args) 0) "__tab__")
                                   (handle-tab))
                                  ((string=? ((e:args) 0) "__ctrl__")
                                   (handle-ctrl))
                                  (else
                                   (handle-key (e:args))))))
                     (run)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: the linkedlist needs at least a single empty line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fill (lambda (index stop)
               (cond ((not (= index stop))
                      (buffer:add "")
                      (fill (inc index) stop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init (lambda ()
               (load filePath)
               (when (zero? (buffer:size))
                     (lines:add ""))
               (clear-term)
               (draw-text)
               (draw-modeline)
               (draw-text)
               (term:setCursorPos 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run)
(clear-term)
