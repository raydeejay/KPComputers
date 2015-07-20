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
                      (unless (or (= index (- h 2))
                                  (= index (dec (buf:size))))
                              (term:setCursorPos 1 (inc y))
                              (term:clearLine)
                              (term:write (buf:get (+ y scrollY)))
                              (set! y (inc y))
                              (write-lines y buf))))

(define draw-text (lambda ()
                    (define oldY y)
                    (set! y 0)
                    (write-lines y buffer)
                    (term:setCursorPos (- x scrollX) (- y scrollY))
                    (set! y oldY)))

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
                              (define line (buffer:get (dec y)))
                              (buffer:set (dec y) (str:sstring line 0 (dec x)))
                              (draw-text)
                              (set! x 1)
                              (set! y (inc y))
                              (setCursorPos x y))
                             (else
                              (do-menu-function menu-item)))
                       (draw-modeline)))

(define handle-back (lambda ()
                      (cond ((> x 1)
                             (define line (buffer:get (dec y)))
                             (define newLine (str:combine (str:sstring line 0 (- x 2))
                                                          (str:sstring line x)))
                             (buffer:set (dec y) newLine)
                             (draw-line (dec y))
                             (set! x (dec x))
                             (setCursorPos x y))
                            ((> y 1)
                             (define prevLine (string-length (buffer:get (- y 2))))
                             (define newLine (str:combine (buffer:get (- y 2))
                                                          (buffer:get (dec y))))
                             (buffer:set (dec y) newLine)
                             (buffer:remove (dec y))
                             (draw-text)
                             (set! x (inc prevLine))
                             (set! y (dec y))
                             (setCursorPos x y)))))

(define handle-up (lambda ()
                    (cond ((> y 1)
                           (set! y (dec y))
                           (setCursorPos x y)))))

(define handle-down (lambda ()
                      (cond ((< y (- (buffer:size) 1))
                             (set! y (inc y))
                             (setCursorPos x y)))))

(define handle-left (lambda ()
                      (cond ((not menu)
                             (cond ((> x 1)
                                    (set! x (dec x)))
                                   ((and (= x 1) (> y 1))
                                    (set! x (string-length (buffer:get (dec y))))
                                    (set! y (dec y)))))
                            (else
                             (set! menu-item (dec menu-item))
                             (cond ((< menu-item 0)
                                    (set! menu-item 1)))
                             (draw-modeline)))
                      (setCursorPos x y)))

(define handle-right (lambda ()
                       (cond ((not menu)
                              (cond ((< x (inc (string-length (buffer:get y))))
                                     (set! x (inc x)))
                                    ((and (= x (inc (string-length (buffer:get y))))
                                          (< y (buffer:size)))
                                     (set! x 1)
                                     (set! y (inc y)))))
                             (else
                              (set! menu-item (inc menu-item))
                              (cond ((> menu-item 1)
                                     (set! menu-item 1)))
                              (draw-modeline)))
                       (setCursorPos x y)))

(define handle-tab (lambda ()
                     (define s (buffer:get (dec y)))
                     (buffer:set (dec y) (str:combine "  " s))
                     (set! x (+ x 2))
                     (setCursorPos x y)
                     (draw-line (dec y))))

(define handle-ctrl (lambda ()
                      (set! menu (not menu))
                      (draw-modeline)))

(define handle-key (lambda ()
                     (define s (buffer:get (dec y)))
                     (define newLine (str:combine (str:sstring s 0 (dec x))
                                                  ((e:args) 0)
                                                  (str:sstring s x)))
                     (buffer:set (dec y) newLine)
                     (draw-line (dec y))
                     (set! x (inc x))
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
               (term:setCursorPos x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run)
(clear-term)
