;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w (term:getWidth))
(define h (term:getHeight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define x 1)
;;(define y 1)
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
(define get-x (lambda () (term:getCursorX)))
(define get-y (lambda () (term:getCursorY)))

(define set-x (lambda (x))
  (term:setCursorX x))

(define set-y (lambda (y))
  (term:setCursorY y))

(define set-cursor (lambda (x y))
  (set-x x)
  (set-y y))

(define idx->cursor (lambda (n) (inc n)))
(define cursor->idx (lambda (n) (dec n)))

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
                      (unless (or (= index (dec (cursor->idx h)))
                                  (= index (dec (buf:size))))
                              (term:setCursorPos 1 (idx->cursor index))
                              (term:clearLine)
                              (term:write (buf:get (+ index scrollY)))
                              (write-lines (inc index) buf))))

(define draw-text (lambda (x y)
                    (write-lines (cursor->idx y) buffer)
                    (set-cursor (- x scrollX) (- y scrollY))))

(define draw-line (lambda (n)
                    (term:setCursorPos (- 1 scrollX) (- (inc n) scrollY))
                    (term:clearLine)
                    (term:write (buffer:get n))
                    (term:setCursorPos (- (get-x) scrollX) (- (inc n) scrollY))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define draw-item (lambda (item selected)
                    (let ((item (if selected
                                    (string-append "[" item "]")
                                    item)))
                      (term:write item)
                      (set-x (+ (get-x)
                                (string-length item))))))

(define draw-menu (lambda (mi)
                    (draw-item "Save" (zero? mi))
                    (draw-item "Exit" (= mi 1))))

(define draw-modeline (lambda ()
                        (let ((lnStr (string-append "Ln "
                                                    (java.lang.String:valueOf y))))
                          (set-cursor 1 (dec h))
                          (term:clearLine)
                          (set-cursor (- w (string-length lnStr)) (dec h))
                          (term:write lnStr))
                        (set-cursor 1 (dec h))
                        (if menu
                            (draw-menu menu-item)
                            (draw-item "Press Ctrl for menu"))
                        (set-cursor (- (get-x) scrollX) (- (get-y) scrollY))))

(define do-menu-function (lambda (func)
                           (cond ((zero? func)
                                  (save filePath))
                                 ((= func 1)
                                  (set! running #f)))))

(define handle-enter-menu (lambda ()
                            (do-menu-function menu-item)
                            (draw-modeline)))

(define handle-left-menu (lambda ()
                           (set! menu-item (dec menu-item))
                           (cond ((< menu-item 0)
                                  (set! menu-item 1)))
                           (draw-modeline)
                           (setCursorPos (get-x) (get-y))))

(define handle-right-menu (lambda ()
                            (set! menu-item (inc menu-item))
                            (cond ((> menu-item 1)
                                   (set! menu-item 1)))
                            (draw-modeline)
                            (setCursorPos (get-x) (get-y))))

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
                              (draw-text (get-x) (get-y))))
                       (set-cursor screenX screenY)
                       (draw-modeline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter (lambda ()
                       (let* ((bufx (cursor->idx (get-x)))
                              (bufy (cursor->idx (get-y)))
                              (line (buffer:get bufy)))
                         (buffer:set bufy (str:sstring line 0 bufx))
                         (buffer:add (inc bufy)
                                     (str:sstring line (inc bufx)
                                                  (string-length line))))
                       (draw-text (get-x) (get-y))
                       (setCursorPos 1 (inc (get-y)))
                       (draw-modeline)))

(define handle-back (lambda ()
                      (unless (= (get-x) (get-y) 1)
                              (cond ((> (get-) 1)
                                     (define line (buffer:get (cursor->idx (get-y))))
                                     (define newLine (str:combine
                                                      (str:sstring line 0 (- (get-x) 2))
                                                      (str:sstring line (get-x))))
                                     (buffer:set (cursor->idx (get-y)) newLine)
                                     (draw-line (cursor->idx (get-y)))
                                     (setCursorPos (dec (get-x)) (get-y)))
                                    ((> (get-y) 1)
                                     (define prevLine (string-length
                                                       (buffer:get (- (get-y) 2))))
                                     (define newLine (str:combine
                                                      (buffer:get (- (get-y) 2))
                                                      (buffer:get (dec (get-y)))))
                                     (buffer:set (dec (get-y)) newLine)
                                     (buffer:remove (dec (get-y)))
                                     (draw-text (get-x) (get-y))
                                     (setCursorPos (inc prevLine) (dec (get-y))))))))

(define handle-up (lambda ()
                    (when (> (get-y) 1)
                          (setCursorPos (get-x)
                                        (dec (get-y))))))

(define handle-down (lambda ()
                      (when (< (get-y) (dec (buffer:size)))
                            (setCursorPos (get-x) (inc (get-y))))))

(define handle-left (lambda ()
                      (cond ((> (get-x) 1)
                             (setCursorPos (dec (get-x))
                                           (get-y)))
                            ((> (get-y) 1)
                             (setCursorPos (string-length
                                            (buffer:get (dec (cursor->idx (get-y)))))
                                           (dec (get-y)))))))

(define handle-right (lambda ()
                       (cond ((< (get-x) (inc (string-length
                                               (buffer:get (cursor->idx (get-y))))))
                              (setCursorPos (inc (get-x)) (get-y)))
                             ((< (get-y) (buffer:size))
                              (setCursorPos 1 (inc (get-y)))))))

(define handle-tab (lambda ()
                     (define s (buffer:get (cursor->idx (get-y))))
                     (buffer:set (cursor->idx (get-y)) (str:combine "  " s))
                     (setCursorPos (+ 2 (get-x)) (get-y))
                     (draw-line (cursor->idx (get-y)))))

(define handle-ctrl (lambda ()
                      (set! menu (not menu))
                      (draw-modeline)))

(define handle-key (lambda ()
                     (define s (buffer:get (cursor->idx (get-y))))
                     (define newLine (str:combine (str:sstring s 0 (cursor->idx (get-x)))
                                                  ((e:args) 0)
                                                  (str:sstring s (get-x))))
                     (buffer:set (cursor->idx (get-y)) newLine)
                     (draw-line (cursor->idx (get-y)))
                     (setCursorPos (inc (get-x)) (get-y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define run (lambda ()
              (cond (running
                     (define e (os:pull))
                     (cond ((string=? (e:name) "char")
                            (cond ((string=? ((e:args) 0) "__enter__")
                                   (if menu
                                       (handle-enter-menu)
                                       (handle-enter)))
                                  ((string=? ((e:args) 0) "__back__")
                                   (handle-back))
                                  ((string=? ((e:args) 0) "__up__")
                                   (handle-up))
                                  ((string=? ((e:args) 0) "__down__")
                                   (handle-down))
                                  ((string=? ((e:args) 0) "__left__")
                                   (if menu
                                       (handle-left-menu)
                                       (handle-left)))
                                  ((string=? ((e:args) 0) "__right__")
                                   (if menu
                                       (handle-right-menu)
                                       (handle-right)))
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
               (draw-text (get-x) (get-y))
               (draw-modeline)
               (draw-text (get-x) (get-y))
               (term:setCursorPos 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run)
(clear-term)
