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
(define write-lines (lambda (index buf scrollX scrollY)
                      (unless (or (= index (dec (cursor->idx h)))
                                  (= index (dec (buf:size))))
                              (term:setCursorPos 1 (idx->cursor index))
                              (term:clearLine)
                              (term:write (buf:get (+ index scrollY)))
                              (write-lines (inc index) buf))))

(define draw-text (lambda (x y scrollX scrollY)
                    (write-lines (cursor->idx y) buffer scrollX scrollY)
                    (set-cursor (- x scrollX) (- y scrollY))))

(define draw-line (lambda (n scrollX scrollY)
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
                              (draw-text (get-x) (get-y) scrollX scrollY)))
                       (set-cursor screenX screenY)
                       (draw-modeline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter-menu (lambda (x y)
                            (do-menu-function menu-item)
                            (draw-modeline)))

(define handle-left-menu (lambda (x y)
                           (set! menu-item (dec menu-item))
                           (cond ((< menu-item 0)
                                  (set! menu-item 1)))
                           (draw-modeline)
                           (setCursorPos x y)))

(define handle-right-menu (lambda (x y)
                            (set! menu-item (inc menu-item))
                            (cond ((> menu-item 1)
                                   (set! menu-item 1)))
                            (draw-modeline)
                            (setCursorPos x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter (lambda (x y)
                       (let* ((bufx (cursor->idx x))
                              (bufy (cursor->idx y))
                              (line (buffer:get bufy)))
                         (buffer:set bufy (str:sstring line 0 bufx))
                         (buffer:add (inc bufy)
                                     (str:sstring line (inc bufx)
                                                  (string-length line))))
                       (draw-text x y scrollX scrollY)
                       (setCursorPos 1 (inc y))
                       (draw-modeline)))

(define handle-back (lambda (x y)
                      (unless (= x y 1)
                              (cond ((> y 1)
                                     (define line (buffer:get (cursor->idx y)))
                                     (define newLine (str:combine
                                                      (str:sstring line 0 (- x 2))
                                                      (str:sstring line x)))
                                     (buffer:set (cursor->idx y) newLine)
                                     (draw-line (cursor->idx y) scrollX scrollY)
                                     (setCursorPos (dec x) y))
                                    ((> y 1)
                                     (define prevLine (string-length
                                                       (buffer:get (- y 2))))
                                     (define newLine (str:combine
                                                      (buffer:get (- y 2))
                                                      (buffer:get (dec y))))
                                     (buffer:set (dec y) newLine)
                                     (buffer:remove (dec y))
                                     (draw-text x y scrollX scrollY)
                                     (setCursorPos (inc prevLine) (dec y)))))))

(define handle-up (lambda (x y)
                    (when (> y 1)
                          (setCursorPos x (dec y)))))

(define handle-down (lambda (x y)
                      (when (< y (dec (buffer:size)))
                            (setCursorPos x (inc y)))))

(define handle-left (lambda (x y)
                      (cond ((> x 1)
                             (setCursorPos (dec x) y))
                            ((> y 1)
                             (setCursorPos (string-length
                                            (buffer:get (dec (cursor->idx y))))
                                           (dec y))))))

(define handle-right (lambda (x y)
                       (cond ((< x (inc (string-length
                                         (buffer:get (cursor->idx y)))))
                              (setCursorPos (inc x) y))
                             ((< y (buffer:size))
                              (setCursorPos 1 (inc y))))))

(define handle-tab (lambda (x y)
                     (define s (buffer:get (cursor->idx y)))
                     (buffer:set (cursor->idx y) (str:combine "  " s))
                     (setCursorPos (+ 2 x) y)
                     (draw-line (cursor->idx y) scrollX scrollY)))

(define handle-ctrl (lambda (x y)
                      (set! menu (not menu))
                      (draw-modeline)))

(define handle-key (lambda (x y key)
                     (define s (buffer:get (cursor->idx y)))
                     (define newLine (str:combine (str:sstring s 0 (cursor->idx x))
                                                  key
                                                  (str:sstring s x)))
                     (buffer:set (cursor->idx y) newLine)
                     (draw-line (cursor->idx y) scrollX scrollY)
                     (setCursorPos (inc x) y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define run (lambda ()
              (when running
                    (let ((x (get-x))
                          (y (get-y))
                          (e (os:pull)))
                      (cond ((string=? (e:name) "char")
                             (cond ((string=? ((e:args) 0) "__enter__")
                                    (if menu
                                        (handle-enter-menu x y)
                                        (handle-enter x y)))
                                   ((string=? ((e:args) 0) "__back__")
                                    (handle-back x y))
                                   ((string=? ((e:args) 0) "__up__")
                                    (handle-up x y))
                                   ((string=? ((e:args) 0) "__down__")
                                    (handle-down x y))
                                   ((string=? ((e:args) 0) "__left__")
                                    (if menu
                                        (handle-left-menu x y)
                                        (handle-left x y)))
                                   ((string=? ((e:args) 0) "__right__")
                                    (if menu
                                        (handle-right-menu x y)
                                        (handle-right x y)))
                                   ((string=? ((e:args) 0) "__tab__")
                                    (handle-tab x y))
                                   ((string=? ((e:args) 0) "__ctrl__")
                                    (handle-ctrl x y))
                                   (else
                                    (handle-key x y ((e:args) 0)))))))
                    (run))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: the linkedlist needs at least a single empty line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define fill (lambda (index stop)
;;                (cond ((not (= index stop))
;;                       (buffer:add "")
;;                       (fill (inc index) stop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init (lambda ()
               (load filePath)
               (when (zero? (buffer:size))
                     (buffer:add ""))
               (clear-term)
               (draw-text (get-x) (get-y) scrollX scrollY)
               (draw-modeline)
               (draw-text (get-x) (get-y) scrollX scrollY)
               (term:setCursorPos 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run)
(clear-term)
