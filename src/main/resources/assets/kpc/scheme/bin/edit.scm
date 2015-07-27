(load "utils.scm")
(load "buffer.scm")
(load "mocks.scm")

(require 'srfi-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w (term:getWidth))
(define h (term:getHeight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scrollX 0)
(define scrollY 0)
(define *buffers* '())
(define *current-buffer* #!null)
(define menu #f)
(define menu-item 0)

(define running #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define clear-term (lambda ()
                     (term:clear)
                     (term:setCursorPos 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading and saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eof? (lambda (line) (or (= -1 line)
                           (eq? #!null line))))

(define consume (lambda (file lines)
                  (let ((line (file:read-line)))
                    (if (eof? line)
                        lines
                        (begin (lines:add line)
                               (consume file lines))))))

(define load-file (lambda (path)
                    (let ((buf (make-buffer path)))
                      (when (fs:exists path)
                            (let* ((file (fs:read path))
                                   (lines (consume file (java.util.LinkedList))))
                              (file:close)
                              (set-buffer-lines! buf lines)))
                      buf)))

(define produce (lambda (file buf)
                  (do ((i 0 (inc i)))
                      ((= i (buf:size)))
                    (file:write (buf:get i))
                    (if (< i (buf:size))
                        (file:write "\n")))))

(define save-file (lambda (path buf)
               (let ((file (fs:open path))
                     (lines (buffer-lines buf)))
                 (produce file lines)
                 (file:close))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing to the terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define draw-text (lambda (buf x y scrollX scrollY)
                    (do ((i 0 (inc i)))
                        ((> i (- h 2)))
                      (let ((bufline (+ i scrollY))
                            (lines (buffer-lines buf)))
                        (when (< bufline (lines:size))
                              (term:setCursorPos (- 1 scrollX) (+ 1 i))
                              (term:write (lines:get bufline)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *menu* (list (cons "Save" (lambda ()
                                    (save-file (buffer-name *current-buffer*) *current-buffer*)
                                    (set! menu #f)))
                     (cons "Load" (lambda ()
                                    (set! *current-buffer* (load-file "another.txt"))
                                    (set! *buffers* (cons *current-buffer* *buffers*))
                                    (set! menu #f)))
                     (cons "Next" (lambda ()
                                    (let ((next-list (cdr
                                                      (find-tail (lambda (e) (eq? e *current-buffer*))
                                                                 *buffers*))))
                                      (set! *current-buffer* (if (eq? next-list '())
                                                                 (car *buffers*)
                                                                 (car next-list))))))
                     (cons "Exit" (lambda () (set! running #f)))))


(define draw-item (lambda (item selected)
                    (let ((item (if selected
                                    (string-append "[" item "]")
                                    (string-append " " item " "))))
                      (term:write item)
                      (term:setCursorX (+ (term:getCursorX) (string-length item))))))

(define draw-menu (lambda (item-n)
                    (term:setCursorPos 1 (dec h))
                    (do ((i 0 (inc i)))
                        ((= i (length *menu*)))
                      (draw-item (car (*menu* i)) (= item-n i)))))

(define draw-modeline
  (lambda (buf)
    (with-preserved-cursor
     (term:setCursorPos 1 (dec h))
     (term:clearLine)
     (if menu
         (draw-menu menu-item)
         (term:write (format #f " ~a - Press Ctrl for menu" (buffer-name buf)))))
     (with-buf-dsl buf
                   (let ((lnStr (format #f "Ln ~d Col ~d" py px)))
                     (term:setCursorPos (- w (string-length lnStr)) (dec h))
                     (term:write lnStr)))))

(define do-menu-function (lambda (item-n) ((cdr (*menu* item-n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter-menu (lambda ()
                            (do-menu-function menu-item)))

(define handle-left-menu (lambda ()
                           (set! menu-item (dec menu-item))
                           (cond ((< menu-item 0)
                                  (set! menu-item (dec (length *menu*)))))))

(define handle-right-menu (lambda ()
                            (set! menu-item (inc menu-item))
                            (cond ((= menu-item (length *menu*))
                                   (set! menu-item 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter (lambda (buf) (insert-newline buf)))
(define handle-back (lambda (buf) (remove-at-point-backward buf)))
(define handle-up (lambda (buf) (move-line-backward buf)))
(define handle-down (lambda (buf) (move-line-forward buf)))
(define handle-left (lambda (buf) (move-backward buf)))
(define handle-right (lambda (buf) (move-forward buf)))
(define handle-home (lambda (buf) (move-to-bol buf)))
(define handle-end (lambda (buf) (move-to-eol buf)))

;; (define handle-tab (lambda (x y)
;;                      (define s (txtbuf:get (cursor->idx y)))
;;                      (txtbuf:set (cursor->idx y) (string-append "  " s))
;;                      (setCursorPos (+ 2 x) y)
;;                      (draw-line (cursor->idx y) scrollX scrollY)))

(define handle-key (lambda (buf key) (insert-char buf key)))

(define handle-ctrl (lambda () (set! menu (not menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define menu-run (lambda (buf e)
                   (let ((name (e:name))
                         (key ((e:args) 0)))
                     (cond ((string=? name "char")
                            (cond ((string=? key "__enter__")
                                   (handle-enter-menu))
                                  ((string=? key "__left__")
                                   (handle-left-menu))
                                  ((string=? key "__right__")
                                   (handle-right-menu))
                                  ((or (string=? key "__ctrl__")
                                       (string=? key "__escape__"))
                                   (handle-ctrl))))))))

(define editor-run (lambda (buf e)
                     (let ((name (e:name))
                           (key ((e:args) 0)))
                       (cond ((string=? name "char")
                              (cond ((string=? key "__enter__")
                                     (handle-enter buf))
                                    ((string=? key "__back__")
                                     (handle-back buf))
                                    ((string=? key "__up__")
                                     (handle-up buf))
                                    ((string=? key "__down__")
                                     (handle-down buf))
                                    ((string=? key "__left__")
                                     (handle-left buf))
                                    ((string=? key "__right__")
                                     (handle-right buf))
                                    ((string=? key "__home__")
                                     (handle-home buf))
                                    ((string=? key "__end__")
                                     (handle-end buf))
                                    ;; ((string=? key "__tab__")
                                    ;;  (handle-tab buf))
                                    ((or (string=? key "__ctrl__")
                                         (string=? key "__escape__"))
                                     (handle-ctrl))
                                    ((string=? key "__f12__")
                                     (exit 0)) ;; yeah \o/
                                    (else
                                     (handle-key buf key))))))))

(define run (lambda (buf)
              (when running
                    (let ((e (os:pull)))
                      (if menu
                          (menu-run buf e)
                          (editor-run buf e)))
                    (term:update *current-buffer*)
                    (run *current-buffer*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init (lambda ()
               (clear-term)
               (screen:start-screen)
               (term:setCursorPos 1 1)
               (if (zero? (length (args)))
                   (set! *buffers* (list (make-buffer "*scratch*")))
                   (for-each (lambda (f)
                               (set! *buffers* (cons (load-file f) *buffers*)))
                             (args)))
               (set! *buffers* (reverse *buffers*))
               (set! *current-buffer* (car *buffers*))
               (term:update *current-buffer*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run *current-buffer*)
(clear-term)
