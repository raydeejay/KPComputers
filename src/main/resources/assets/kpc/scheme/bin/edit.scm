(load "utils.scm")
(load "buffer.scm")
(load "mocks.scm")

(require 'srfi-1)
(import (rnrs hashtables))

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
         (term:write (format #f " ~a - Press ~a for menu"
                             (buffer-name buf)
                             (if *kpc* "Ctrl" "Escape")))))
    (with-buf-dsl buf
                  (let ((lnStr (format #f "Ln ~d Col ~d" py px)))
                    (term:setCursorPos (- w (string-length lnStr)) (dec h))
                    (term:write lnStr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *keybindings* '())
(define add-key-binding (lambda (table key fn) (cons (pair key fn) table)))
;; (define make-bindings-table (lambda () (make-hashtable string-hash string=?)))

;; general handlers
(define toggle-menu (lambda (buf) (set! menu (not menu))))
(define quit-editor-hard (lambda (buf) (exit 0)))
(define dummy-kh (lambda (#!rest params) #t))


(define do-menu-function (lambda (item-n) ((cdr (*menu* item-n)))))
(define activate-menu-item (lambda (buf) (do-menu-function menu-item)))

(define move-menu-backward (lambda (buf) (set! menu-item (if (zero? menu-item)
                                                        (dec (length *menu*))
                                                        (dec menu-item)))))

(define move-menu-forward (lambda (buf) (set! menu-item (if (= menu-item (dec (length *menu*)))
                                                       0
                                                       (inc menu-item)))))

;; editor specific handlers
(define handle-key (lambda (buf key) (insert-char buf key)))
(define handle-tab (lambda (buf) (dotimes 2 (insert-char buf " "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define install-menu-bindings
  (lambda (buf)
    (set! *keybindings* (-> '()
                            (add-key-binding "__enter__" activate-menu-item)
                            (add-key-binding "__left__" move-menu-backward)
                            (add-key-binding "__right__" move-menu-forward)
                            (add-key-binding "__ctrl__" toggle-menu)
                            (add-key-binding "__escape__" toggle-menu)
                            (add-key-binding "__f12__" quit-editor-hard)))))


(define install-editor-bindings
  (lambda (buf)
    (set! *keybindings* (-> '()
                            (add-key-binding "__enter__" insert-newline)
                            (add-key-binding "__back__" remove-at-point-backward)
                            (add-key-binding "__up__" move-line-backward)
                            (add-key-binding "__down__" move-line-forward)
                            (add-key-binding "__left__" move-backward)
                            (add-key-binding "__right__" move-forward)
                            (add-key-binding "__home__" move-to-bol)
                            (add-key-binding "__end__" move-to-eol)
                            (add-key-binding "__delete__" remove-at-point)
                            (add-key-binding "__ctrl__" toggle-menu)
                            (add-key-binding "__escape__" toggle-menu)
                            (add-key-binding "__f12__" quit-editor-hard)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define base-run (lambda (buf e default-kh)
                   (let ((name (e:name))
                         (key ((e:args) 0)))
                     (and (string=? name "char")
                          (not (string=? key "__ignored__"))
                          (let ((binding (find (lambda (i)
                                                 (string=? (car i) key))
                                               *keybindings*)))
                            (if binding
                                ((cdr binding) buf)
                                (default-kh buf key)))))))

(define menu-run (lambda (buf e) (base-run buf e dummy-kh)))
(define editor-run (lambda (buf e) (base-run buf e handle-key)))

(define run (lambda (buf)
              (when running
                    (let ((e (os:pull)))
                      (if menu
                          (begin (install-menu-bindings buf) (menu-run buf e))
                          (begin (install-editor-bindings buf) (editor-run buf e))))
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
(install-editor-bindings *current-buffer*)
(run *current-buffer*)
(clear-term)
