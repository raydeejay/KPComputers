(load "utils.scm")
(load "buffer.scm")
(load "mocks.scm")

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
(define txtbuf (java.util.LinkedList))
;(define filePath (args 0))
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
;; (define eof? (lambda (line) (= -1 line)))

;; (define consume (lambda (file acc)
;;                   (let ((line (file:readLine)))
;;                     (if (eof? line)
;;                         acc
;;                         (begin (acc:add line)
;;                                (consume file acc))))))

;; (define load (lambda (path)
;;                (when (fs:exists path)
;;                      (consume (fs:read path) (java.util.LinkedList)))))

;; (define produce (lambda (file buf)
;;                   (do ((i 0 (inc i)))
;;                       ((< i (txtbuf:size)))
;;                     (file:write (string-append (txtbuf:get i) #\n)))))

;; (define save (lambda (path)
;;                (produce (fs:open path) txtbuf)))

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
(define draw-item (lambda (item selected)
                    (let ((item (if selected
                                    (string-append "[" item "]")
                                    (string-append " " item " "))))
                      (term:write item)
                      (term:setCursorX (+ (term:getCursorX) (string-length item))))))

(define draw-menu (lambda (mi)
                    (term:setCursorPos 1 (dec h))
                    (draw-item "Save" (zero? mi))
                    (draw-item "Exit" (= mi 1))))

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

(define do-menu-function (lambda (func)
                           (cond ((zero? func)
                                  ;; (save filePath)
                                  (set! menu #f))
                                 ((= func 1)
                                  (set! running #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter-menu (lambda ()
                            (do-menu-function menu-item)))

(define handle-left-menu (lambda ()
                           (set! menu-item (dec menu-item))
                           (cond ((< menu-item 0)
                                  (set! menu-item 1)))))

(define handle-right-menu (lambda ()
                            (set! menu-item (inc menu-item))
                            (cond ((> menu-item 1)
                                   (set! menu-item 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handle-enter (lambda (buf) (insert-newline buf)))
(define handle-back (lambda (buf) (remove-at-point-backward buf)))
(define handle-up (lambda (buf) (move-line-backward buf)))
(define handle-down (lambda (buf) (move-line-forward buf)))
(define handle-left (lambda (buf) (move-backward buf)))
(define handle-right (lambda (buf) (move-forward buf)))

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
                    (term:update buf)
                    (run buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init (lambda ()
               ;;               (set! txtbuf (load filePath))
               (clear-term)
               (screen:start-screen)
               (term:setCursorPos 1 1)
               (set! txtbuf (make-buffer "*scratch*"))
               (term:update txtbuf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(run txtbuf)
(clear-term)
