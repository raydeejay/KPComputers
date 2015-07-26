;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the term object
;;
;; allows me to treat lanterna terminals as laceh terminals,
;; which have 1-based coordinates
;;
;; eventually I may ask her to change it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "utils.scm")

;; terminal
(define tf::com.googlecode.lanterna.terminal.DefaultTerminalFactory
  (com.googlecode.lanterna.terminal.DefaultTerminalFactory))
(define terminal::com.googlecode.lanterna.terminal.Terminal
  (tf:create-terminal))
;; screen
(define screen::com.googlecode.lanterna.screen.TerminalScreen
  (com.googlecode.lanterna.screen.TerminalScreen terminal))

;; TODO: these can probably be aliased instead...
(defmacro char* (c) `(com.googlecode.lanterna.TextCharacter ,c))
(defmacro xy* (x y) `(com.googlecode.lanterna.TerminalPosition ,x ,y))

;; state
(define private-x 1)
(define private-y 1)

;; functions
(define term:getWidth (lambda () ((terminal:getTerminalSize):getColumns)))
(define term:getHeight (lambda () ((terminal:getTerminalSize):getRows)))

(define term:getCursorX (lambda () private-x))
(define term:getCursorY (lambda () private-y))

(define term:setCursorPos (lambda (xx yy)
                            (set! private-x xx)
                            (set! private-y yy)
                            (screen:set-cursor-position (xy* (- xx 1) (- yy 1)))))
(define term:setCursorX (lambda (xx) (term:setCursorPos xx (term:getCursorY))))
(define term:setCursorY (lambda (yy) (term:setCursorPos (term:getCursorX) yy)))

;; this crappy macro doesn't preserve the potential return value,
;; but, oh, well... it will do for now
(defmacro with-preserved-cursor (#!rest body)
  (let ((old-x-symb (gentemp))
        (old-y-symb (gentemp)))
    `(let ((,old-x-symb (term:getCursorX))
           (,old-y-symb (term:getCursorY)))
       ,@body
       (term:setCursorX ,old-x-symb)
       (term:setCursorY ,old-y-symb))))

(define term:refresh (lambda ()
                       (term:setCursorPos (term:getCursorX)
                                          (term:getCursorY))
                       (screen:refresh)))
(define term:write-char (lambda (c)
                          (screen:set-character (dec (term:getCursorX))
                                                (dec (term:getCursorY))
                                                (char* c))))
(define term:advance (lambda () (term:setCursorX (+ 1 (term:getCursorX)))))
(define term:write (lambda (str)
                     (for-each (lambda (c)
                                 (term:write-char c)
                                 (term:advance))
                               (string->list str))))

(define term:update
  (lambda (buf)
    (with-buf-dsl buf
                  (let ((x (inc px))
                        (y (inc py)))
                    (cond ((> (- x scrollX) w)
                           (set! scrollX (- px (dec w))))
                          ((<= x scrollX)
                           (set! scrollX (- w (- w x) 1))))
                    (cond ((> (- y scrollY) (- h 2))
                          (set! scrollY (- y (- h 2))))
                          ((<= y scrollY)
                           (set! scrollY (dec scrollY))))
                    (let ((ox (- x scrollX))
                          (oy (- y scrollY)))
                      (screen:clear)
                      (draw-text buf ox oy scrollX scrollY)
                      (draw-modeline buf)
                      (term:setCursorPos ox oy)
                      (term:refresh))))))

(define term:clear (lambda ()
                     (screen:clear)
                     (term:setCursorPos 1 1)
                     (term:refresh)))
(define term:clearLine (lambda ()
                         (with-preserved-cursor
                          (term:setCursorX 1)
                          (do ((i 1 (inc i)))
                              ((> i (term:getWidth)) 'exit)  ;; just to remember this
                            (term:write-char #\space)
                            (term:advance)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the fs object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define private-file-port ())
(define fs:exists (lambda (f) (file-exists? f)))
;; fs:read
;; fs:open

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the file object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file:readLine
;; file:write

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the os object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define e::com.googlecode.lanterna.input.KeyStroke #!null) ;; eekkkk......
(define e #!null) ;; eekkkk......
(define e-type::com.googlecode.lanterna.input.KeyType #!null) ;; eekkkk......
(define os:pull (lambda ()
                  (let* ((i (screen:read-input))
                         (key-type (i:get-key-type)))
                    (set! e-type key-type)
                    (cond ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "Escape"))
                           (set! e "__escape__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "Enter"))
                           (set! e "__enter__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "ArrowUp"))
                           (set! e "__up__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "ArrowDown"))
                           (set! e "__down__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "Backspace"))
                           (set! e "__back__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "Tab"))
                           (set! e "__tab__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "ArrowLeft"))
                           (set! e "__left__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "ArrowRight"))
                           (set! e "__right__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "F12"))
                           (set! e "__f12__"))
                          ((eqv? key-type (com.googlecode.lanterna.input.KeyType:valueOf "Character"))
                           (set! e ((i:get-character):char-value)))
                          (else (set! e " "))))
                  e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the e object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define e:name (lambda () "char"))
(define e:args (lambda () (list (if (string? e) e (string e))))) ;; incomplete but good enough

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the args "object"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (args) 0
