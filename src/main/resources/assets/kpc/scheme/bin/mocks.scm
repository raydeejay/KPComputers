;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the term object
;;
;; allows me to treat lanterna terminals as laceh terminals,
;; which have 1-based coordinates
;;
;; eventually I may ask her to change it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load "utils.scm")

;; terminal
(define tf::com.googlecode.lanterna.terminal.DefaultTerminalFactory
  (com.googlecode.lanterna.terminal.DefaultTerminalFactory))
(define terminal::com.googlecode.lanterna.terminal.Terminal
  (tf:create-terminal))
;; screen
(define screen::com.googlecode.lanterna.screen.TerminalScreen
  (com.googlecode.lanterna.screen.TerminalScreen terminal))

;; aliases
(define-alias char* com.googlecode.lanterna.TextCharacter)
(define-alias xy* com.googlecode.lanterna.TerminalPosition)

(define-alias SGR_ com.googlecode.lanterna.SGR)
(defmacro SGR (str) `(SGR_:valueOf ,str))

(define-alias color_ com.googlecode.lanterna.TextColor:ANSI)
(defmacro color (str) `(color_:valueOf ,str))

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

;; TODO: global state, eek.. better create a class
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

(define DELTA (com.googlecode.lanterna.screen.Screen:RefreshType:valueOf "DELTA"))
(define term:refresh (lambda ()
                       (term:setCursorPos (term:getCursorX)
                                          (term:getCursorY))
                       (screen:refresh DELTA)))

(define term:advance (lambda () (term:setCursorX (+ 1 (term:getCursorX)))))

(define apply-sgr-to-char (lambda (c #!optional attrs)
                            (if (or (not attrs) (eq? attrs '()))
                                c
                                (apply-sgr-to-char (c:withModifier (car attrs))
                                                   (cdr attrs)))))

(define term:write-char (lambda (c #!optional attrs)
                          (screen:set-character (dec (term:getCursorX))
                                                (dec (term:getCursorY))
                                                (apply-sgr-to-char c attrs))))

(define term:write (lambda (str #!rest attrs)
                     (for-each (lambda (c)
                                 (term:write-char (char* c) attrs)
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

(define term:clearLine (lambda (#!rest attrs)
                         (with-preserved-cursor
                          (term:setCursorX 1)
                          (do ((i 1 (inc i)))
                              ((> i (term:getWidth)) 'exit)  ;; just to remember this
                            (term:write-char (char* #\space) attrs)
                            (term:advance)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the fs object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fs:exists (lambda (f) (file-exists? f)))
(define fs:open (lambda (path) (open-output-file path))) ;; open a file for WRITING
(define fs:read (lambda (path)
                  (java.io.BufferedReader
                   (java.io.FileReader path)))) ;; OPEN A FILE for READING

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the file object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro file:write (line) `(write-string ,line file))
(defmacro file:close () `(close-port file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the os object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define e #!null) ;; eekkkk......
(define e-type::com.googlecode.lanterna.input.KeyType #!null) ;; eekkkk......

(define-alias KeyType com.googlecode.lanterna.input.KeyType)

(define translation-table (map (lambda (p) (cons (KeyType:valueOf (car p)) (cadr p)))
                               '(("Escape" "__escape__")
                                 ("Enter" "__enter__")
                                 ("ArrowUp" "__up__")
                                 ("ArrowDown" "__down__")
                                 ("Backspace" "__back__")
                                 ("Tab" "__tab__")
                                 ("ArrowLeft" "__left__")
                                 ("ArrowRight" "__right__")
                                 ("Home" "__home__")
                                 ("End" "__end__")
                                 ("Insert" "__insert__")
                                 ("Delete" "__delete__")
                                 ("F12" "__f12__"))))

(define os:pull (lambda ()
                  (let* ((i (screen:read-input))
                         (key-type (i:get-key-type)))
                    (set! e-type key-type)
                    (let ((key (assoc key-type translation-table)))
                      (cond (key (set! e (cdr key)))
                            ((eqv? key-type (KeyType:valueOf "Character"))
                             (set! e ((i:get-character):char-value)))
                            (else (set! e "__ignored__"))))) ;; debug/dev code
                  e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the e object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define e:name (lambda () "char"))
(define e:args (lambda () (list (if (string? e) e (string e))))) ;; incomplete but good enough

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mock/wrapper for the args "object"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define args (lambda () (vector->list command-line-arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to determmine if we run on KPC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *kpc* #f)
