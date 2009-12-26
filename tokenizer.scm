;;  This file is part of the arc package
;;  Copyright (C) 2002, 2003, 2009 by Gregor Klinke
;;
;;  This library is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; TODO: read numbers (reals, engnotated ints)
;;;       track file location


(define current-char #f)
(define line-count 1)

(define (next-char port)
  (set! current-char (read-char port))
  (if (or (eq? current-char #\newline)
          (eq? current-char #\return))
      (set! line-count (+ line-count 1)))
  current-char)

(define (read-comment-line port)
  (let loop ((c current-char))
    (if (or (eof-object? c))
        #f
        (if (equal? c #\newline)
            (next-char port)
            (loop (next-char port))))))

(define (is-whitespace c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)))

(define (is-digit c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (is-hex-digit c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (is-alpha c)
  (or (and (char>=? c #\a)
           (char<=? c #\z))
      (and (char>=? c #\A)
           (char<=? c #\Z))))

(define (is-alpha-spec c)
  (or (char=? c #\-)
      (char=? c #\_)
      (char=? c #\*)
      (char=? c #\+)
      (char=? c #\%)
      (char=? c #\>)
      (char=? c #\<)
      (char=? c #\?)
      (char=? c #\/)))

(define (is-symbol-char c)
  (or (is-alpha c)
      (is-digit c)
      (is-alpha-spec c)))

(define (is-delimiter c)
  (or (is-whitespace c)
      (char=? c #\")
      (char=? c #\() (char=? c #\))
      (char=? c #\[) (char=? c #\])
      (char=? c #\{) (char=? c #\})
      (char=? c #\.)
      (char=? c #\,)
      (char=? c #\;)
      (char=? c #\@)))


(define (read-identifier prefix port type)
  (let loop ((res '()))
    (if (or (eof-object? current-char)
            (is-delimiter current-char))
        (cons type (string-append prefix (list->string (reverse res))))

        (let ((c current-char))
          (next-char port)
          (loop (cons c res))) )))


(define (translate-number str)
  (let* ((l (string-length str))
         (lastc (string-ref str (- l 1))))
    (cond
     ( (or (char=? lastc #\h)
           (char=? lastc #\H)) (string->number (substring str 0 (- l 1)) 16) )
     ( (or (char=? lastc #\t)
           (char=? lastc #\T)) (string->number (substring str 0 (- l 1)) 8) )
     ( (or (char=? lastc #\y)
           (char=? lastc #\Y)) (string->number (substring str 0 (- l 1)) 2) )
     ( else (string->number str 10)))))

(define (read-number port neg?)
  (let loop ((res '()))
    (if (or (eof-object? current-char)
            (is-delimiter current-char))
        (let* ((numstr (list->string (reverse res)))
               (num (translate-number numstr)))
          (if (not num)
              (cons 'ERROR (string-append "not a number: " numstr))
              (cons 'INT (if neg? (* num -1) num))))
        (let ((c current-char))
          (next-char port)
          (loop (cons c res))))))


(define (read-string separator port)
  (next-char port)                      ; ignore the leading "
  (let loop ((res '()))
    (if (eof-object? current-char)
        (cons 'ERROR "unfinished string")
        (if (char=? current-char separator)
            (begin
              (next-char port)          ; ignore trailing "
              (cons 'STR (string-append (list->string (reverse res)))))

            (let ((c current-char))
              (if (char=? c #\\)
                  (let ((token (read-character port #t)))
                    (if (and (pair? token)
                             (eq? (car token) 'CHAR))
                        (loop (cons (cdr token) res))
                        token))
                  (begin
                    (next-char port)
                    (loop (cons c res)))) )))))


(define (map-charnm->char name)
  (cond
   ((string=? name "space") #\space)
   ((or (string=? name "nl")
        (string=? name "newline")) #\newline)
   ((or (string=? name "cr")
        (string=? name "return")) #\return)
   ((string=? name "tab") #\tab)
   (else 'unknown)))


(define (translate-char charnm)
  (if (= (string-length charnm) 1)
      (cons 'CHAR (string-ref charnm 0))
      (let* ((c (map-charnm->char charnm)))
        (if (eq? c 'unknown)
            (cons 'ERROR (string-append "unknown char named " charnm))
            (cons 'CHAR c))) ))


(define (read-symbol-character port need-terminator?)
  (let* ((id (read-identifier "" port 'TMP))
         (res (if (and (pair? id)
                       (eq? (car id) 'TMP))
                  (translate-char (cdr id))
                  id) ) )
    (if (and (pair? id)
             (eq? (car id) 'TMP)
             (= (string-length (cdr id)) 1))
        res
        (if (and need-terminator? )
            (if (char=? current-char #\;)
                (begin
                  (next-char port)
                  res)
                (cons 'ERROR "unterminated char"))
            res))))


(define (read-named-character port needs-terminator?)
  (cond
    ( (or (is-whitespace current-char)
          (eof-object? current-char)) (cons 'ERROR "unterminated char notation"))
    ( (or (is-alpha current-char)
          (is-digit current-char)) (read-symbol-character port
                                                          needs-terminator?) )
    ( else (let ((c current-char))
             (next-char port)
             (cons 'CHAR c)) )))


(define (read-numeric-character port needs-terminator?)
  (let* ((token (read-number port #f))
         (c (if (and (pair? token)
                     (eq? (car token) 'INT))
                (integer->char (cdr token))
                #f)))

    (if (char? c)
        (if needs-terminator?
            (if (char=? current-char #\;)
                (begin
                  (next-char port)
                  (cons 'CHAR c))
                (cons 'ERROR "unterminated char"))
            (cons 'CHAR c))
        token)))


(define (read-character port needs-terminator?)
  (next-char port)
  (if (char=? current-char #\u)
      (begin
        (next-char port)
        (read-numeric-character port needs-terminator?))
      (begin
        (read-named-character port needs-terminator?))))


(define (read-hash port)
  (let* ((token (read-identifier "" port 'TMP)))
    (if (and (pair? token)
             (eq? (car token) 'TMP))
        (cond
         ( (or (string=? (cdr token) "t")
               (string=? (cdr token) "true")) (cons 'BOOL #t))
         ( (or (string=? (cdr token) "f")
               (string=? (cdr token) "false")) (cons 'BOOL #f))
         ( (string=? (cdr token) "nil") 'NIL)
         ( (string=? (cdr token) "eof") 'EOF-TOKEN )
         ( (or (string=? (cdr token) "function")
               (string=? (cdr token) "func")) 'FUNCTION)
         ( else (cons 'ERROR "unknown hash-notation")))
        (cons 'ERROR "unknown hash-notation"))))

(define (dispatch-id-operator-read port)
  (let* ((token (read-identifier "" port 'SYM)))
    (if (and (pair? token)
             (eq? (car token) 'SYM))
        (cond
         ((string=? (cdr token) "*")   'MULTIPLY)
         ((string=? (cdr token) "/")   'DEVIDE)
         ((string=? (cdr token) "mod") 'MODULO)
         ((string=? (cdr token) "-")   'MINUS)
         ((string=? (cdr token) "+")   'ADD)
         ((string=? (cdr token) "%")   'FOLD)
         ((string=? (cdr token) "=")   'ASSIGN)
         ((string=? (cdr token) "**")  'EXP)

         ((string=? (cdr token) "==")  'EQUAL)
         ((string=? (cdr token) "<>")  'UNEQUAL)
         ((string=? (cdr token) "<=>") 'COMPARE)
         ((string=? (cdr token) "<")   'LESS)
         ((string=? (cdr token) "<=")  'LESSEQ)
         ((string=? (cdr token) ">")   'GREATER)
         ((string=? (cdr token) ">=")  'GREATEREQ)

         ((string=? (cdr token) "<<")  'SHIFTLEFT)
         ((string=? (cdr token) ">>")  'SHIFTRIGHT)

         ((string=? (cdr token) "and") 'AND)
         ((string=? (cdr token) "or")  'OR)

         ((string=? (cdr token) "in")  'IN)
         ((string=? (cdr token) "by")  'BY)

         (else token))
        token)))

(define (return-and-next port type)
  (next-char port)
  type)

(define (tokenize-next-token port)
  (if (eof-object? current-char)
      'EOF
      (case current-char
        ( (#\space #\newline #\tab) (begin
                                      (next-char port)
                                      (tokenize-next-token port)) )
        ( (#\() (return-and-next port 'PRO))
        ( (#\)) (return-and-next port 'PRC))
        ( (#\[) (return-and-next port 'BRO))
        ( (#\]) (return-and-next port 'BRC))
        ( (#\{) (return-and-next port 'BRCO))
        ( (#\}) (return-and-next port 'BRCC))

        ( (#\,) (return-and-next port 'COMMA))
        ( (#\;) (return-and-next port 'SEMICOLON))
        ( (#\:) (return-and-next port 'COLON))
        ( (#\.) (begin
                  (next-char port)
                  (if (equal? current-char #\.)
                      (begin
                        (next-char port)
                        (if (equal? current-char #\.)
                            (return-and-next port 'ELLIPSIS)
                            'RANGE))
                      'DOT)) )
        ( (#\-) (begin
                  (next-char port)
                  (case current-char
                    ( (#\-) (begin
                              (read-comment-line port)
                              (tokenize-next-token port)) )
                    ( (#\>) (begin
                              (next-char port)
                              (if (is-symbol-char current-char)
                                  (read-identifier "->" port 'SYM)
                                  (return-and-next port 'MAPTO) )) )
                    ( else (if (or (is-alpha current-char)
                                   (is-alpha-spec current-char))
                               (read-identifier "-" port 'SYM)
                               (if (is-digit current-char)
                                   (read-number port #t)
                                   'MINUS)) ) )
                  ))

        ( (#\@) (return-and-next port 'AT))
        ( (#\&) (return-and-next port 'BITAND))
        ( (#\|) (return-and-next port 'BITOR))
        ( (#\^) (return-and-next port 'BITXOR))

        ( (#\+) (dispatch-id-operator-read port))
        ( (#\/) (dispatch-id-operator-read port))
        ( (#\*) (dispatch-id-operator-read port))
        ( (#\%) (dispatch-id-operator-read port))
        ( (#\<) (dispatch-id-operator-read port))
        ( (#\>) (dispatch-id-operator-read port))
        ( (#\=) (dispatch-id-operator-read port))

        ( (#\#) (begin
                  (next-char port)
                  (case current-char
                    ((#\#) (return-and-next port 'SANGHASH))
                    ((#\[) (return-and-next port 'CARRAYOP))
                    ((#\() (return-and-next port 'CVECTOP))
                    ((#\t #\f #\n #\e) (read-hash port))
                    (else (cons 'ERROR "unknown hash-notation")))))

        ( (#\") (read-string #\" port))

        ( (#\') (begin
                  (next-char port)
                  (if (is-symbol-char current-char)
                      (read-identifier "" port 'KEYW)
                      'QUOTE)))

        ( (#\\) (read-character port #f))

        ( (#\?) (begin
                  (next-char port)
                  (read-identifier "" port 'MACROPARAM)))

        (else (cond
               ((is-alpha current-char) (dispatch-id-operator-read port))
               ((is-alpha-spec current-char) (dispatch-id-operator-read port))
               ((is-digit current-char) (read-number port #f))

               (else (return-and-next port (cons 'UNKNOWN current-char)))))
        )))

(define (tokenize-port port)
  (next-char port)
  (let loop ((token (tokenize-next-token port)))
    (if (eq? token 'EOF)
        (begin
          (newline)
          #t)
        (begin
          (display token) (display " ") (newline)
          (loop (tokenize-next-token port))))) )

(define (tokenize-file filename)
  (let ((port (open-input-file filename)))
    (tokenize-port port)
    (close-input-port port)))



(define (token-value token)
  (cdr token))


(define (symbol-token? token)
  (and (pair? token)
       (eq? (car token) 'SYM)))


(define (keyarg-token? token)
  (and (pair? token)
       (eq? (car token) 'SYM)
       (let ((val (token-value token)))
         (equal? (string-ref val (- (string-length val) 1)) #\:))))

(define (keyarg-token-value token)
  (let ((val (cdr token)))
    (substring val 0 (- (string-length val) 1))))

(define (keyword-token? token)
  (and (pair? token)
       (eq? (car token) 'KEYW)))


(define (integer-token? token)
  (and (pair? token)
       (eq? (car token) 'INT)))


(define (char-token? token)
  (and (pair? token)
       (eq? (car token) 'CHAR)))


(define (string-token? token)
  (and (pair? token)
       (eq? (car token) 'STR)))


(define (bool-token? token)
  (and (pair? token)
       (eq? (car token) 'BOOL)))


(define (nil-token? token)
  (eq? token 'NIL))


(define (eof-token? token)
  (eq? token 'EOF-TOKEN))


(define (macroparam-token? token)
  (and (pair? token)
       (eq? (car token) 'MACROPARAM)))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
