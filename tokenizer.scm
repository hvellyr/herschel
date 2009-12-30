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

(define (parse-error msg detail)
  (arc:display "Notation error:" (number->string line-count) ": "
               msg ": " detail 'nl)
  #f)


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


(define (in-char-range? c from to)
  (and (char>=? c from)
       (char<=? c to)))


(define (is-whitespace c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)))


(define (is-digit c)
  (in-char-range? c #\0 #\9))


(define (is-hex-digit c)
  (or (in-char-range? c #\0 #\9)
      (in-char-range? c #\a #\f)
      (in-char-range? c #\A #\F)))


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
      (char=? c #\')
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
     ( (or (char=? lastc #\o)
           (char=? lastc #\O)) (string->number (substring str 0 (- l 1)) 8) )
     ( (or (char=? lastc #\y)
           (char=? lastc #\Y)) (string->number (substring str 0 (- l 1)) 2) )
     ( else (string->number str 10)))))


(define (read-int-number-part port pred)
  (let loop ((res '()))
    (if (or (eof-object? current-char)
            (not (pred current-char)))
        (list->string (reverse res))
        (let ((c current-char))
          (next-char port)
          (loop (cons c res))))))


(define (make-number-string first second exp-sign exp)
  (let ((tmp (string-append first (if (string? second)
                                      (string-append 
                                       "."
                                       second)))))
    (if (and exp-sign exp)
        (cond ((equal? exp-sign 1)
               (string-append tmp "e+" exp))
              ((equal? exp-sign -1)
               (string-append tmp "e-" exp))
              (else tmp))
        tmp)))


(define (read-number port neg?)
  (let ((first #f)
        (second #f)
        (exp-sign #f)
        (exp #f)
        (type 'int)
        (radix 10)
        (imaginary? #f)
        (expr #f))
    (set! first (read-int-number-part port is-hex-digit))
    (cond ((char=? current-char #\.)
           (begin
             (next-char port)
             (set! second (read-int-number-part port is-digit))
             (set! type 'real)
             (if (or (char=? current-char #\e)
                     (char=? current-char #\E))
                 (begin
                   (next-char port)
                   (cond ((char=? current-char #\-) 
                          (begin
                            (next-char port)
                            (set! exp-sign -1)
                            (set! exp (read-int-number-part port is-digit))))
                         ((char=? current-char #\+)
                          (begin
                            (next-char port)
                            (set! exp-sign 1)
                            (set! exp (read-int-number-part port is-digit))))
                         (else (parse-error "Bad number notation (0)" current-char)))))))
          ((char=? current-char #\/)
           (begin
             (next-char port)
             (set! second (read-int-number-part port is-digit))
             (set! type 'rational)))
          )
    ;;;
    (cond ((or (char=? current-char #\h)
               (char=? current-char #\H))
           (begin
             (if (not (eq? type 'int))
                 (parse-error "Unexpected integer notation char" current-char))
             (next-char port)
             (set! radix 16)))
          ((or (char=? current-char #\o)
               (char=? current-char #\O))
           (begin
             (if (not (eq? type 'int))
                 (parse-error "Unexpected integer notation char" current-char))
             (next-char port)
             (set! radix 8)))
          ((or (char=? current-char #\y)
               (char=? current-char #\Y))
           (begin
             (if (not (eq? type 'int))
                 (parse-error "Unexpected integer notation char" current-char))
             (next-char port)
             (set! radix 2))) )
    ;;;
    (cond ((or (char=? current-char #\f)
               (char=? current-char #\F))
           (begin
             (next-char port)
             (set! type 'float)))
          ((or (char=? current-char #\d)
               (char=? current-char #\D))
           (begin
             (next-char port)
             (cond ((char=? current-char #\l)
                    (begin
                      (next-char port)
                      (set! type 'long-double)))
                   ((is-delimiter current-char)
                    (set! type 'double))
                   ((eof-object? current-char)
                    (set! type 'double))
                   (else (parse-error "Bad number notation (1)" current-char)))))
          ((or (char=? current-char #\u)
               (char=? current-char #\U))
           (begin
             (next-char port)
             (cond ((or (char=? current-char #\s)
                        (char=? current-char #\S))
                    (begin
                      (next-char port)
                      (set! type 'unsigned-short)))
                   ((or (char=? current-char #\w)
                        (char=? current-char #\W))
                    (begin
                      (next-char port)
                      (set! type 'unsigned-word)))
                   ((or (char=? current-char #\l)
                        (char=? current-char #\L))
                    (begin
                      (next-char port)
                      (set! type 'unsigned-long)))
                   (else (parse-error "Bad number notation (3)" current-char)))))
          ((or (char=? current-char #\t)
               (char=? current-char #\T))
           (begin
             (next-char port)
             (set! type 'octet)))
          ((or (char=? current-char #\s)
               (char=? current-char #\S))
           (begin
             (next-char port)
             (set! type 'short)))
          ((or (char=? current-char #\w)
               (char=? current-char #\W))
           (begin
             (next-char port)
             (set! type 'word)))
          ((or (char=? current-char #\l)
               (char=? current-char #\L))
           (begin
             (next-char port)
             (set! type 'long)))
          ((eof-object? current-char) 'ok)
          ((is-delimiter current-char) 'ok)
          ((or (char=? current-char #\j) (char=? current-char #\J)
               (char=? current-char #\i) (char=? current-char #\I)) 'ok)
          (else (parse-error "Bad number notation (2)" current-char)))
    ;;;
    (if (or (char=? current-char #\j) (char=? current-char #\J)
            (char=? current-char #\i) (char=? current-char #\I))
        (begin
          (next-char port)
          (set! imaginary? #t)))
    ;;;
    (set! expr
          (case type
            ((int) (let ((num (string->number first radix)))
                     (cons 'INT (if neg? (* num -1) num))))
            ((real) (let* ((numstr (make-number-string first second exp-sign exp))
                           (num (string->number numstr)))
                      (cons 'REAL (if neg? (* num -1) num))))
            ((rational) (let ((n1 (string->number first 10))
                              (n2 (string->number second 10)))
                          (cons 'RATIONAL (cons n1 n2))))
            ((float) (let* ((numstr (make-number-string first second exp-sign exp))
                            (num (string->number numstr)))
                       (cons 'FLOAT (if neg? (* num -1) num))))
            ((double) (let* ((numstr (make-number-string first second exp-sign exp))
                             (num (string->number numstr)))
                        (cons 'DOUBLE (if neg? (* num -1) num))))
            ((long-double) (let* ((numstr (make-number-string first second exp-sign exp))
                                  (num (string->number numstr)))
                             (cons 'LONGDOUBLE (if neg? (* num -1) num))))
            ((octet) (let ((num (string->number first radix)))
                       (if neg?
                           (parse-error
                            "Misplaced '-'.  Octets cannot be negative" neg?))
                       (cons 'OCTET num)))
            ((short) (let ((num (string->number first radix)))
                       (cons 'SHORT (if neg? (* num -1) num))))
            ((unsigned-short) (let ((num (string->number first radix)))
                                (if neg?
                                    (parse-error
                                     "Misplaced '-'.  Unsigned shorts cannot be negative"
                                     neg?))
                                (cons 'USHORT num)))
            ((word) (let ((num (string->number first radix)))
                      (cons 'WORD (if neg? (* num -1) num))))
            ((unsigned-word) (let ((num (string->number first radix)))
                               (if neg?
                                   (parse-error
                                    "Misplaced '-'.  Unsigned words cannot be negative"
                                    neg?))
                               (cons 'UWORD num)))
            ((long) (let ((num (string->number first radix)))
                      (cons 'LONG (if neg? (* num -1) num))))
            ((unsigned-long) (let ((num (string->number first radix)))
                               (if neg?
                                   (parse-error
                                    "Misplaced '-'.  Unsigned longs cannot be negative" neg?))
                               (cons 'ULONG num)))
            (else (parse-error "How this?" type))))
    (if imaginary?
        (cons 'IMAGINARY expr)
        expr)))


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
  (and (pair? token) (eq? (car token) 'INT)))


(define (imaginary-token? token)
  (and (pair? token) (eq? (car token) 'IMAGINARY)))


(define (real-token? token)
  (and (pair? token) (eq? (car token) 'REAL)))


(define (rational-token? token)
  (and (pair? token) (eq? (car token) 'RATIONAL)))


(define (octet-token? token)
  (and (pair? token) (eq? (car token) 'OCTET)))


(define (short-token? token)
  (and (pair? token) (eq? (car token) 'SHORT)))


(define (ushort-token? token)
  (and (pair? token) (eq? (car token) 'USHORT)))


(define (word-token? token)
  (and (pair? token) (eq? (car token) 'WORD)))


(define (uword-token? token)
  (and (pair? token) (eq? (car token) 'UWORD)))


(define (long-token? token)
  (and (pair? token) (eq? (car token) 'LONG)))


(define (ulong-token? token)
  (and (pair? token) (eq? (car token) 'ULONG)))


(define (float-token? token)
  (and (pair? token) (eq? (car token) 'FLOAT)))


(define (double-token? token)
  (and (pair? token) (eq? (car token) 'DOUBLE)))


(define (longdouble-token? token)
  (and (pair? token) (eq? (car token) 'LONGDOUBLE)))


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
