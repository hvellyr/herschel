;;  This file is part of the heather package
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


;;----------------------------------------------------------------------

(define-class <hea:internal-token-port> (<hea:token-port>) (tokenlist))
(define-class <hea:file-token-port> (<hea:token-port>) (file-port))

(define (translate-token token)
  (cond ((eq? token 'PRO)          (apt-punct "("))
        ((eq? token 'PRC)          (apt-punct ")"))
        ((eq? token 'BRO)          (apt-punct "["))
        ((eq? token 'BRC)          (apt-punct "]"))
        ((eq? token 'BRCO)         (apt-punct "{"))
        ((eq? token 'BRCC)         (apt-punct "}"))
        ((eq? token 'ASSIGN)       (apt-punct "="))
        ((eq? token 'COMMA)        (apt-punct ","))
        ((eq? token 'SEMICOLON)    (apt-punct ";"))
        ((eq? token 'COLON)        (apt-punct ":"))
        ((eq? token 'MAPTO)        (apt-id "->"))
        ((eq? token 'ELLIPSIS)     (apt-id "..."))
        ((eq? token 'RANGE)        (apt-id ".."))
        ((eq? token 'DOT)          (apt-punct "."))
        ((eq? token 'QUOTE)        (apt-punct "'"))
        ((eq? token 'CARRAYOP)     (apt-punct "#["))
        ((eq? token 'CVECTOP)      (apt-punct "#("))
        ((eq? token 'SANGHASH)     (apt-punct "##"))
        ((eq? token 'AT)           (apt-punct "@"))

        ((eq? token 'MULTIPLY)     (apt-id "*"))
        ((eq? token 'DEVIDE)       (apt-id "/"))
        ((eq? token 'MODULO)       (apt-id "mod"))
        ((eq? token 'MINUS)        (apt-id "-"))
        ((eq? token 'ADD)          (apt-id "+"))
        ((eq? token 'FOLD)         (apt-id "%"))
        ((eq? token 'EXP)          (apt-id "**"))
        ((eq? token 'EQUAL)        (apt-id "=="))
        ((eq? token 'UNEQUAL)      (apt-id "<>"))
        ((eq? token 'COMPARE)      (apt-id "<=>"))
        ((eq? token 'LESS)         (apt-id "<"))
        ((eq? token 'LESSEQ)       (apt-id "<="))
        ((eq? token 'GREATER)      (apt-id ">"))
        ((eq? token 'GREATEREQ)    (apt-id ">="))
        ((eq? token 'SHIFTLEFT)    (apt-id "<<"))
        ((eq? token 'SHIFTRIGHT)   (apt-id ">>"))
        ((eq? token 'AND)          (apt-id "and"))
        ((eq? token 'OR)           (apt-id "or"))
        ((eq? token 'IN)           (apt-id "in"))
        ((eq? token 'BY)           (apt-id "by"))
        ((eq? token 'BITAND)       (apt-id "&"))
        ((eq? token 'BITOR)        (apt-id "|"))
        ((eq? token 'BITXOR)       (apt-id "^"))

        ((keyword-token? token) (let ((x (token-value token)))
                                  (if (string? x)
                                      (apt-lit (string->symbol x) 'keyw)
                                      (apt-lit x 'keyw))))
        ((symbol-token? token)     (apt-id (token-value token)))
        ((macroparam-token? token) (apt-macro-param (token-value token)))

        ((integer-token? token)    (apt-lit (token-value token) 'int))
        ((imaginary-token? token)  (apt-lit (translate-token (token-value token)) 'imaginary))
        ((real-token? token)       (apt-lit (token-value token) 'real))
        ((rational-token? token)   (apt-lit (token-value token) 'rational))
        ((octet-token? token)      (apt-lit (token-value token) 'octet))
        ((short-token? token)      (apt-lit (token-value token) 'short))
        ((ushort-token? token)     (apt-lit (token-value token) 'ushort))
        ((word-token? token)       (apt-lit (token-value token) 'word))
        ((uword-token? token)      (apt-lit (token-value token) 'uword))
        ((long-token? token)       (apt-lit (token-value token) 'long))
        ((ulong-token? token)      (apt-lit (token-value token) 'ulong))
        ((float-token? token)      (apt-lit (token-value token) 'float))
        ((double-token? token)     (apt-lit (token-value token) 'double))
        ((longdouble-token? token) (apt-lit (token-value token) 'ldouble))

        ((char-token? token)       (apt-lit (token-value token) 'char))
        ((string-token? token)     (apt-lit (token-value token) 'str))
        ((bool-token? token)       (apt-lit (token-value token) 'bool))

        ((eq? token 'FUNCTION)     (apt-punct "#function"))
        ((nil-token? token)        (apt-lit '() 'nil))
        ((eof-token? token)        (apt-lit 'EOF-TOKEN 'eof))

        (else token)))


(define-method (initialise <hea:internal-token-port> args)
  (call-next-method)
  (slot-set! self 'tokenlist (list-ref args 0))
  self)


(define-method (next-token <hea:internal-token-port>)
  (let ((tl (slot-ref self 'tokenlist)))
    (if (not (null? tl))
        (let ((token (car tl)))
          (slot-set! self 'tokenlist (cdr tl))
          token)
        'EOF)))


(define-method (initialise <hea:file-token-port> args)
  (call-next-method)
  (let ((p (list-ref args 0)))
    (slot-set! self 'file-port p)
    (next-char p))
  self)


(define-method (next-token <hea:file-token-port>)
  (let* ((token (tokenize-next-token (slot-ref self 'file-port))))
    (translate-token token)))


;;----------------------------------------------------------------------

(define (syntax-error msg token)
  (arc:display "Syntax error:" (number->string line-count) ": "
               msg ": " token 'nl)
  #f)


(define (error-expected-token ctx where token)
  (arc:display "Syntax error: " (number->string line-count)
               ": " where ": Expected token " token ", got: "
               (current-token ctx) 'nl)
  #f)


;;----------------------------------------------------------------------

(define (parse-exprlist-until-def ctx port exprlist)
  (cond
   ((eq? (current-token port) 'EOF) exprlist)

   ((apt-id-eq? (current-token port) "def") exprlist)

   ((apt-punct-eq? (current-token port) "}") exprlist)

   (else (let ((expr (parse-expr ctx port)))
           (if expr
               (parse-exprlist-until-def ctx port
                                         (append exprlist (list expr)))
               #f))) ))


(define (parse-exprlist-until-brcc ctx port exprlist)
  (cond
   ((eq? (current-token port) 'EOF)
    exprlist)

   ((apt-id-eq? (current-token port) "def")
    (let ((expr (parse-def ctx port 'global)))
      (if expr
          (parse-exprlist-until-brcc ctx port
                                     (append exprlist (list expr)))
          #f)))

   ((apt-punct-eq? (current-token port) "}") exprlist)

   (else (let ((expr (parse-expr ctx port)))
           (if expr
               (parse-exprlist-until-brcc ctx port
                                          (append exprlist (list expr)))
               #f))) ))


(define (parse-exprlist ctx port exprlist)
  (cond
   ((eq? (current-token port) 'EOF) exprlist)
   ((apt-punct-eq? (current-token port) "}") exprlist)
   ((apt-punct-eq? (current-token port) ")") exprlist)

   (else (let ((expr (parse-expr ctx port)))
           (if expr
               (begin
                 (parse-exprlist ctx port (append exprlist (list expr))))
               exprlist))) ))


(define (parse-modifiers ctx port possible-mods)
  (let loop ((res '()))
    (if (and (apt-id? (current-token port))
             (member (apt-id-value (current-token port)) possible-mods))
        (let* ((sym (apt-id-value (current-token port))))
          (next-token port)
          (loop (append res (list sym))))
        res)))


(define (parse-group ctx port)
  (next-token port)
  (let ((retv (parse-expr ctx port)))
    (if (apt-punct-eq? (current-token port) ")")
        (begin
          (next-token port)
          retv)
        (syntax-error "Expected ), got" (current-token port)))))


(define (parse-block ctx port)
  (next-token port)
  (let ((retv (parse-exprlist-until-brcc ctx port '())))
    (if (apt-punct-eq? (current-token port) "}")
        (begin
          (next-token port)
          (apt-nested* "{" "}" retv))
        (syntax-error "Expected }, got" (current-token port)))))


(define (parse-if-expr ctx port)
  (let ((test-expr #f)
        (true-expr #f))
    (next-token port)

    (if (apt-punct-eq? (current-token port) "(")
        (begin
          (next-token port)
          (set! test-expr (parse-expr ctx port))
          (if (apt-punct-eq? (current-token port) ")")
              (begin
                (next-token port)
                (set! true-expr (parse-expr ctx port))

                (if (apt-id-eq? (current-token port) "else")
                    (begin
                      (next-token port)
                      (apt-seq (apt-id "if")
                               (apt-nested "(" ")" test-expr)
                               true-expr
                               (apt-id "else")
                               (parse-expr ctx port)))
                    (apt-seq (apt-id "if")
                             (apt-nested "(" ")" test-expr)
                             true-expr)))
              (syntax-error "if: Expected ), got " (current-token port))))
        (syntax-error "if: Expected (, got " (current-token port)))))


(define (parse-on ctx port possible-on-keys)
  (if (apt-id? (current-token port))
      (let ((sym (current-token port)))
        (let* ((macro-id (apt-id-value sym))
               (macro    (lookup-macro ctx macro-id))
               (type     (lookup-macro-type ctx macro-id)))
          (if macro
              (parse-make-macro-call ctx port sym '()
                                     macro type #t 'local)
              (if (member (apt-id-value sym) possible-on-keys)
                  (begin
                    (next-token port)
                    (if (apt-punct-eq? (current-token port) "(")
                        (begin
                          (next-token port)
                          (let* ((params (parse-functions-params ctx port
                                                                 '()))
                                 (expr (parse-expr ctx port)))
                            (apt-seq (apt-id "on")
                                     sym
                                     (apt-nested* "(" ")" params)
                                     expr)))
                        (error-expected-token ctx 'on "(")))
                  (syntax-error "Unexpected 'on' expr:" (apt-id-value sym))))))
      (syntax-error "Unexpected 'on' expr:" (current-token port))))


(define (parse-funcall-params-del ctx port params)
  (cond
   ((apt-punct-eq? (current-token port) ",")
    (let ((comma (current-token port)))
      (next-token port)
      (parse-funcall-params ctx port (append params (list comma)))))

   ((apt-punct-eq? (current-token port) ")")
    (begin
      (next-token port)
      params))

   (else (syntax-error "Expect , or ), got" (current-token port)))))


(define (parse-funcall-params ctx port params)
  (cond
   ((eq? (current-token port) 'EOF)
    (syntax-error "Unterminated argument list" (current-token port)))

   ((apt-id-keyarg? (current-token port))
    (let* ((key (current-token port))
           (val #f))
      (next-token port)
      (set! val (parse-expr ctx port))
      (if val
          (parse-funcall-params-del ctx port
                                    (append params
                                            (list (apt-seq key val))))
          #f)))

   ((apt-punct-eq? (current-token port) ")")
    (begin
      (next-token port)
      params))

   (else (let ((expr (parse-expr ctx port)))
           (if expr
               (parse-funcall-params-del ctx port
                                         (append params (list expr)))
               #f))) ))


(define (apt-find-replace-token token bindings)
  (let* ((name (apt-macro-param-name token))
         (replc-token-pair (assoc name bindings)))
    (if replc-token-pair
        (cadr replc-token-pair)
        (begin
          (syntax-error "Undefined macro param '" name "'" 'nl)
          #f))))


(define (replace-match-bindings template bindings)
  (let loop ((nl template)
             (res '()))
    (if (null? nl)
        res
        (let* ((token (car nl))
               (replc-token (cond ((apt-punct? token) token)
                                  ((apt-lit? token) token)
                                  ((apt-id? token) token)

                                  ((apt-nested? token)
                                   (apt-nested* (apt-nested-left token)
                                                (apt-nested-right token)
                                                (replace-match-bindings
                                                 (apt-nested-body token)
                                                 bindings)))
                                  ((apt-seq? token)
                                   (apt-seq* (replace-match-bindings
                                              (apt-seq-body token) bindings)))
                                  ((apt-macro-param? token)
                                   (apt-find-replace-token token bindings))
                                  (else #f))))
          (if replc-token
              (loop (cdr nl) (append res (list replc-token)))
              (loop (cdr nl) res)) ))))


(define (match-syntax ctx port syntax-table)
  (let check-next ((node (cadar (vector-ref syntax-table 1)))
                   (token (current-token port))
                   (bindings '()))
    ;;(arc:display "x5 node " token 'nl)
    (let ((follow-set (st-find-node node (current-token port))))
      (cond (follow-set
             (check-next follow-set (next-token port) bindings))

            ((st-has-eof-set? node)
             (begin
               (let ((replcment (replace-match-bindings (st-replacement node)
                                                        bindings)))
                 replcment)))

            (else
             (begin
               (set! follow-set (st-find-macro-param node))
               (if follow-set
                   (let* ((macro-param (st-extract-macro-param node))
                          (macro-prm-type (apt-macro-param-type macro-param)))
                     (cond ((eq? macro-prm-type 'expr)
                            (begin
                              (let* ((expr (parse-expr ctx port))
                                     (param-name (apt-macro-param-name macro-param))
                                     (new-bindings (append
                                                    bindings
                                                    (list (cons param-name (list expr)))) ))
                                (check-next follow-set (current-token port) new-bindings))))

                           ((eq? macro-prm-type 'name)
                            (if (apt-id? (current-token port))
                                (begin
                                  (let* ((macro-name (apt-macro-param-name macro-param))
                                         (new-bindings
                                          (append
                                           bindings
                                           (list (cons macro-name
                                                       (list (current-token port))))) ))
                                    (check-next follow-set (next-token port) new-bindings)))
                                (syntax-error "Expected an symbol got a "
                                              (current-token port))))
                           (else (syntax-error "Expected ?*:expr or ?*:name"))))
                   #f)
               ) ))
      )))


(define (parse-do-match-syntax-func ctx port expr args syntax-table
                                    parse-parameters?)
  ;;(arc:display "x3 " (current-token port) " - " parse-parameters? " - " args 'nl)
  (let ((old-current-token (current-token port)))
    (if parse-parameters?
        (begin
          (unread-token port old-current-token)
          (if args
              (let ((next-tk (next-token port)))
                (unread-token port next-tk)
                (if (not (apt-punct-eq? next-tk ")"))
                    (unread-token port (apt-punct ",")))
                (unread-token port args)))
          (unread-token port (apt-punct "(")))
        (begin
          (unread-token port old-current-token)
          (unread-token port (apt-punct ")"))
          (let loop ((nl args)
                     (res '()))
            (if (null? nl)
                (for-each (lambda (x)
                            (unread-token port x) )
                          res)
                (loop (cdr nl)
                      (if (not (null? (cdr nl)))
                          (cons (car nl)
                                (cons (apt-punct ",") res))
                          (cons (car nl) res)))))
          (unread-token port (apt-punct "(")) ))
    ;;(arc:display "x4 " expr " - " (current-token port) 'nl)
    (current-token-set! port expr)
    (match-syntax ctx port syntax-table)) )


(define (parse-do-match-syntax-def ctx port syntax-table scope)
  (unread-token port (current-token port))
  (current-token-set! port (cond ((eq? scope 'local) (apt-id "let"))
                                 ((eq? scope 'global) (apt-id "def"))
                                 (else (apt-id "unknown"))))
  (match-syntax ctx port syntax-table))


(define (parse-do-match-syntax-on ctx port syntax-table scope)
  (unread-token port (current-token port))
  (current-token-set! port (apt-id "on"))
  (match-syntax ctx port syntax-table))


(define (parse-make-macro-call ctx port expr args macro type
                               parse-parameters? scope)
  ;;(arc:display "x1 " (current-token port) " - " type 'nl)
  (let* ((syntax-table (vector-ref macro 2))
         (filtered (cond ((or (eq? type 'func)
                              (eq? type 'stmt))
                          (parse-do-match-syntax-func ctx port
                                                      expr args
                                                      syntax-table
                                                      parse-parameters?))
                         ((eq? type 'def)
                          (parse-do-match-syntax-def ctx port
                                                     syntax-table scope))
                         ((eq? type 'on)
                          (parse-do-match-syntax-on ctx port
                                                    syntax-table scope))
                         (else #f))) )
    ;;;(arc:display "x2 " filtered 'nl)
    (if (and (list? filtered)
             (> (length filtered) 0))
        (let* ((follows filtered)
               (last-current-token (current-token port))
               (retval #f)
               (temp-port (make-object <hea:internal-token-port>
                                       (list (cdr follows)))))
          (push-port port temp-port)
          (current-token-set! port (car follows))
          (set! retval (apt-seq* (parse-exprlist ctx port '())))
          (pop-port port)
          (current-token-set! port last-current-token)
          retval)
        #f)))


(define (parse-function-call ctx port expr pre-scanned-args
                             parse-parameters?)
  (if parse-parameters?
      (let* ((prms (parse-funcall-params ctx port '())))
        (if prms
            (let ((prms2 (if pre-scanned-args
                             (if (and (list prms)
                                      (> (length prms) 0))
                                 (append (list pre-scanned-args
                                               (apt-punct ",")) prms)
                                 (list pre-scanned-args))
                             prms)) )
              (apt-funcall expr prms2))
            #f))
      (apt-funcall expr pre-scanned-args)))


(define (parse-param-call ctx port expr pre-scanned-args parse-parameters?)
  (if (apt-id? expr)
      (let* ((macro-id (apt-id-value expr))
             (macro    (lookup-macro ctx macro-id))
             (type     (lookup-macro-type ctx macro-id)))
        (if macro
            (parse-make-macro-call ctx port expr
                                   pre-scanned-args macro type
                                   parse-parameters? 'local)
            (parse-function-call ctx port expr pre-scanned-args
                                 parse-parameters?)))
      (parse-function-call ctx port expr pre-scanned-args
                           parse-parameters?)))


(define (parse-slice ctx port expr1)
  (let ((index (parse-expr ctx port)))
    (if (apt-punct-eq? (current-token port) "]")
        (if index
            (begin
              (next-token port)
              (apt-funcall (apt-id "slice")
                           (append (list expr1 (apt-punct ","))
                                   (list index)) ))
            #f)
        (syntax-error "Expected ], got" (current-token port)))))


(define (parse-access ctx port expr1)
  (cond
   ((apt-punct-eq? (current-token port) "(")
    (begin
      (next-token port)
      (parse-access ctx port (parse-param-call ctx port expr1 #f #t))))
   ((apt-punct-eq? (current-token port) "[")
    (begin
      (next-token port)
      (parse-access ctx port (parse-slice ctx port expr1))))

   ((apt-punct-eq? (current-token port) ".")
    (begin
      (next-token port)
      (if (apt-id? (current-token port))
          (let ((sym (current-token port)))
            (next-token port)
            (cond ((apt-punct-eq? (current-token port) "(")
                   (begin
                     (next-token port)
                     (parse-access ctx port
                                   (parse-param-call ctx port sym expr1 #t))))

                  ((or (apt-punct-eq? (current-token port) "[")
                       (apt-punct-eq? (current-token port) "."))
                   (parse-access ctx port (parse-param-call ctx port sym
                                                            (list expr1) #f)))
                  (else (parse-param-call ctx port sym
                                          (list expr1) #f))))
          (syntax-error "Expected symbol, got" (current-token port)) )))
   (else
    expr1)))


(define (parse-const-container ctx port del-token? left right)
  (let loop ((res '()))
    (if (del-token? (current-token port))
        (begin
          (next-token port)
          (apt-nested* left right res))
        (let ((expr (parse-expr ctx port)))
          (if (apt-punct-eq? (current-token port) ",")
              (begin
                (next-token port)
                (loop (append res (list expr (apt-punct ",")))))
              (loop (append res (list expr)))) ))))


(define (parse-function ctx port)
  (if (apt-punct-eq? (current-token port) "(")
      (begin
        (next-token port)
        (parse-func-def ctx port #f '() 'local))
      (syntax-error "function: Expected (, got" (current-token port))))


;;----------------------------------------------------------------------
;; expressions
;;----------------------------------------------------------------------

(define (parse-atom-expr ctx port)
  (cond
   ;;; literals
   ((or (apt-lit? (current-token port))
        (apt-seq? (current-token port))
        (apt-nested? (current-token port)))
    (let ((val (current-token port)))
      (next-token port)
      val))

   ;; groups
   ((apt-punct-eq? (current-token port) "(")
    (parse-access ctx port (parse-group ctx port)))

   ;; blocks
   ((apt-punct-eq? (current-token port) "{")
    (parse-access ctx port (parse-block ctx port)))

   ((apt-id? (current-token port))
    (let ((sym (apt-id-value (current-token port))))
      (cond
       ((equal? sym "if") (parse-if-expr ctx port))
       ((equal? sym "let") (parse-def ctx port 'local))
       ((equal? sym "on") (begin
                            (next-token port)
                            (parse-on ctx port '("signal" "exit"))))

       (else (begin
               (next-token port)
               (parse-access ctx port (apt-id sym)))) )
      ))

   ((apt-punct-eq? (current-token port) "#(")
    (begin
      (next-token port)
      (parse-const-container ctx port
                             (lambda (token) (apt-punct-eq? token ")"))
                             "#(" ")")))

   ((apt-punct-eq? (current-token port) "#[")
    (begin
      (next-token port)
      (parse-const-container ctx port
                             (lambda (token) (apt-punct-eq? token "]"))
                             "#[" "]")))

   ((apt-punct-eq? (current-token port) "#function")
    (begin
      (next-token port)
      (parse-function ctx port)))

   (else (syntax-error "Unexpected token (1)" (current-token port)))))


(define (operator->int op)
  (case op
    ((fold mapto by in)        1)
    ((range ellipsis)          2)
    ((and or)                  3)
    ((bitand bitor bitxor)     4)
    ((equal unequal less
      lesseq greater
      greatereq compare)       5)
    ((exp
      shiftleft shiftright)    8)
    ((add min)                 9)
    ((mul div mod)            10)

    ((dot)                    20)
    (else 999999)))


(define (operator>? op1 op2)
  (> (operator->int op1)
     (operator->int op2)))


(define (operator-right? op1)
  (case op1
    ((assign) #t)
    (else #f)))


(define (parse-operator ctx port)
  (let ((curtok (current-token port)))
    (cond ((apt-punct-eq? curtok "=")    'assign)
          ((apt-id-eq? curtok "+")       'add)
          ((apt-id-eq? curtok "-")       'min)
          ((apt-id-eq? curtok "*")       'mul)
          ((apt-id-eq? curtok "/")       'div)
          ((apt-id-eq? curtok "mod")     'mod)
          ((apt-id-eq? curtok "%")       'fold)
          ((apt-id-eq? curtok "**")      'exp)
          ((apt-id-eq? curtok "==")      'equal)
          ((apt-id-eq? curtok "<>")      'unequal)
          ((apt-id-eq? curtok "<")       'less)
          ((apt-id-eq? curtok "<=")      'lesseq)
          ((apt-id-eq? curtok ">")       'greater)
          ((apt-id-eq? curtok ">=")      'greatereq)
          ((apt-id-eq? curtok "<=>")     'compare)
          ((apt-id-eq? curtok "&")       'bitand)
          ((apt-id-eq? curtok "|")       'bitor)
          ((apt-id-eq? curtok "^")       'bitxor)
          ((apt-id-eq? curtok "and")     'and)
          ((apt-id-eq? curtok "or")      'or)
          ((apt-id-eq? curtok "->")      'mapto)
          ((apt-id-eq? curtok "..")      'range)
          ((apt-id-eq? curtok "by")      'by)
          ((apt-id-eq? curtok "...")     'ellipsis)

          ((apt-id-eq? curtok "<<")      'shiftleft)
          ((apt-id-eq? curtok ">>")      'shiftright)
          ((apt-id-eq? curtok "in")      'in)
          ((apt-punct-eq? curtok ".")    'dot)

          ((eq? curtok 'EOF)      #f)
          (else #f))))


(define (apt-sym-funcall? expr)
  (and (list? expr)
       (eq? (car expr) 'seq)
       (apt-id? (cadr expr))
       (apt-nested? (caddr expr))))


(define (apt-func-name-add-! expr)
  (apt-id (string-append (cadr (cadr expr)) "!")))


(define (apt-func-params expr)
  (cdddr (caddr expr)))


(define (apt-assign left right)
  (if (apt-sym-funcall? left)
      (apt-seq (apt-func-name-add-! left)
               (apt-nested* "(" ")" (append (append (apt-func-params left)
                                                    (list (apt-punct ",")))
                                            (list right))))
      (apt-seq left (apt-punct "=") right)))


(define (apt-operator left op right)
  (let ((op-sym (case op
                  ((assign)     (apt-punct "="))
                  ((add)        (apt-id "+"))
                  ((min)        (apt-id "-"))
                  ((mul)        (apt-id "*"))
                  ((div)        (apt-id "/"))
                  ((mod)        (apt-id "mod"))
                  ((fold)       (apt-id "%"))
                  ((exp)        (apt-id "**"))
                  ((equal)      (apt-id "=="))
                  ((unequal)    (apt-id "<>"))
                  ((less)       (apt-id "<"))
                  ((lesseq)     (apt-id "<="))
                  ((greater)    (apt-id ">"))
                  ((greatereq)  (apt-id ">="))
                  ((compare)    (apt-id "<=>"))
                  ((bitand)     (apt-id "&"))
                  ((bitor)      (apt-id "|"))
                  ((bitxor)     (apt-id "^"))
                  ((and)        (apt-id "and"))
                  ((or)         (apt-id "or"))
                  ((mapto)      (apt-id "->"))
                  ((range)      (apt-id ".."))
                  ((by)         (apt-id "by"))
                  ((ellipsis)   (apt-id "..."))
                  ((shiftleft)  (apt-id "<<"))
                  ((shiftright) (apt-id ">>"))
                  ((in)         (apt-id "in"))
                  ((dot)        (apt-id "."))
                  (else #f))))
    (if (eq? op 'assign)
        (apt-assign left right)
        (apt-seq left op-sym right))))


(define (parse-expr ctx port)
  (letrec ((loop (lambda (expr1 op1)
                   (let ((expr2 #f))
                     (if (not op1)
                         expr1
                         (begin
                           (next-token port)
                           (set! expr2 (parse-atom-expr ctx port))

                           (if expr2
                               (let ((op2 (parse-operator ctx port)))
                                 (if (not op2)
                                     (apt-operator expr1 op1 expr2)
                                     (if (and (not (operator-right? op1))
                                             (operator>? op1 op2) )
                                         (loop (apt-operator expr1 op1 expr2) op2)
                                         (apt-operator expr1 op1 (loop expr2 op2)))))
                               #f)
                           ))) )))
    (let ((expr1 (parse-atom-expr ctx port)))
      (if expr1
          (let ((op1 (parse-operator ctx port)) )
            (if op1
                (loop expr1 op1)
                expr1))
          #f))))


;;----------------------------------------------------------------------
;; type
;;----------------------------------------------------------------------

(define (parse-type-params ctx port del-token? params)
  (if (apt-id? (current-token port))
      (let ((type (parse-type ctx port)))
        (cond ((apt-punct-eq? (current-token port) ",")
               (let ((comma (current-token port)))
                 (next-token port)
                 (parse-type-params ctx port del-token?
                                    (append params
                                            (list type comma)))))
              ((del-token? (current-token port))
               (append params (list type)))

              (else (syntax-error "Unexpected token (8)" (current-token port))) ))
      params))


(define (parse-complex-type-del ctx port type-list)
  (cond ((apt-punct-eq? (current-token port) ",")
         (begin
           (next-token port)
           (parse-complex-type ctx port type-list)))
        ((apt-punct-eq? (current-token port) ")")
         (begin
           (next-token port)
           (apt-seq* type-list)))
        (else (syntax-error "complex type: expected , or ), got: "
                            (current-token port)))))


(define (parse-complex-type ctx port type-list)
  (cond ((apt-id? (current-token port))
         (let ((ty (parse-simple-type ctx port #t)))
           (if ty
               (parse-complex-type-del ctx port (append type-list (list ty)))
               #f)))
        ((apt-punct-eq? (current-token port) "(")
         (let ((ty (parse-complex-type ctx port '())))
           (if ty
               (parse-complex-type-del ctx port (append type-list (list ty)))
               #f)))
        (else (syntax-error "complex type: expected ID or (, got: "
                            (current-token port)))))


(define (parse-array-type ctx port base-type)
  (if (apt-punct-eq? (current-token port) "[")
      (let ((size-param #f))
        (next-token port)
        (if (not (apt-punct-eq? (current-token port) "]"))
            (set! size-param (parse-expr ctx port)))
        (if (apt-punct-eq? (current-token port) "]")
            (begin
              (next-token port)
              (if size-param
                  (apt-seq base-type (apt-nested "[" "]" size-param))
                  (apt-seq base-type (apt-nested "[" "]"))))
            (syntax-error "Expected ], got" (current-token port))))
      base-type))


(define (parse-function-type ctx port)
  (let ((params (parse-functions-params ctx port '()))
        (rettype #f))
    (if (apt-punct-eq? (current-token port) ":")
        (begin
          (next-token port)
          (set! rettype (parse-type ctx port))))
    (apt-seq* (list (apt-id "Function")
                    (apt-nested* "(" ")" params)
                    (apt-punct ":")
                    rettype))))

(define (parse-simple-type ctx port expect-constraint?)
  (let* ((sym (current-token port)))
    (next-token port)
    (cond ((apt-punct-eq? (current-token port) "(")
           (begin
             (next-token port)
             (if (equal? (apt-id-value sym) "Function")
                 (parse-function-type ctx port)
                 (let ((params (parse-type-params ctx port
                                                  (lambda (token)
                                                    (apt-punct-eq? token ")"))
                                                  '())))
                   (if (apt-punct-eq? (current-token port) ")")
                       (begin
                         (next-token port)
                         (parse-array-type ctx port
                                           (apt-seq sym (apt-nested* "(" ")"
                                                                     params))))
                       (syntax-error "Expected ), got" (current-token port))))) ))
          ((apt-punct-eq? (current-token port) ".")
           (begin
             (next-token port)
             (let loop ((res (list sym)))
               (cond ((apt-id? (current-token port))
                      (let ((sym2 (current-token port)))
                        (next-token port)
                        (if (apt-punct-eq? (current-token port) ".")
                            (begin
                              (next-token port)
                              (loop (append res (list (apt-punct ".") sym2))))
                            (apt-seq* (append res (list (apt-punct ".") sym2))))))
                     (else (apt-seq* res))))))
          ((and expect-constraint?
                (apt-punct-eq? (current-token port) "="))
           (begin
             (next-token port)
             (let ((constraint (parse-expr ctx port)))
               (if constraint
                   (apt-seq* (list sym (apt-punct "=") constraint))
                   #f))))
          (else (parse-array-type ctx port sym) ))))


(define (parse-type ctx port)
  (cond
   ((apt-id? (current-token port)) (parse-simple-type ctx port #f))
   ((apt-punct-eq? (current-token port) "(")
    (begin
      (next-token port)
      (parse-array-type ctx port (parse-complex-type ctx port '()))))
   (else (syntax-error "Expected type, got" (current-token port)))))


;;----------------------------------------------------------------------
;; def type
;;----------------------------------------------------------------------

(define (parse-type-slot-decl-params ctx port params)
  (cond ((apt-id-keyarg? (current-token port))
         (let* ((key (current-token port))
                (val #f))
           (next-token port)
           (set! val (parse-expr ctx port))
           (if val
               (if (apt-punct-eq? (current-token port) ",")
                   (let ((comma (current-token port)))
                     (next-token port)
                     (parse-type-slot-decl-params ctx port
                                                  (append params
                                                          (list (apt-seq key val)
                                                                comma))))
                   (append params (list (apt-seq key val))) )
               #f)))
        ((apt-id? (current-token port))
         (let ((sym (current-token port)))
           (next-token port)
           (if (apt-punct-eq? (current-token port) ",")
               (let ((comma (current-token port)))
                 (next-token port)
                 (parse-type-slot-decl-params ctx port
                                              (append params
                                                      (list sym comma))))
               (append params (list sym)))))
        (else params)))


(define (parse-type-slot-decl ctx port)
  (if (apt-id? (current-token port))
      (let* ((sym (current-token port))
             (type #f)
             (init-value #f)
             (params #f))
        (next-token port)
        (if (apt-punct-eq? (current-token port) ":")
            (begin
              (next-token port)
              (set! type (parse-type ctx port))))
        (if (apt-punct-eq? (current-token port) "=")
            (begin
              (next-token port)
              (set! init-value (parse-expr ctx port))))
        (if (apt-punct-eq? (current-token port) ",")
            (begin
              (next-token port)
              (set! params (parse-type-slot-decl-params ctx port '()))))

        (let ((res (list (apt-id "slot") sym)))
          (if type
              (set! res (append res (list (apt-punct ":") type))))
          (if init-value
              (set! res (append res (list (apt-punct "=") init-value))))
          (if params
              (set! res (append res (append (list (apt-punct ","))
                                            params))))
          (apt-seq* res)))
      (syntax-error "Expected symbol, got: " (current-token port))))


(define (parse-typedef-decls ctx port)
  (let loop ((res '()))
    (cond ((apt-id? (current-token port))
           (let* ((sym (apt-id-value (current-token port)))
                  (expr (cond ((equal? sym "slot")
                               (begin
                                 (next-token port)
                                 (parse-type-slot-decl ctx port)))
                              ((equal? sym "on")
                               (begin
                                 (next-token port)
                                 (parse-on ctx port '("init" "delete"))))
                              (else (syntax-error "Unexpected symbol:" (current-token port))))))
             (if expr
                 (loop (append res (list expr)))
                 #f)))
          ((apt-punct-eq? (current-token port) "}") res)
          (else (syntax-error "Unexpect token:" (current-token port))))))


(define (parse-type-def ctx port modifiers type)
  (if (apt-id? (current-token port))
      (let ((sym (current-token port))
            (params #f)
            (derives-from #f)
            (decls #f))
        (next-token port)
        (if (apt-punct-eq? (current-token port) "(")
            (begin
              (next-token port)
              (set! params (parse-functions-params ctx port '()))))
        (if (apt-punct-eq? (current-token port) ":")
            (begin
              (next-token port)
              (set! derives-from (parse-type ctx port))))
        (case type
          ((class) (if (apt-punct-eq? (current-token port) "{")
                       (begin
                         (next-token port)
                         (set! decls (parse-typedef-decls ctx port))
                         (if (apt-punct-eq? (current-token port) "}")
                             (begin
                               (next-token port)
                               (apt-class "class" sym params derives-from decls))
                             (error-expected-token ctx 'type-def "}")))
                       (error-expected-token ctx 'type-def "{")))
          ((type) (apt-class "type" sym params derives-from #f))
          (else (syntax-error "programming error" type))))
      (error-expected-token ctx 'type-def "symbol")))


;;----------------------------------------------------------------------
;; macros
;;----------------------------------------------------------------------

(define (macro-determine-pattern-type pattern)
  (let loop ((nl pattern)
             (state 'init)
             (prc-count 0))
    (if (null? nl)
        (if (eq? state 'scan-for-expr)
            'func
            'any)
        (let ((elt (car nl)))
          (case state
            ((init) (if (apt-id? elt)
                        (cond ((or (apt-id-eq? elt "def")
                                   (apt-id-eq? elt "let")) 'def)
                              ((apt-id-eq? elt "on") 'on)
                              (else (loop (cdr nl) 'scan-syms prc-count)))
                        'any))
            ((scan-syms) (cond ((apt-id? elt)
                                (loop (cdr nl) 'scan-syms prc-count))
                               ((and (apt-punct? elt)
                                     (equal? (apt-punct-value elt) "("))
                                (loop (cdr nl) 'scan-for-prc (+ prc-count 1)))
                               (else 'any)))
            ((scan-for-prc) (if (and (apt-punct? elt)
                                     (equal? (apt-punct-value elt) ")"))
                                (if (equal? prc-count 1)
                                    (loop (cdr nl) 'scan-for-expr 0)
                                    (loop (cdr nl) 'scan-for-prc (- prc-count 1)))
                                (loop (cdr nl) 'scan-for-prc prc-count)))
            ((scan-for-expr) 'stmt)

            (else 'any))) )))


(define (macro-determine-type macro-patterns)
  (let pattern-loop ((pnl macro-patterns)
                     (last-type 'any))
    (if (null? pnl)
        last-type
        (let* ((mp (macro-prod-pattern (car pnl)))
               (pattern-type (macro-determine-pattern-type mp)))
          (if (or (eq? last-type 'any)
                  (eq? last-type pattern-type))
              (pattern-loop (cdr pnl) pattern-type)
              (begin
                (arc:display "Macro has inconsistent patterns: "
                             (car pnl) 'nl)
                'any)))) ))


(define (parse-macro-comp ctx port)
  (if (apt-punct-eq? (current-token port) "{")
      (begin
        (next-token port)
        (let loop ((res '())
                   (brace-count 1))
          (let ((token (current-token port)))
            (cond ((apt-punct-eq? (current-token port) "}")
                   (let ((count (- brace-count 1)))
                     (next-token port)
                     (if (<= count 0)
                         res
                         (loop (append res (list token))
                               count))))
                  ((apt-punct-eq? (current-token port) "{")
                   (let ((count (+ brace-count 1)))
                     (next-token port)
                     (loop (append res (list token))
                           count)))
                  (else (begin
                          (next-token port)
                          (loop (append res (list token))
                                brace-count)) )))))
      (syntax-error "Expected {, got" (current-token port))))


(define (parse-macro-basic-pattern ctx port pattern-name)
  (let ((pattern (parse-macro-comp ctx port))
        (replc #f))
    (if pattern
        (if (apt-id-eq? (current-token port) "->")
            (begin
              (next-token port)
              (set! replc (parse-macro-comp ctx port))
              (list 'prod
                    ':name pattern-name
                    ':pattern pattern ':replc replc))
            (syntax-error "Expected ->, got" (current-token port)))
        #f)))


(define (parse-macro-patterns ctx port)
  (let loop ((res '())
             (last-pattern-name #f))
    (cond ((apt-punct-eq? (current-token port) "}")  res)
          ((apt-punct-eq? (current-token port) "{")
           (let ((expr (parse-macro-basic-pattern ctx port
                                                  last-pattern-name)))
             (if expr
                 (loop (append res (list expr))
                       last-pattern-name)
                 #f)))
          ((apt-id-keyarg? (current-token port)) (let ((sym (current-token port)))
                                            (next-token port)
                                            (loop res (apt-id-keyarg-value sym))))
          (else (syntax-error "Unexpected token (9)" (current-token port))))))


(define (parse-macro-def ctx port modifiers)
  (if (apt-id? (current-token port))
      (let ((sym (apt-id-value (current-token port))))
        (next-token port)
        (if (apt-punct-eq? (current-token port) "{")
            (begin
              (next-token port)
              (let* ((patterns (parse-macro-patterns ctx port))
                     (macro-type (macro-determine-type patterns)))
                (if (apt-punct-eq? (current-token port) "}")
                    (begin
                      (next-token port)
                      (if patterns
                          (begin
                            (register-macro ctx
                                            macro-type sym
                                            (vector ':macro
                                                    patterns
                                                    (macro-compile (list 'macro
                                                                         ':patterns patterns))))
                            'ignore)
                          #f))
                    (syntax-error "Expected }, got" (current-token port))) ))
            (syntax-error "Expected {, got" (current-token port))))
      (syntax-error "Expected symbol, got" (current-token port))))


(define (parse-alias-def ctx port modifiers)
  (if (apt-id? (current-token port))
      (let* ((sym (current-token port)))
        (next-token port)
        (if (apt-punct-eq? (current-token port) "=")
            (begin
              (next-token port)
              (let ((type (parse-type ctx port)))
                (apt-seq (apt-id "def") (apt-id "alias") sym (apt-punct "=") type)))
            (syntax-error "Expected =, got" (current-token port))))
      (syntax-error "Expected symbol, got" (current-token port))))


(define (apt-param keyarg sym flag type init-value)
  (if (eq? flag 'rest)
      (apt-seq sym (apt-id "..."))
      (if (or (eq? flag 'spec) type init-value)
          (let ((nl '()))
            (if keyarg
                (set! nl (append nl (list keyarg))))
            (set! nl (append nl (list sym)))
            (if (or (eq? flag 'spec) type)
                (set! nl (append nl (list (apt-punct ":")))))
            (if (eq? flag 'spec)
                (set! nl (append nl (list (apt-punct "@")))))
            (if type
                (set! nl (append nl (list type))))
            (if init-value
                (set! nl (append nl (list (apt-punct "=")
                                          init-value))))
            (apt-seq* nl))
          sym)))


(define (parse-functions-params ctx port param-list)
  (cond
   ((apt-id? (current-token port))
    (let* ((sym #f)
           (type #f)
           (spec? #f)
           (keyarg #f)
           (init-value #f))
      (if (apt-id-keyarg? (current-token port))
          (begin
            (set! keyarg (current-token port))
            (next-token port)))
      (if (apt-id? (current-token port))
          (begin
            (set! sym (current-token port))
            (next-token port)

            (cond
             ((apt-id-eq? (current-token port) "...")
              (begin
                (next-token port)
                (if (apt-punct-eq? (current-token port) ")")
                    (begin
                      (next-token port)
                      (append param-list (list (apt-param #f sym 'rest type init-value))))
                    (syntax-error "Rest argument must be last in parameter list"
                                  (current-token port)))))

             ((apt-punct-eq? (current-token port) ":")
              (begin
                (next-token port)

                (if (apt-punct-eq? (current-token port) "@")
                    (begin
                      (next-token port)
                      (set! spec? 'spec)))

                (set! type (parse-type ctx port))

                (if (apt-punct-eq? (current-token port) "=")
                    (begin
                      (next-token port)
                      (if (eq? spec? 'spec)
                          (syntax-error "Keyed parameters can not be specialized"
                                        (current-token port)))
                      (set! spec? 'key)
                      (set! init-value (parse-expr ctx port))))

                (cond
                 ((apt-punct-eq? (current-token port) ",")
                  (begin
                    (next-token port)
                    (parse-functions-params ctx port
                                            (append param-list
                                                    (list (apt-param keyarg sym spec? type
                                                                     init-value))))))

                 ((apt-punct-eq? (current-token port) ")")
                  (begin
                    (next-token port)
                    (append param-list (list (apt-param keyarg sym spec? type init-value)))))

                 (else (syntax-error "Unexpected token (2)" (current-token port))) )))

             ((apt-punct-eq? (current-token port) "=")
              (begin
                (next-token port)
                (set! spec? 'key)
                (set! init-value (parse-expr ctx port))

                (cond
                 ((apt-punct-eq? (current-token port) ")")
                  (begin
                    (next-token port)
                    (append param-list (list (apt-param keyarg sym spec? type init-value)))))
                 ((apt-punct-eq? (current-token port) ",")
                  (begin
                    (next-token port)
                    (parse-functions-params ctx port
                                            (append param-list
                                                    (list (apt-param keyarg sym spec? type
                                                                     init-value))))))
                 (else (syntax-error "Unexpected token (3)" (current-token port))) )))

             ((apt-punct-eq? (current-token port) ",")
              (begin
                (next-token port)
                (parse-functions-params ctx port
                                        (append param-list
                                                (list (apt-param #f sym spec? type
                                                                 init-value))))))

             ((apt-punct-eq? (current-token port) ")")
              (begin
                (next-token port)
                (append param-list (list (apt-param #f sym spec? type init-value)))))

             (else (syntax-error "Unexpected token (4)" (current-token port))) ))

          (syntax-error "expected id" (current-token port)))))

   ((apt-punct-eq? (current-token port) ")")
    (begin
      (next-token port)
      param-list))

   (else (syntax-error "Unexpected token (5)" (current-token port))) ))


(define (apt-map-scope scope)
  (cond ((eq? scope 'global) (apt-id "def"))
        ((eq? scope 'local) (apt-id "let"))
        (else (apt-id "bad"))))


(define (apt-function params type body)
  (apt-seq (apt-punct "#function")
           (apt-nested* "(" ")" params)
           (if type (apt-punct ":") #f)
           (if type type #f)
           (if (equal? (length body) 1)
               (car body)
               (apt-nested* "{" "}" body))))


(define (apt-funcdef scope modifiers sym params type body)
  (let ((nl (list (apt-map-scope scope))))
    (if (member "meth" modifiers)
        (set! nl (append nl (list (apt-id "meth")))))
    (set! nl (append nl (list sym
                              (apt-nested* "(" ")" params))))
    (if type
        (set! nl (append nl (list (apt-punct ":") type))))
    (set! nl (append nl (list (if (equal? (length body) 1)
                                  (car body)
                                  (apt-nested* "{" "}" body)))))
    (apt-seq* nl)))


(define (parse-func-def ctx port sym modifiers scope)
  (let* ((params (parse-functions-params ctx port '()))
         (type (if (apt-punct-eq? (current-token port) ":")
                   (begin
                     (next-token port)
                     (parse-type ctx port))
                   #f))
         (body (if (eq? scope 'global)
                   (parse-exprlist-until-def ctx port '())
                   (list (parse-expr ctx port)))) )
    (if sym
        (apt-funcdef scope modifiers sym params type body)
        (apt-function params type body)) ))


(define (apt-vardef scope name type modifiers init)
  (let ((res '()))
    (set! res (list (apt-map-scope scope)))
    (set! res (append res (map (lambda (m)
                                 (apt-id m))
                               modifiers)))
    (set! res (append res (list name)))
    (if type
        (set! res (append res (list (apt-punct ":") type))))
    (if init
        (set! res (append res (list (apt-punct "=") init))))

    (apt-seq* res)))


(define (parse-func-or-var-def ctx port scope modifiers)
  (let* ((sym (current-token port))
         (macro-id (apt-id-value sym))
         (macro    (lookup-macro ctx macro-id))
         (type     (lookup-macro-type ctx macro-id)))
    (if macro
        (begin
          (let ((expr (parse-make-macro-call ctx port sym '()
                                             macro type #t scope)))
            (if expr
                expr
                (syntax-error "Incomplete macro appliance: " macro-id))))
        (begin
          (next-token port)
          (cond
           ((apt-punct-eq? (current-token port) "(")
            (begin
              (next-token port)
              (parse-func-def ctx port sym modifiers scope)))

           ((apt-punct-eq? (current-token port) ":")
            (begin
              (next-token port)
              (let ((type (parse-type ctx port)))
                (if (apt-punct-eq? (current-token port) "=")
                    (begin
                      (next-token port)
                      (let ((init-value (parse-expr ctx port)))
                        (apt-vardef scope sym type modifiers init-value)))
                    (apt-vardef scope sym type modifiers #f)))
              ))

           ((apt-punct-eq? (current-token port) "=")
            (begin
              (next-token port)
              (let ((init-value (parse-expr ctx port)))
                (apt-vardef scope sym #f modifiers init-value))))

           (else (apt-vardef scope sym #f modifiers #f))
           )))))


(define (parse-def ctx port scope)
  (next-token port)
  (let* ((modifiers (parse-modifiers ctx port '("meth" "fluid" "const"))))
    (if (apt-id? (current-token port))
        (cond
         ((apt-id-eq? (current-token port) "type")
          (begin
            (next-token port)
            (parse-type-def ctx port modifiers 'type)))
         ((apt-id-eq? (current-token port) "class")
          (begin
            (next-token port)
            (parse-type-def ctx port modifiers 'class)))
         ((apt-id-eq? (current-token port) "alias")
          (begin
            (next-token port)
            (parse-alias-def ctx port modifiers)))
         ((apt-id-eq? (current-token port) "macro")
          (begin
            (next-token port)
            (parse-macro-def ctx port modifiers)))
         ((apt-id? (current-token port))
          (parse-func-or-var-def ctx port scope modifiers))
         (else (syntax-error "Expected symbol, got" (current-token port))))
        (syntax-error "Expected symbol, got" (current-token port)))
     ))


(define (parse-namespace ctx port)
  (next-token port)
  (if (apt-id? (current-token port))
      (let ((nsname (current-token port)))
        (next-token port)
        (if (apt-punct-eq? (current-token port) "(")
            (begin
              (next-token port)
              (let ((str (parse-expr ctx port)))
                (if (apt-punct-eq? (current-token port) ")")
                    (begin
                      (next-token port)
                      (apt-seq (apt-id "namespace")
                               nsname
                               (apt-nested "(" ")" str)))
                    (syntax-error "Expected ), got" (current-token port)))))
            (apt-seq (apt-id "namespace")
                     nsname)))
      (syntax-error "Expected SYMBOL, got " (current-token port))))


(define (parse-import ctx port)
  (next-token port)
  (if (apt-punct-eq? (current-token port) "(")
      (begin
        (next-token port)
        (let ((prms (parse-funcall-params ctx port '())))
          (apt-seq (apt-id "import") (apt-nested* "(" ")" prms))))
      (syntax-error "import: Expected (, got" (current-token port))))


(define (parse-next-top ctx port)
  (let loop ((apt (list)))
    (if (eq? (current-token port) 'EOF)
        apt
        (begin
          (if (apt-id? (current-token port))
              (let* ((sym (apt-id-value (current-token port)))
                     (expr (cond
                            ((equal? sym "def") (parse-def ctx port 'global))
                            ((equal? sym "namespace") (parse-namespace ctx port))
                            ((equal? sym "import") (parse-import ctx port))
                            (else (syntax-error "Unexpected symbol"
                                                (current-token port))))) )
                (cond ((eq? expr 'ignore) (loop apt))
                      ((not expr) #f)
                      (else (loop (append apt (list expr))))))
              (syntax-error "Unexpected token (6)" (current-token port)))))) )



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
