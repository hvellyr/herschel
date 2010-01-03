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
  (hea:display "Syntax error:" (number->string line-count) ": "
               msg ": " token 'nl)
  #f)


(define (error-expected-token ctx where token)
  (hea:display "Syntax error: " (number->string line-count)
               ": " where ": Expected token " token ", got: "
               (current-token ctx) 'nl)
  #f)


;;----------------------------------------------------------------------

(define (parse-exprlist-until-def ctx exprlist)
  (cond
   ((eq? (current-token ctx) 'EOF) exprlist)

   ((apt-id-eq? (current-token ctx) "def") exprlist)

   ((apt-punct-eq? (current-token ctx) "}") exprlist)

   (else (let ((expr (parse-expr ctx)))
           (if expr
               (parse-exprlist-until-def ctx
                                         (append exprlist (list expr)))
               #f))) ))


(define (parse-exprlist-until-brcc ctx exprlist)
  (cond
   ((eq? (current-token ctx) 'EOF)
    exprlist)

   ((apt-id-eq? (current-token ctx) "def")
    (let ((expr (parse-def ctx 'global)))
      (if expr
          (parse-exprlist-until-brcc ctx
                                     (append exprlist (list expr)))
          #f)))

   ((apt-punct-eq? (current-token ctx) "}") exprlist)

   (else (let ((expr (parse-expr ctx)))
           (if expr
               (parse-exprlist-until-brcc ctx
                                          (append exprlist (list expr)))
               #f))) ))


(define (parse-exprlist* ctx exprlist parse-proc)
  (cond
   ((eq? (current-token ctx) 'EOF) exprlist)
   ((apt-punct-eq? (current-token ctx) "}") exprlist)
   ((apt-punct-eq? (current-token ctx) ")") exprlist)

   (else (let ((expr (parse-proc ctx)))
           (if expr
               (begin
                 (parse-exprlist* ctx (append exprlist (list expr))
                                  parse-proc))
               exprlist))) ))


(define (parse-exprlist ctx exprlist)
  (parse-exprlist* ctx exprlist parse-expr))


(define (parse-top-exprlist ctx exprlist)
  (parse-exprlist* ctx exprlist parse-top-expr))


(define (parse-modifiers ctx possible-mods)
  (let modifier-loop ((res '()))
    (if (and (apt-id? (current-token ctx))
             (member (apt-id-value (current-token ctx)) possible-mods))
        (let* ((sym (apt-id-value (current-token ctx))))
          (next-token ctx)
          (modifier-loop (append res (list sym))))
        res)))


(define (parse-group ctx)
  (next-token ctx)
  (let ((retv (parse-expr ctx)))
    (if (apt-punct-eq? (current-token ctx) ")")
        (begin
          (next-token ctx)
          retv)
        (syntax-error "Expected ), got" (current-token ctx)))))


(define (parse-block ctx)
  (next-token ctx)
  (let ((retv (parse-exprlist-until-brcc ctx '())))
    (if (apt-punct-eq? (current-token ctx) "}")
        (begin
          (next-token ctx)
          (apt-nested* "{" "}" retv))
        (syntax-error "Expected }, got" (current-token ctx)))))


(define (parse-if-expr ctx)
  (let ((test-expr #f)
        (true-expr #f))
    (next-token ctx)
    ;;;
    (if (apt-punct-eq? (current-token ctx) "(")
        (begin
          (next-token ctx)
          (set! test-expr (parse-expr ctx))
          (if (apt-punct-eq? (current-token ctx) ")")
              (begin
                (next-token ctx)
                (set! true-expr (parse-expr ctx))

                (if (apt-id-eq? (current-token ctx) "else")
                    (begin
                      (next-token ctx)
                      (apt-seq (apt-id "if")
                               (apt-nested "(" ")" test-expr)
                               true-expr
                               (apt-id "else")
                               (parse-expr ctx)))
                    (apt-seq (apt-id "if")
                             (apt-nested "(" ")" test-expr)
                             true-expr)))
              (syntax-error "if: Expected ), got " (current-token ctx))))
        (syntax-error "if: Expected (, got " (current-token ctx)))))


(define (parse-on ctx possible-on-keys)
  (if (apt-id? (current-token ctx))
      (let ((sym (current-token ctx)))
        (let* ((macro-id (qualified-id-for-lookup ctx sym))
               (macro    (lookup-macro ctx macro-id))
               (type     (lookup-macro-type ctx macro-id))
               (name     (apt-id (car macro-id))))
          (if macro
              (parse-make-macro-call ctx name '()
                                     macro type #t 'local)
              (if (member (apt-id-value sym) possible-on-keys)
                  (begin
                    (next-token ctx)
                    (if (apt-punct-eq? (current-token ctx) "(")
                        (begin
                          (next-token ctx)
                          (let* ((params (parse-functions-params ctx '()))
                                 (expr (parse-expr ctx)))
                            (apt-seq (apt-id "on")
                                     sym
                                     (apt-nested* "(" ")" params)
                                     expr)))
                        (error-expected-token ctx 'on "(")))
                  (syntax-error "Unexpected 'on' expr:" (apt-id-value sym))))))
      (syntax-error "Unexpected 'on' expr:" (current-token ctx))))


(define (parse-funcall-params-del ctx params)
  (cond
   ((apt-punct-eq? (current-token ctx) ",")
    (let ((comma (current-token ctx)))
      (next-token ctx)
      (parse-funcall-params ctx (append params (list comma)))))

   ((apt-punct-eq? (current-token ctx) ")")
    (begin
      (next-token ctx)
      params))

   (else (syntax-error "Expect , or ), got" (current-token ctx)))))


(define (parse-funcall-params ctx params)
  (cond
   ((eq? (current-token ctx) 'EOF)
    (syntax-error "Unterminated argument list" (current-token ctx)))

   ((apt-id-keyarg? (current-token ctx))
    (let* ((key (current-token ctx))
           (val #f))
      (next-token ctx)
      (set! val (parse-expr ctx))
      (if val
          (parse-funcall-params-del ctx
                                    (append params
                                            (list (apt-seq key val))))
          #f)))

   ((apt-punct-eq? (current-token ctx) ")")
    (begin
      (next-token ctx)
      params))

   (else (let ((expr (parse-expr ctx)))
           (if expr
               (parse-funcall-params-del ctx
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


(define (replace-sanghash-ids tokens)
  (let sanghash-loop ((nl tokens)
                      (res '()))
    (if (null? nl)
        res
        (let ((token (car nl)))
          (if (apt-id? token)
              (let ((operand1 token))
                (if (and (not (null? (cdr nl)))
                         (not (null? (cddr nl))))
                    (if (apt-punct-eq? (cadr nl) "##")
                        (if (apt-id? (caddr nl))
                            (sanghash-loop
                             (cdddr nl)
                             (append res
                                     (list (apt-id (string-append
                                                    (apt-id-value token)
                                                    (apt-id-value (caddr nl)))))))
                            (syntax-error "## requires right hand id value"
                                          tokens))
                        (sanghash-loop (cdr nl)
                                       (append res (list token))))
                    (sanghash-loop (cdr nl)
                                   (append res (list token)))))
              (sanghash-loop (cdr nl) (append res (list token))))))
    ))


(define (replace-match-bindings template bindings)
  (let bindings-loop ((nl template)
                      (res '()))
    (if (null? nl)
        (replace-sanghash-ids res)
        (let* ((token (car nl))
               (replc-token
                (cond ((apt-punct? token) token)
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
              (bindings-loop (cdr nl) (append res (list replc-token)))
              (bindings-loop (cdr nl) res)) ))))


(define (match-syntax ctx syntax-table)
  (let check-next ((node (cadar (vector-ref syntax-table 1)))
                   (token (current-token ctx))
                   (bindings '()))
    ;;(hea:display "x5 node " token 'nl)
    (let ((follow-set (st-find-node node (current-token ctx))))
      (cond (follow-set
             (check-next follow-set (next-token ctx) bindings))

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
                              (let* ((expr (parse-expr ctx))
                                     (param-name (apt-macro-param-name macro-param))
                                     (new-bindings (append
                                                    bindings
                                                    (list (cons param-name (list expr)))) ))
                                (check-next follow-set (current-token ctx) new-bindings))))

                           ((eq? macro-prm-type 'name)
                            (if (apt-id? (current-token ctx))
                                (begin
                                  (let* ((macro-name (apt-macro-param-name macro-param))
                                         (new-bindings
                                          (append
                                           bindings
                                           (list (cons macro-name
                                                       (list (current-token ctx))))) ))
                                    (check-next follow-set (next-token ctx) new-bindings)))
                                (syntax-error "Expected an symbol got a "
                                              (current-token ctx))))
                           (else (syntax-error "Expected ?*:expr or ?*:name"
                                               (current-token ctx)))))
                   #f)
               ) ))
      )))


(define (parse-do-match-syntax-func ctx expr args syntax-table
                                    parse-parameters?)
  ;;(hea:display "x3 " (current-token ctx) " - " parse-parameters? " - " args 'nl)
  (let ((old-current-token (current-token ctx)))
    (if parse-parameters?
        (begin
          (unread-token ctx old-current-token)
          (if args
              (let ((next-tk (next-token ctx)))
                (unread-token ctx next-tk)
                (if (not (apt-punct-eq? next-tk ")"))
                    (unread-token ctx (apt-punct ",")))
                (unread-token ctx args)))
          (unread-token ctx (apt-punct "(")))
        (begin
          (unread-token ctx old-current-token)
          (unread-token ctx (apt-punct ")"))
          (let syntax-loop ((nl args)
                            (res '()))
            (if (null? nl)
                (for-each (lambda (x)
                            (unread-token ctx x) )
                          res)
                (syntax-loop (cdr nl)
                             (if (not (null? (cdr nl)))
                                 (cons (car nl)
                                       (cons (apt-punct ",") res))
                                 (cons (car nl) res)))))
          (unread-token ctx (apt-punct "(")) ))
    ;;(hea:display "x4 " expr " - " (current-token ctx) 'nl)
    (current-token-set! ctx expr)
    (match-syntax ctx syntax-table)) )


(define (apt-map-scope scope)
  (cond ((eq? scope 'global) (apt-id "def"))
        ((eq? scope 'local) (apt-id "let"))
        (else (apt-id "bad"))))


(define (parse-do-match-syntax-def ctx on-sym syntax-table scope)
  (unread-token ctx on-sym)
  (current-token-set! ctx (apt-map-scope scope))
  (match-syntax ctx syntax-table))


(define (parse-do-match-syntax-on ctx on-sym syntax-table scope)
  (unread-token ctx on-sym)
  (current-token-set! ctx (apt-id "on"))
  (match-syntax ctx syntax-table))


(define (parse-make-macro-call ctx expr args macro type
                               parse-parameters? scope)
  ;;(hea:display "x1 " (current-token ctx) " - " type " - " args " -- " scope 'nl)
  (let* ((syntax-table (vector-ref macro 2))
         (filtered (cond ((or (eq? type 'func)
                              (eq? type 'stmt))
                          (parse-do-match-syntax-func ctx
                                                      expr args
                                                      syntax-table
                                                      parse-parameters?))
                         ((eq? type 'def)
                          (parse-do-match-syntax-def ctx expr
                                                     syntax-table scope))
                         ((eq? type 'on)
                          (parse-do-match-syntax-on ctx expr
                                                    syntax-table scope))
                         (else #f))) )
    (if (and (list? filtered)
             (> (length filtered) 0))
        (let* ((follows filtered)
               (last-current-token (current-token ctx))
               (retval #f)
               (temp-port (make-object <hea:internal-token-port>
                                       (list (cdr follows)))))
          (push-port (current-port ctx) temp-port)
          (current-token-set! ctx (car follows))
          (set! retval
                (cond ((eq? scope 'local)
                       (apt-seq* (parse-exprlist ctx '())))
                      ((eq? scope 'global)
                       (parse-top-exprlist ctx '()))
                      (else (syntax-error "Unknown scope" expr))))
          (pop-port (current-port ctx))
          (current-token-set! ctx last-current-token)
          retval)
        #f)))


(define (parse-function-call ctx expr pre-scanned-args
                             parse-parameters?)
  (if parse-parameters?
      (let* ((prms (parse-funcall-params ctx '())))
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


(define (parse-param-call ctx expr pre-scanned-args parse-parameters?)
  (if (apt-id? expr)
      (let* ((macro-id (qualified-id-for-lookup ctx expr))
             (macro    (lookup-macro ctx macro-id))
             (type     (lookup-macro-type ctx macro-id))
             (sym      (apt-id (car macro-id))))
        (if macro
            (begin
              (hea:assert type "Something wrong with the macro facility?")
              (parse-make-macro-call ctx sym
                                     pre-scanned-args macro type
                                     parse-parameters? 'local))
            (parse-function-call ctx sym pre-scanned-args
                                 parse-parameters?)))
      (parse-function-call ctx expr pre-scanned-args
                           parse-parameters?)))


(define (parse-slice ctx expr1)
  (let ((index (parse-expr ctx)))
    (if (apt-punct-eq? (current-token ctx) "]")
        (if index
            (begin
              (next-token ctx)
              (apt-funcall (apt-id "slice")
                           (append (list expr1 (apt-punct ","))
                                   (list index)) ))
            #f)
        (syntax-error "Expected ], got" (current-token ctx)))))


(define (parse-access ctx expr1)
  (cond
   ((apt-punct-eq? (current-token ctx) "(")
    (begin
      (next-token ctx)
      (parse-access ctx (parse-param-call ctx expr1 #f #t))))
   ((apt-punct-eq? (current-token ctx) "[")
    (begin
      (next-token ctx)
      (parse-access ctx (parse-slice ctx expr1))))

   ((apt-punct-eq? (current-token ctx) ".")
    (begin
      (next-token ctx)
      (if (apt-id? (current-token ctx))
          (let ((sym (current-token ctx)))
            (next-token ctx)
            (cond ((apt-punct-eq? (current-token ctx) "(")
                   (begin
                     (next-token ctx)
                     (parse-access ctx
                                   (parse-param-call ctx sym expr1 #t))))

                  ((or (apt-punct-eq? (current-token ctx) "[")
                       (apt-punct-eq? (current-token ctx) "."))
                   (parse-access ctx (parse-param-call ctx sym
                                                       (list expr1) #f)))
                  (else (parse-param-call ctx sym
                                          (list expr1) #f))))
          (syntax-error "Expected symbol, got" (current-token ctx)) )))
   (else
    expr1)))


(define (parse-const-container ctx del-token? left right)
  (let constcont-loop ((res '()))
    (if (del-token? (current-token ctx))
        (begin
          (next-token ctx)
          (apt-nested* left right res))
        (let ((expr (parse-expr ctx)))
          (if (apt-punct-eq? (current-token ctx) ",")
              (begin
                (next-token ctx)
                (constcont-loop (append res (list expr (apt-punct ",")))))
              (constcont-loop (append res (list expr)))) ))))


(define (parse-function ctx)
  (if (apt-punct-eq? (current-token ctx) "(")
      (begin
        (next-token ctx)
        (parse-func-def ctx #f '() 'local))
      (syntax-error "function: Expected (, got" (current-token ctx))))


;;----------------------------------------------------------------------
;; expressions
;;----------------------------------------------------------------------

(define (parse-atom-expr ctx)
  (cond
   ;;; literals
   ((or (apt-lit? (current-token ctx))
        (apt-seq? (current-token ctx))
        (apt-nested? (current-token ctx)))
    (let ((val (current-token ctx)))
      (next-token ctx)
      val))

   ;; groups
   ((apt-punct-eq? (current-token ctx) "(")
    (parse-access ctx (parse-group ctx)))

   ;; blocks
   ((apt-punct-eq? (current-token ctx) "{")
    (parse-access ctx (parse-block ctx)))

   ((apt-id? (current-token ctx))
    (let ((sym (apt-id-value (current-token ctx))))
      (cond
       ((equal? sym "if") (parse-if-expr ctx))
       ((equal? sym "let") (parse-def ctx 'local))
       ((equal? sym "on") (begin
                            (next-token ctx)
                            (parse-on ctx '("signal" "exit"))))
       (else (let ((id (current-token ctx)))
               (next-token ctx)
               (parse-access ctx id))) )))

   ((apt-punct-eq? (current-token ctx) "#(")
    (begin
      (next-token ctx)
      (parse-const-container ctx
                             (lambda (token) (apt-punct-eq? token ")"))
                             "#(" ")")))

   ((apt-punct-eq? (current-token ctx) "#[")
    (begin
      (next-token ctx)
      (parse-const-container ctx
                             (lambda (token) (apt-punct-eq? token "]"))
                             "#[" "]")))

   ((apt-punct-eq? (current-token ctx) "#function")
    (begin
      (next-token ctx)
      (parse-function ctx)))

   (else (syntax-error "Unexpected token (1)" (current-token ctx)))))


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


(define (parse-operator ctx)
  (let ((curtok (current-token ctx)))
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


(define (parse-expr ctx)
  (letrec ((expr-loop (lambda (expr1 op1)
                        (let ((expr2 #f))
                          (if (not op1)
                              expr1
                              (begin
                                (next-token ctx)
                                (set! expr2 (parse-atom-expr ctx))

                                (if expr2
                                    (let ((op2 (parse-operator ctx)))
                                      (if (not op2)
                                          (apt-operator expr1 op1 expr2)
                                          (if (and (not (operator-right? op1))
                                             (operator>? op1 op2) )
                                              (expr-loop (apt-operator expr1 op1
                                                                       expr2) op2)
                                              (apt-operator expr1 op1
                                                            (expr-loop expr2 op2)))))
                                    #f)
                                ))) )))
    (let ((expr1 (parse-atom-expr ctx)))
      (if expr1
          (let ((op1 (parse-operator ctx)) )
            (if op1
                (expr-loop expr1 op1)
                expr1))
          #f))))


;;----------------------------------------------------------------------
;; type
;;----------------------------------------------------------------------

(define (parse-type-params ctx del-token? params)
  (if (apt-id? (current-token ctx))
      (let ((type (parse-type ctx)))
        (cond ((apt-punct-eq? (current-token ctx) ",")
               (let ((comma (current-token ctx)))
                 (next-token ctx)
                 (parse-type-params ctx del-token?
                                    (append params
                                            (list type comma)))))
              ((del-token? (current-token ctx))
               (append params (list type)))

              (else (syntax-error "Unexpected token (8)" (current-token ctx))) ))
      params))


(define (parse-complex-type-del ctx type-list)
  (cond ((apt-punct-eq? (current-token ctx) ",")
         (begin
           (next-token ctx)
           (parse-complex-type ctx type-list)))
        ((apt-punct-eq? (current-token ctx) ")")
         (begin
           (next-token ctx)
           (apt-seq* type-list)))
        (else (syntax-error "complex type: expected , or ), got: "
                            (current-token ctx)))))


(define (parse-complex-type ctx type-list)
  (cond ((apt-id? (current-token ctx))
         (let ((ty (parse-simple-type ctx #t)))
           (if ty
               (parse-complex-type-del ctx (append type-list (list ty)))
               #f)))
        ((apt-punct-eq? (current-token ctx) "(")
         (let ((ty (parse-complex-type ctx '())))
           (if ty
               (parse-complex-type-del ctx (append type-list (list ty)))
               #f)))
        (else (syntax-error "complex type: expected ID or (, got: "
                            (current-token ctx)))))


(define (parse-array-type ctx base-type)
  (if (apt-punct-eq? (current-token ctx) "[")
      (let ((size-param #f))
        (next-token ctx)
        (if (not (apt-punct-eq? (current-token ctx) "]"))
            (set! size-param (parse-expr ctx)))
        (if (apt-punct-eq? (current-token ctx) "]")
            (begin
              (next-token ctx)
              (if size-param
                  (apt-seq base-type (apt-nested "[" "]" size-param))
                  (apt-seq base-type (apt-nested "[" "]"))))
            (syntax-error "Expected ], got" (current-token ctx))))
      base-type))


(define (parse-function-type ctx)
  (let ((params (parse-functions-params ctx '()))
        (rettype #f))
    (if (apt-punct-eq? (current-token ctx) ":")
        (begin
          (next-token ctx)
          (set! rettype (parse-type ctx))))
    (apt-seq* (list (apt-id "Function")
                    (apt-nested* "(" ")" params)
                    (apt-punct ":")
                    rettype))))

(define (parse-simple-type ctx expect-constraint?)
  (let* ((sym (current-token ctx)))
    (next-token ctx)
    (cond ((apt-punct-eq? (current-token ctx) "(")
           (begin
             (next-token ctx)
             (if (equal? (apt-id-value sym) "Function")
                 (parse-function-type ctx)
                 (let ((params (parse-type-params ctx
                                                  (lambda (token)
                                                    (apt-punct-eq? token ")"))
                                                  '())))
                   (if (apt-punct-eq? (current-token ctx) ")")
                       (begin
                         (next-token ctx)
                         (parse-array-type ctx
                                           (apt-seq sym (apt-nested* "(" ")"
                                                                     params))))
                       (syntax-error "Expected ), got" (current-token ctx))))) ))
          ((apt-punct-eq? (current-token ctx) ".")
           (begin
             (next-token ctx)
             (let st-loop ((res (list sym)))
               (cond ((apt-id? (current-token ctx))
                      (let ((sym2 (current-token ctx)))
                        (next-token ctx)
                        (if (apt-punct-eq? (current-token ctx) ".")
                            (begin
                              (next-token ctx)
                              (st-loop (append res (list (apt-punct ".") sym2))))
                            (apt-seq* (append res (list (apt-punct ".") sym2))))))
                     (else (apt-seq* res))))))
          ((and expect-constraint?
                (apt-punct-eq? (current-token ctx) "="))
           (begin
             (next-token ctx)
             (let ((constraint (parse-expr ctx)))
               (if constraint
                   (apt-seq* (list sym (apt-punct "=") constraint))
                   #f))))
          (else (parse-array-type ctx sym) ))))


(define (parse-type ctx)
  (cond
   ((apt-id? (current-token ctx)) (parse-simple-type ctx #f))
   ((apt-punct-eq? (current-token ctx) "(")
    (begin
      (next-token ctx)
      (parse-array-type ctx (parse-complex-type ctx '()))))
   (else (syntax-error "Expected type, got" (current-token ctx)))))


;;----------------------------------------------------------------------
;; def type
;;----------------------------------------------------------------------

(define (parse-type-slot-decl-params ctx params)
  (cond ((apt-id-keyarg? (current-token ctx))
         (let* ((key (current-token ctx))
                (val #f))
           (next-token ctx)
           (set! val (parse-expr ctx))
           (if val
               (if (apt-punct-eq? (current-token ctx) ",")
                   (let ((comma (current-token ctx)))
                     (next-token ctx)
                     (parse-type-slot-decl-params ctx
                                                  (append params
                                                          (list (apt-seq key val)
                                                                comma))))
                   (append params (list (apt-seq key val))) )
               #f)))
        ((apt-id? (current-token ctx))
         (let ((sym (current-token ctx)))
           (next-token ctx)
           (if (apt-punct-eq? (current-token ctx) ",")
               (let ((comma (current-token ctx)))
                 (next-token ctx)
                 (parse-type-slot-decl-params ctx
                                              (append params
                                                      (list sym comma))))
               (append params (list sym)))))
        (else params)))


(define (parse-type-slot-decl ctx)
  (if (apt-id? (current-token ctx))
      (let* ((sym (current-token ctx))
             (type #f)
             (init-value #f)
             (params #f))
        (next-token ctx)
        (if (apt-punct-eq? (current-token ctx) ":")
            (begin
              (next-token ctx)
              (set! type (parse-type ctx))))
        (if (apt-punct-eq? (current-token ctx) "=")
            (begin
              (next-token ctx)
              (set! init-value (parse-expr ctx))))
        (if (apt-punct-eq? (current-token ctx) ",")
            (begin
              (next-token ctx)
              (set! params (parse-type-slot-decl-params ctx '()))))

        (let ((res (list (apt-id "slot") sym)))
          (if type
              (set! res (append res (list (apt-punct ":") type))))
          (if init-value
              (set! res (append res (list (apt-punct "=") init-value))))
          (if params
              (set! res (append res (append (list (apt-punct ","))
                                            params))))
          (apt-seq* res)))
      (syntax-error "Expected symbol, got: " (current-token ctx))))


(define (parse-typedef-decls ctx)
  (let typedef-loop ((res '()))
    (cond ((apt-id? (current-token ctx))
           (let* ((sym (apt-id-value (current-token ctx)))
                  (expr (cond ((equal? sym "slot")
                               (begin
                                 (next-token ctx)
                                 (parse-type-slot-decl ctx)))
                              ((equal? sym "on")
                               (begin
                                 (next-token ctx)
                                 (parse-on ctx '("init" "delete"))))
                              (else (syntax-error "Unexpected symbol:" (current-token ctx))))))
             (if expr
                 (typedef-loop (append res (list expr)))
                 #f)))
          ((apt-punct-eq? (current-token ctx) "}") res)
          (else (syntax-error "Unexpect token:" (current-token ctx))))))


(define (parse-type-def ctx modifiers type)
  (if (apt-id? (current-token ctx))
      (let ((sym (apt-id* (apt-id-value (current-token ctx))
                          (current-namespace ctx)))
            (params #f)
            (derives-from #f)
            (decls #f))
        (next-token ctx)
        (if (apt-punct-eq? (current-token ctx) "(")
            (begin
              (next-token ctx)
              (set! params (parse-functions-params ctx '()))))
        (if (apt-punct-eq? (current-token ctx) ":")
            (begin
              (next-token ctx)
              (set! derives-from (parse-type ctx))))
        (case type
          ((class) (if (apt-punct-eq? (current-token ctx) "{")
                       (begin
                         (next-token ctx)
                         (set! decls (parse-typedef-decls ctx))
                         (if (apt-punct-eq? (current-token ctx) "}")
                             (begin
                               (next-token ctx)
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
  (let pattern-loop ((nl pattern)
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
                              (else (pattern-loop (cdr nl) 'scan-syms prc-count)))
                        'any))
            ((scan-syms) (cond ((apt-id? elt)
                                (pattern-loop (cdr nl) 'scan-syms prc-count))
                               ((and (apt-punct? elt)
                                     (equal? (apt-punct-value elt) "("))
                                (pattern-loop (cdr nl) 'scan-for-prc (+ prc-count 1)))
                               (else 'any)))
            ((scan-for-prc) (if (and (apt-punct? elt)
                                     (equal? (apt-punct-value elt) ")"))
                                (if (equal? prc-count 1)
                                    (pattern-loop (cdr nl) 'scan-for-expr 0)
                                    (pattern-loop (cdr nl) 'scan-for-prc (- prc-count 1)))
                                (pattern-loop (cdr nl) 'scan-for-prc prc-count)))
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
                (hea:display "Macro has inconsistent patterns: "
                             (car pnl) 'nl)
                'any)))) ))


(define (parse-macro-comp ctx)
  (if (apt-punct-eq? (current-token ctx) "{")
      (begin
        (next-token ctx)
        (let maccmp-loop ((res '())
                          (brace-count 1))
          (let ((token (current-token ctx)))
            (cond ((apt-punct-eq? (current-token ctx) "}")
                   (let ((count (- brace-count 1)))
                     (next-token ctx)
                     (if (<= count 0)
                         res
                         (maccmp-loop (append res (list token))
                                      count))))
                  ((apt-punct-eq? (current-token ctx) "{")
                   (let ((count (+ brace-count 1)))
                     (next-token ctx)
                     (maccmp-loop (append res (list token))
                                  count)))
                  (else (begin
                          (next-token ctx)
                          (maccmp-loop (append res (list token))
                                       brace-count)) )))))
      (syntax-error "Expected {, got" (current-token ctx))))


(define (parse-macro-basic-pattern ctx pattern-name)
  (let ((pattern (parse-macro-comp ctx))
        (replc #f))
    (if pattern
        (if (apt-id-eq? (current-token ctx) "->")
            (begin
              (next-token ctx)
              (set! replc (parse-macro-comp ctx))
              (list 'prod
                    ':name pattern-name
                    ':pattern pattern ':replc replc))
            (syntax-error "Expected ->, got" (current-token ctx)))
        #f)))


(define (parse-macro-patterns ctx)
  (let mp-loop ((res '())
             (last-pattern-name #f))
    (cond ((apt-punct-eq? (current-token ctx) "}")  res)
          ((apt-punct-eq? (current-token ctx) "{")
           (let ((expr (parse-macro-basic-pattern ctx
                                                  last-pattern-name)))
             (if expr
                 (mp-loop (append res (list expr))
                       last-pattern-name)
                 #f)))
          ((apt-id-keyarg? (current-token ctx))
           (let ((sym (current-token ctx)))
             (next-token ctx)
             (mp-loop res (apt-id-keyarg-value sym))))
          (else (syntax-error "Unexpected token (9)" (current-token ctx))))))


(define (parse-macro-def ctx scope modifiers)
  (hea:assert (eq? scope 'global) "TODO.  Local macro def not supported yet")
  (if (apt-id? (current-token ctx))
      (let ((sym (apt-id-value (current-token ctx))))
        (next-token ctx)
        (if (apt-punct-eq? (current-token ctx) "{")
            (begin
              (next-token ctx)
              (let* ((patterns (parse-macro-patterns ctx))
                     (macro-type (macro-determine-type patterns)))
                (if (apt-punct-eq? (current-token ctx) "}")
                    (begin
                      (next-token ctx)
                      (if patterns
                          (begin
                            (register-macro ctx
                                            macro-type sym (current-namespace ctx)
                                            (vector ':macro
                                                    patterns
                                                    (macro-compile (list 'macro
                                                                         ':patterns patterns))))
                            'ignore)
                          #f))
                    (syntax-error "Expected }, got" (current-token ctx))) ))
            (syntax-error "Expected {, got" (current-token ctx))))
      (syntax-error "Expected symbol, got" (current-token ctx))))


(define (parse-alias-def ctx scope modifiers)
  (if (apt-id? (current-token ctx))
      (let* ((sym (if (eq? scope 'global)
                      (apt-id* (apt-id-value (current-token ctx))
                               (current-namespace ctx))
                      (current-token ctx))))
        (next-token ctx)
        (if (apt-punct-eq? (current-token ctx) "=")
            (begin
              (next-token ctx)
              (let ((type (parse-type ctx)))
                (apt-seq (apt-map-scope scope)
                         (apt-id "alias") sym (apt-punct "=") type)))
            (syntax-error "Expected =, got" (current-token ctx))))
      (syntax-error "Expected symbol, got" (current-token ctx))))


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


(define (parse-functions-params ctx param-list)
  (cond
   ((apt-id? (current-token ctx))
    (let* ((sym #f)
           (type #f)
           (spec? #f)
           (keyarg #f)
           (init-value #f))
      (if (apt-id-keyarg? (current-token ctx))
          (begin
            (set! keyarg (current-token ctx))
            (next-token ctx)))
      (if (apt-id? (current-token ctx))
          (begin
            (set! sym (current-token ctx))
            (next-token ctx)

            (cond
             ((apt-id-eq? (current-token ctx) "...")
              (begin
                (next-token ctx)
                (if (apt-punct-eq? (current-token ctx) ")")
                    (begin
                      (next-token ctx)
                      (append param-list (list (apt-param #f sym 'rest type init-value))))
                    (syntax-error "Rest argument must be last in parameter list"
                                  (current-token ctx)))))

             ((apt-punct-eq? (current-token ctx) ":")
              (begin
                (next-token ctx)

                (if (apt-punct-eq? (current-token ctx) "@")
                    (begin
                      (next-token ctx)
                      (set! spec? 'spec)))

                (set! type (parse-type ctx))

                (if (apt-punct-eq? (current-token ctx) "=")
                    (begin
                      (next-token ctx)
                      (if (eq? spec? 'spec)
                          (syntax-error "Keyed parameters can not be specialized"
                                        (current-token ctx)))
                      (set! spec? 'key)
                      (set! init-value (parse-expr ctx))))

                (cond
                 ((apt-punct-eq? (current-token ctx) ",")
                  (begin
                    (next-token ctx)
                    (parse-functions-params ctx
                                            (append param-list
                                                    (list (apt-param keyarg sym spec? type
                                                                     init-value))))))

                 ((apt-punct-eq? (current-token ctx) ")")
                  (begin
                    (next-token ctx)
                    (append param-list (list (apt-param keyarg sym spec? type init-value)))))

                 (else (syntax-error "Unexpected token (2)" (current-token ctx))) )))

             ((apt-punct-eq? (current-token ctx) "=")
              (begin
                (next-token ctx)
                (set! spec? 'key)
                (set! init-value (parse-expr ctx))

                (cond
                 ((apt-punct-eq? (current-token ctx) ")")
                  (begin
                    (next-token ctx)
                    (append param-list (list (apt-param keyarg sym spec? type init-value)))))
                 ((apt-punct-eq? (current-token ctx) ",")
                  (begin
                    (next-token ctx)
                    (parse-functions-params ctx
                                            (append param-list
                                                    (list (apt-param keyarg sym spec? type
                                                                     init-value))))))
                 (else (syntax-error "Unexpected token (3)" (current-token ctx))) )))

             ((apt-punct-eq? (current-token ctx) ",")
              (begin
                (next-token ctx)
                (parse-functions-params ctx
                                        (append param-list
                                                (list (apt-param #f sym spec? type
                                                                 init-value))))))

             ((apt-punct-eq? (current-token ctx) ")")
              (begin
                (next-token ctx)
                (append param-list (list (apt-param #f sym spec? type init-value)))))

             (else (syntax-error "Unexpected token (4)" (current-token ctx))) ))

          (syntax-error "expected id" (current-token ctx)))))

   ((apt-punct-eq? (current-token ctx) ")")
    (begin
      (next-token ctx)
      param-list))

   (else (syntax-error "Unexpected token (5)" (current-token ctx))) ))


(define (apt-function params type body)
  (apt-seq (apt-punct "#function")
           (apt-nested* "(" ")" params)
           (if type (apt-punct ":") #f)
           (if type type #f)
           (if (equal? (length body) 1)
               (car body)
               (apt-nested* "{" "}" body))))


(define (apt-funcdef ctx scope modifiers sym params type body)
  (let ((nl (list (apt-map-scope scope))))
    (if (member "meth" modifiers)
        (set! nl (append nl (list (apt-id "meth")))))
    (set! nl (append nl (list (if (eq? scope 'global)
                                  (apt-id* (apt-id-value sym)
                                           (current-namespace ctx))
                                  sym)
                              (apt-nested* "(" ")" params))))
    (if type
        (set! nl (append nl (list (apt-punct ":") type))))
    (set! nl (append nl (list (if (equal? (length body) 1)
                                  (car body)
                                  (apt-nested* "{" "}" body)))))
    (apt-seq* nl)))


(define (parse-func-def ctx sym modifiers scope)
  (let* ((params (parse-functions-params ctx '()))
         (type (if (apt-punct-eq? (current-token ctx) ":")
                   (begin
                     (next-token ctx)
                     (parse-type ctx))
                   #f))
         (body (if (eq? scope 'global)
                   (parse-exprlist-until-def ctx '())
                   (list (parse-expr ctx)))) )
    (if sym
        (apt-funcdef ctx scope modifiers sym params type body)
        (apt-function params type body)) ))


(define (apt-vardef ctx scope name type modifiers init)
  (let ((res '()))
    (set! res (list (apt-map-scope scope)))
    (set! res (append res (map (lambda (m)
                                 (apt-id m))
                               modifiers)))
    (set! res (append res (list (if (eq? scope 'global)
                                    (apt-id* (apt-id-value name)
                                             (current-namespace ctx))
                                    name))))
    (if type
        (set! res (append res (list (apt-punct ":") type))))
    (if init
        (set! res (append res (list (apt-punct "=") init))))

    (apt-seq* res)))


(define (parse-func-or-var-def ctx scope modifiers)
  (let* ((sym (current-token ctx))
         (macro-id (qualified-id-for-lookup ctx sym))
         (macro    (lookup-macro ctx macro-id))
         (type     (lookup-macro-type ctx macro-id))
         (name     (apt-id (car macro-id))))
    (if macro
        (begin
          (let ((expr (parse-make-macro-call ctx name '()
                                             macro type #t scope)))
            (if expr
                expr
                (syntax-error "Incomplete macro appliance: " macro-id))))
        (begin
          (next-token ctx)
          (cond
           ((apt-punct-eq? (current-token ctx) "(")
            (begin
              (next-token ctx)
              (parse-func-def ctx sym modifiers scope)))

           ((apt-punct-eq? (current-token ctx) ":")
            (begin
              (next-token ctx)
              (let ((type (parse-type ctx)))
                (if (apt-punct-eq? (current-token ctx) "=")
                    (begin
                      (next-token ctx)
                      (let ((init-value (parse-expr ctx)))
                        (apt-vardef ctx scope sym type modifiers init-value)))
                    (apt-vardef ctx scope sym type modifiers #f)))
              ))

           ((apt-punct-eq? (current-token ctx) "=")
            (begin
              (next-token ctx)
              (let ((init-value (parse-expr ctx)))
                (apt-vardef ctx scope sym #f modifiers init-value))))

           (else (apt-vardef ctx scope sym #f modifiers #f))
           )))))


(define (parse-def ctx scope)
  (next-token ctx)
  (let* ((modifiers (parse-modifiers ctx '("meth" "fluid" "const")))
         (curtok (current-token ctx)))
    (if (apt-id? curtok)
        (cond
         ((apt-id-eq? curtok "type")
          (if (eq? scope 'global)
              (begin
                (next-token ctx)
                (parse-type-def ctx modifiers 'type))
              (syntax-error "Local type defs are not supported" curtok)))
         ((apt-id-eq? curtok "class")
          (if (eq? scope 'global)
              (begin
                (next-token ctx)
                (parse-type-def ctx modifiers 'class))
              (syntax-error "Local class defs are not supported" curtok)))
         ((apt-id-eq? curtok "alias")
          (begin
            (next-token ctx)
            (parse-alias-def ctx scope modifiers)))
         ((apt-id-eq? curtok "macro")
          (begin
            (next-token ctx)
            (parse-macro-def ctx scope modifiers)))
         ((apt-id? curtok)
          (parse-func-or-var-def ctx scope modifiers))
         (else (syntax-error "Expected symbol, got" curtok)))
        (syntax-error "Expected symbol, got" curtok))
     ))


(define (parse-namespace ctx)
  (next-token ctx)
  (if (apt-id? (current-token ctx))
      (let ((nsname (current-token ctx)))
        (next-token ctx)
        (if (apt-punct-eq? (current-token ctx) "(")
            (begin
              (next-token ctx)
              (let ((str (parse-expr ctx)))
                (if (apt-punct-eq? (current-token ctx) ")")
                    (let ((ns (apt-lit-value str)))
                      (next-token ctx)
                      (register-namespace-mapping ctx (apt-id-value nsname) ns)
                      (set-current-namespace! ctx ns)
                      (apt-seq (apt-id "namespace")
                               nsname
                               (apt-nested "(" ")" str)))
                    (syntax-error "Expected ), got" (current-token ctx)))))
            (let ((str (lookup-namespace ctx (apt-id-value nsname))))
              (if str
                  (set-current-namespace! ctx str)
                  (syntax-error "Undefined namespace abbreviation" nsname))
              (apt-seq (apt-id "namespace")
                       nsname))))
      (syntax-error "Expected SYMBOL, got " (current-token ctx))))


(define (parse-import ctx)
  (next-token ctx)
  (if (apt-punct-eq? (current-token ctx) "(")
      (begin
        (next-token ctx)
        (let ((prms (parse-funcall-params ctx '())))
          (apt-seq (apt-id "import") (apt-nested* "(" ")" prms))))
      (syntax-error "import: Expected (, got" (current-token ctx))))


(define (parse-top-expr ctx)
  (if (apt-id? (current-token ctx))
      (let ((sym (apt-id-value (current-token ctx))))
        (cond
         ((equal? sym "def") (parse-def ctx 'global))
         ((equal? sym "namespace") (parse-namespace ctx))
         ((equal? sym "import") (parse-import ctx))
         (else (syntax-error "Unexpected symbol"
                             (current-token ctx)))))
      (syntax-error "Unexpected token (6)" (current-token ctx))))


(define (parse-next-top ctx)
  (let nt-loop ((apt (list)))
    (if (eq? (current-token ctx) 'EOF)
        apt
        (let ((expr (parse-top-expr ctx)))
          (cond ((and (list? expr)
                      (not (apt? expr)))
                 (let nt2-loop ((nl expr))
                   (hea:display "NT2 " nl 'nl)
                   (if (null? nl)
                       (nt-loop apt)
                       (cond ((eq? (car nl) 'ignore) (nt2-loop (cdr nl)))
                             ((not expr) (nt2-loop (cdr nl)))
                             (else (begin
                                     (set! apt (append apt (list (car nl))))
                                     (nt2-loop (cdr nl))))))))
                ((eq? expr 'ignore) (nt-loop apt))
                ((not expr) #f)
                (else (nt-loop (append apt (list expr)))))))))



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
