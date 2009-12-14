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

(define *macro-registry* '())

(define (macro-register type name macro)
  (let* ((mp (assoc name *macro-registry*)))
    (if mp
        (set-cdr! mp (cons type macro))
        (set! *macro-registry*
              (append *macro-registry*
                      (list (cons name (cons type macro))))))))


(define (macro-lookup name)
  (let ((mp (assoc name *macro-registry*)))
    (if mp
        (cddr mp)
        #f)))


(define (macro-type name)
  (let ((mp (assoc name *macro-registry*)))
    (if mp
        (cadr mp)
        #f)))


;;----------------------------------------------------------------------

(define (apt-alloc)
  (list))


(define (apt-append-list dst src)
  (append dst (arc:reduce (lambda (o nl)
                            (if o
                                (cons o nl)
                                nl))
                          '() src)))


(define (apt-seq . contained)
  (apt-append-list (list 'seq) contained))


(define (apt-seq* contained)
  (apt-append-list (list 'seq) contained))


(define (apt-seq? expr)
  (and (list? expr)
       (>= (length expr) 2)
       (eq? (car expr) 'seq)))


(define (apt-seq-body expr)
  (cdr expr))


(define (apt-lit value)
  (list 'lit value))


(define (apt-lit? expr)
  (and (list? expr)
       (= (length expr) 2)
       (eq? (car expr) 'lit)))


(define (apt-lit-value expr)
  (cadr expr))


(define (apt-lit-eq? lit val)
  (and (apt-lit? lit)
       (equal? (apt-lit-value lit) val)))


(define (apt-lit-keyword? expr)
  (and (apt-lit? expr)
       (symbol? (apt-lit-value expr))))


(define (apt-lit-string? expr)
  (and (apt-lit? expr)
       (string? (apt-lit-value expr))))


(define (apt-lit-int? expr)
  (and (apt-lit? expr)
       (integer? (apt-lit-value expr))))


(define (apt-lit-char? expr)
  (and (apt-lit? expr)
       (char? (apt-lit-value expr))))


(define (apt-lit-bool? expr)
  (and (apt-lit? expr)
       (boolean? (apt-lit-value expr))))


(define (apt-id id)
  (list 'id id))


(define (apt-id? id)
  (and (list? id)
       (= (length id) 2)
       (eq? (car id) 'id)))


(define (apt-id-value id)
  (cadr id))


(define (apt-id-eq? id val)
  (and (apt-id? id)
       (equal? (apt-id-value id) val)))


(define (apt-id-keyarg? id)
  (and (apt-id? id)
       (let ((val (apt-id-value id)))
         (equal? (string-ref val (- (string-length val) 1)) #\:))))


(define (apt-id-keyarg-value id)
  (let ((val (apt-id-value id)))
    (substring val 0 (- (string-length val) 1))))


(define (apt-punct val)
  (list 'punct val))


(define (apt-punct? expr)
  (and (list? expr)
       (= (length expr) 2)
       (eq? (car expr) 'punct)))


(define (apt-punct-value expr)
  (cadr expr))


(define (apt-punct-eq? apt str)
  (and (apt-punct? apt)
       (equal? (apt-punct-value apt) str)))


(define (apt-nested left right . contained)
  (apt-append-list (list 'nested left right) contained))


(define (apt-nested* left right contained)
  (apt-append-list (list 'nested left right) contained))


(define (apt-nested? expr)
  (and (list? expr)
       (>= (length expr) 3)
       (eq? (car expr) 'nested)))


(define (apt-nested-body expr)
  (cdddr expr))


(define (apt-nested-left expr)
  (cadr expr))


(define (apt-nested-right expr)
  (caddr expr))


(define (apt-macro-call macro left right contained)
  (apt-append-list (list 'macro-call macro)
                   (list (apt-nested* left right contained))))


(define (apt-macro-stmt macro left right contained body)
  (apt-append-list (list 'macro-stmt macro)
                   (append (list (apt-nested* left right contained))
                           body)))


(define (apt-macro-param macro . contained)
  (apt-append-list (list 'macro-param macro) contained))


(define (apt-macro-param? expr)
  (and (list? expr)
       (>= (length expr) 2)
       (eq? (car expr) 'macro-param)))


(define (apt-macro-param-type expr)
  (let* ((name (cadr expr))
         (splitup (string-split name #\:)))
    (if splitup
        (cond ((equal? (cadr splitup) "expr") 'expr)
              ((equal? (cadr splitup) "name") 'name)
              ((equal? (cadr splitup) "body") 'body)
              (else 'unknown))
        'unknown)))


(define (apt-macro-param-name expr)
  (let* ((full-name (cadr expr))
         (splitup (string-split full-name #\:)))
    (if splitup
        (car splitup)
        full-name)))


;; higher order functions
(define (apt-funcall expr1 args)
  (apt-seq expr1 (apt-nested* "(" ")" args)))


;;;(define (apt-symbol value)
;;;  (let* ((ns-pos (if (char=? (string-ref value 0) #\<)
;;;                     (string-find value #\> 1)
;;;                     #f))
;;;         (ns (if ns-pos
;;;                 (substring value 1 ns-pos)
;;;                 #f))
;;;         (sym (if ns-pos
;;;                  (substring value (+ ns-pos 1) (string-length value))
;;;                  value)))
;;;    (if ns
;;;        (vector ':apt 'symbol ':ns ns ':sym sym)
;;;        (vector ':apt 'symbol ':sym sym))))


(define (apt-class sym params isatype decls)
  (let* ((res #f))
    (set! res (list (apt-id "def") sym
                    (apt-nested* "(" ")" params)))
    (if isatype
        (set! res (append res (list (apt-punct ":")
                                    isatype))))
    (set! res (append res (list (apt-nested* "{" "}" decls))))

    (apt-seq* res)))


;;----------------------------------------------------------------------

(define current-token #f)


(define (next-token-port port)
  (let* ((token (tokenize-next-token port))
         (apt (cond ((eq? token 'PRO)          (apt-punct "("))
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
                                                  (apt-lit (string->symbol x))
                                                  (apt-lit x))))
                    ((symbol-token? token)     (apt-id (token-value token)))
                    ((macroparam-token? token) (apt-macro-param (token-value token)))

                    ((or (integer-token? token)
                         (char-token? token)
                         (string-token? token)
                         (bool-token? token))  (apt-lit (token-value token)))

                    ((eq? token 'FUNCTION)     (apt-punct "#function"))
                    ((nil-token? token)        (apt-lit '()))
                    ((eof-token? token)        (apt-lit 'EOF-TOKEN))

                    (else token))) )
    apt))


(define (make-tokenport tokenlist)
  (vector ':tokenport tokenlist))


(define (tokenport? port)
  (and (vector? port)
       (eq? (vector-ref port 0) ':tokenport)))


(define (next-token-tokenport port)
  (if (not (null? (vector-ref port 1)))
      (let ((token (car (vector-ref port 1))))
        (vector-set! port 1 (cdr (vector-ref port 1)))
        token)
      'EOF))


(define *unread-stack* '())


(define (next-token port)
  (let ((token (if (not (null? *unread-stack*))
                   (let ((val (car *unread-stack*)))
                     (set! *unread-stack* (cdr *unread-stack*))
                     val)
                   (cond ((input-port? port) (next-token-port port))
                         ((tokenport? port)  (next-token-tokenport port))
                         (else #f)) )))
    (set! current-token token)
    current-token))


(define (unread-token port token)
  (set! *unread-stack* (cons token *unread-stack*)))


(define (syntax-error msg token)
  (arc:display "Syntax error:" (number->string line-count) ": "
               msg ": " token 'nl)
  #f)


(define (error-expected-token where token)
  (arc:display "Syntax error: " (number->string line-count)
               ": " where ": Expected token " token ", got: " current-token 'nl)
  #f)


;;----------------------------------------------------------------------

(define (parse-exprlist-until-def port exprlist)
  (cond
   ((eq? current-token 'EOF) exprlist)

   ((apt-id-eq? current-token "def") exprlist)

   ((apt-punct-eq? current-token "}") exprlist)

   (else (let ((expr (parse-expr port)))
           (if expr
               (parse-exprlist-until-def port (append exprlist
                                                      (list expr)))
               #f))) ))


(define (parse-exprlist-until-brcc port exprlist)
  (cond
   ((eq? current-token 'EOF)
    exprlist)

   ((apt-id-eq? current-token "def")
    (let ((expr (parse-def port 'global)))
      (if expr
          (parse-exprlist-until-brcc port (append exprlist
                                                  (list expr)))
          #f)))

   ((apt-punct-eq? current-token "}") exprlist)

   (else (let ((expr (parse-expr port)))
           (if expr
               (parse-exprlist-until-brcc port (append exprlist
                                                       (list expr)))
               #f))) ))


(define (parse-exprlist port exprlist)
  (cond
   ((eq? current-token 'EOF) exprlist)
   ((apt-punct-eq? current-token "}") exprlist)
   ((apt-punct-eq? current-token ")") exprlist)

   (else (let ((expr (parse-expr port)))
           (if expr
               (begin
                 (parse-exprlist port (append exprlist (list expr))))
               exprlist))) ))


(define (parse-modifiers port possible-mods)
  (let loop ((res '()))
    (if (and (apt-id? current-token)
             (member (apt-id-value current-token) possible-mods))
        (let* ((sym (apt-id-value current-token)))
          (next-token port)
          (loop (append res (list sym))))
        res)))


(define (parse-group port)
  (next-token port)
  (let ((retv (parse-expr port)))
    (if (apt-punct-eq? current-token ")")
        (begin
          (next-token port)
          retv)
        (syntax-error "Expected ), got" current-token))))


(define (parse-block port)
  (next-token port)
  (let ((retv (parse-exprlist-until-brcc port '())))
    (if (apt-punct-eq? current-token "}")
        (begin
          (next-token port)
          (apt-nested* "{" "}" retv))
        (syntax-error "Expected }, got" current-token))))


(define (parse-if-expr port)
  (let ((test-expr #f)
        (true-expr #f))
    (next-token port)

    (if (apt-punct-eq? current-token "(")
        (begin
          (next-token port)
          (set! test-expr (parse-expr port))
          (if (apt-punct-eq? current-token ")")
              (begin
                (next-token port)
                (set! true-expr (parse-expr port))

                (if (apt-id-eq? current-token "else")
                    (begin
                      (next-token port)
                      (apt-seq (apt-id "if")
                               (apt-nested "(" ")" test-expr)
                               true-expr
                               (apt-id "else")
                               (parse-expr port)))
                    (apt-seq (apt-id "if")
                             (apt-nested "(" ")" test-expr)
                             true-expr)))
              (syntax-error "if: Expected ), got " current-token)))
        (syntax-error "if: Expected (, got " current-token))))


(define (parse-on port possible-on-keys)
  (if (apt-id? current-token)
      (let ((sym current-token))
        (let* ((macro-id (apt-id-value sym))
               (macro    (macro-lookup macro-id))
               (type     (macro-type macro-id)))
          (if macro
              (parse-make-macro-call port sym '() macro type #t 'local)
              (if (member (apt-id-value sym) possible-on-keys)
                  (begin
                    (next-token port)
                    (if (apt-punct-eq? current-token "(")
                        (begin
                          (next-token port)
                          (let* ((params (parse-functions-params port '()))
                                 (expr (parse-expr port)))
                            (apt-seq (apt-id "on")
                                     sym
                                     (apt-nested* "(" ")" params)
                                     expr)))
                        (error-expected-token 'on "(")))
                  (syntax-error "Unexpected 'on' expr:" (apt-id-value sym))))))
      (syntax-error "Unexpected 'on' expr:" current-token)))


(define (parse-funcall-params-del port params)
  (cond
   ((apt-punct-eq? current-token ",")
    (let ((comma current-token))
      (next-token port)
      (parse-funcall-params port (append params (list comma)))))

   ((apt-punct-eq? current-token ")")
    (begin
      (next-token port)
      params))

   (else (syntax-error "Expect , or ), got" current-token))))


(define (parse-funcall-params port params)
  (cond
   ((eq? current-token 'EOF)
    (syntax-error "Unterminated argument list" current-token))

   ((apt-id-keyarg? current-token) (let* ((key current-token)
                                          (val #f))
                                     (next-token port)
                                     (set! val (parse-expr port))
                                     (if val
                                         (parse-funcall-params-del
                                          port (append params
                                                       (list (apt-seq key val))))
                                         #f)))

   ((apt-punct-eq? current-token ")")
    (begin
      (next-token port)
      params))

   (else (let ((expr (parse-expr port)))
           (if expr
               (parse-funcall-params-del port (append params
                                                      (list expr)))
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


(define (match-syntax port syntax-table)
;;;  (arc:display "x1" 'nl)
  (let check-next ((node (cadar (vector-ref syntax-table 1)))
                   (token current-token)
                   (bindings '()))
;;;    (arc:display "x2" node " --- " current-token 'nl)
    (let ((follow-set (st-find-node node current-token)))
;;;      (arc:display "x3 follow-set: " follow-set 'nl)
      (cond (follow-set
             (begin
;;;               (arc:display "x4" 'nl)
               (check-next follow-set (next-token port) bindings)))

            ((st-has-eof-set? node)
             (begin
;;;               (arc:display "DONE " node ", " bindings 'nl)
               (let ((replcment (replace-match-bindings (st-replacement node)
                                                        bindings)))
                 replcment)))

            (else
             (begin
               (set! follow-set (st-find-macro-param node))
;;;               (arc:display "x4a follow-set: " follow-set 'nl)
               (if follow-set
                   (begin
;;;                     (arc:display "x5 " node 'nl)
                     (let* ((macro-param (st-extract-macro-param node))
                            (macro-prm-type (apt-macro-param-type macro-param)))
;;;                       (arc:display "x6: " macro-param " --- " macro-prm-type 'nl)
                       (cond ((eq? macro-prm-type 'expr)
                              (begin
                                (let* ((expr (parse-expr port))
                                       (param-name (apt-macro-param-name macro-param))
                                       (new-bindings (append
                                                      bindings
                                                      (list (cons param-name (list expr)))) ))
;;;                                  (arc:display "x9 " expr 'nl)
                                  (check-next follow-set current-token new-bindings))))

                             ((eq? macro-prm-type 'name)
                              (if (apt-id? current-token)
                                  (begin
                                    (let* ((macro-name (apt-macro-param-name macro-param))
                                           (new-bindings (append
                                                          bindings
                                                          (list (cons macro-name
                                                                      (list current-token)))) ))
                                      (check-next follow-set (next-token port) new-bindings)))
                                  (syntax-error "Expected an symbol got a " current-token)))

                             (else (syntax-error "Expected ?*:expr or ?*:name")))))
                   #f)
               ) ))
      )))


(define (parse-do-match-syntax-func port expr args syntax-table parse-parameters?)
  (let ((old-current-token current-token))
    (if parse-parameters?
        (begin
          (if args
              (let ((next-tk (next-token port)))
                (unread-token port next-tk)
                (if (not (apt-punct-eq? next-tk ")"))
                    (unread-token port (apt-punct ",")))
                (unread-token port args)))
          (unread-token port old-current-token))
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
    (set! current-token expr)
    (match-syntax port syntax-table)) )


(define (parse-do-match-syntax-def port syntax-table scope)
  (unread-token port current-token)
  (set! current-token (cond ((eq? scope 'local) (apt-id "let"))
                            ((eq? scope 'global) (apt-id "def"))
                            (else (apt-id "unknown"))))
  (match-syntax port syntax-table))


(define (parse-do-match-syntax-on port syntax-table scope)
  (unread-token port current-token)
  (set! current-token (apt-id "on"))
  (match-syntax port syntax-table))


(define (parse-make-macro-call port expr args macro type parse-parameters?
                               scope)
  (let* ((syntax-table (vector-ref macro 2))
         (filtered (cond ((or (eq? type 'func) 
                              (eq? type 'stmt))
                          (parse-do-match-syntax-func port
                                                      expr args
                                                      syntax-table
                                                      parse-parameters?))
                         ((eq? type 'def)
                          (parse-do-match-syntax-def port syntax-table scope))
                         ((eq? type 'on)
                          (parse-do-match-syntax-on port syntax-table scope))
                         (else #f))) )
    (if (and (list? filtered)
             (> (length filtered) 0))
        (let ((follows filtered)
              (last-current-token current-token)
              (retval #f))
          (set! current-token (car follows))
          (set! retval (apt-seq* (parse-exprlist (make-tokenport (cdr follows))
                                                 '())))
          (set! current-token last-current-token)
          retval)
        #f)))


(define (parse-function-call port expr pre-scanned-args parse-parameters?)
  (if parse-parameters?
      (let* ((prms (parse-funcall-params port '())))
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


(define (parse-param-call port expr pre-scanned-args parse-parameters?)
  (if (apt-id? expr)
      (let* ((macro-id (apt-id-value expr))
             (macro    (macro-lookup macro-id))
             (type     (macro-type macro-id)))
        (if macro
            (parse-make-macro-call port expr pre-scanned-args macro type
                                   parse-parameters? 'local)
            (parse-function-call port expr pre-scanned-args
                                 parse-parameters?)))
      (parse-function-call port expr pre-scanned-args
                           parse-parameters?)))


(define (parse-slice port expr1)
  (let ((index (parse-expr port)))
    (if (apt-punct-eq? current-token "]")
        (if index
            (begin
              (next-token port)
              (apt-funcall (apt-id "slice")
                           (append (list expr1 (apt-punct ","))
                                   (list index)) ))
            #f)
        (syntax-error "Expected ], got" current-token))))


(define (parse-access port expr1)
  (cond
   ((apt-punct-eq? current-token "(")
    (begin
      (next-token port)
      (parse-access port (parse-param-call port expr1 #f #t))))
   ((apt-punct-eq? current-token "[")
    (begin
      (next-token port)
      (parse-access port (parse-slice port expr1))))

   ((apt-punct-eq? current-token ".")
    (begin
      (next-token port)
      (if (apt-id? current-token)
          (let ((sym current-token))
            (next-token port)
            (cond ((apt-punct-eq? current-token "(")
                   (begin
                     (next-token port)
                     (parse-access port (parse-param-call port sym expr1 #t))))

                  ((or (apt-punct-eq? current-token "[")
                       (apt-punct-eq? current-token "."))
                   (parse-access port (parse-param-call port sym
                                                        (list expr1) #f)))
                  (else (parse-param-call port sym
                                          (list expr1) #f))))
          (syntax-error "Expected symbol, got" current-token) )))
   (else
    expr1)))


(define (parse-const-container port del-token? type)
  (let loop ((res '()))
    (if (del-token? current-token)
        (begin
          (next-token port)
          (list type res))
        (let ((expr (parse-expr port)))
          (if (apt-punct-eq? current-token ",")
              (next-token port))
          (loop (append res (list expr)))) )))


(define (parse-function port)
  (if (apt-punct-eq? current-token "(")
      (begin
        (next-token port)
        (parse-func-def port #f '() 'local))
      (syntax-error "function: Expected (, got" current-token)))


;;----------------------------------------------------------------------
;; expressions
;;----------------------------------------------------------------------

(define (parse-atom-expr port)
  (cond
   ;;; literals
   ((or (apt-lit? current-token)
        (apt-seq? current-token)
        (apt-nested? current-token))
    (let ((val current-token))
      (next-token port)
      val))

   ;; groups
   ((apt-punct-eq? current-token "(")
    (parse-access port (parse-group port)))

   ;; blocks
   ((apt-punct-eq? current-token "{")
    (parse-access port (parse-block port)))

   ((apt-id? current-token)
    (let ((sym (apt-id-value current-token)))
      (cond
       ((equal? sym "if") (parse-if-expr port))
       ((equal? sym "let") (parse-def port 'local))
       ((equal? sym "on") (begin
                            (next-token port)
                            (parse-on port '("signal" "exit"))))

       (else (begin
               (next-token port)
               (parse-access port (apt-id sym)))) )
      ))

   ((apt-punct-eq? current-token "#(")
    (begin
      (next-token port)
      (parse-const-container port
                             (lambda (token) (apt-punct-eq? token ")"))
                             'vector)))

   ((apt-punct-eq? current-token "#[")
    (begin
      (next-token port)
      (parse-const-container port
                             (lambda (token) (apt-punct-eq? token "]"))
                             'array)))

   ((apt-punct-eq? current-token "#function")
    (begin
      (next-token port)
      (parse-function port)))

   (else (syntax-error "Unexpected token (1)" current-token))))


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


(define (parse-operator port)
  (cond ((apt-punct-eq? current-token "=")    'assign)
        ((apt-id-eq? current-token "+")       'add)
        ((apt-id-eq? current-token "-")       'min)
        ((apt-id-eq? current-token "*")       'mul)
        ((apt-id-eq? current-token "/")       'div)
        ((apt-id-eq? current-token "mod")     'mod)
        ((apt-id-eq? current-token "%")       'fold)
        ((apt-id-eq? current-token "**")      'exp)
        ((apt-id-eq? current-token "==")      'equal)
        ((apt-id-eq? current-token "<>")      'unequal)
        ((apt-id-eq? current-token "<")       'less)
        ((apt-id-eq? current-token "<=")      'lesseq)
        ((apt-id-eq? current-token ">")       'greater)
        ((apt-id-eq? current-token ">=")      'greatereq)
        ((apt-id-eq? current-token "<=>")     'compare)
        ((apt-id-eq? current-token "&")       'bitand)
        ((apt-id-eq? current-token "|")       'bitor)
        ((apt-id-eq? current-token "^")       'bitxor)
        ((apt-id-eq? current-token "and")     'and)
        ((apt-id-eq? current-token "or")      'or)
        ((apt-id-eq? current-token "->")      'mapto)
        ((apt-id-eq? current-token "..")      'range)
        ((apt-id-eq? current-token "by")      'by)
        ((apt-id-eq? current-token "...")     'ellipsis)

        ((apt-id-eq? current-token "<<")      'shiftleft)
        ((apt-id-eq? current-token ">>")      'shiftright)
        ((apt-id-eq? current-token "in")      'in)
        ((apt-punct-eq? current-token ".")    'dot)

        ((eq? current-token 'EOF)      #f)
        (else #f)))


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


(define (parse-expr port)
  (letrec ((loop (lambda (expr1 op1)
                   (let ((expr2 #f))
                     (if (not op1)
                         expr1
                         (begin
                           (next-token port)
                           (set! expr2 (parse-atom-expr port))

                           (if expr2
                               (let ((op2 (parse-operator port)))
                                 (if (not op2)
                                     (apt-operator expr1 op1 expr2)
                                     (if (and (not (operator-right? op1))
                                             (operator>? op1 op2) )
                                         (loop (apt-operator expr1 op1 expr2) op2)
                                         (apt-operator expr1 op1 (loop expr2 op2)))))
                               #f)
                           ))) )))
    (let ((expr1 (parse-atom-expr port)))
      (if expr1
          (let ((op1 (parse-operator port)) )
            (if op1
                (loop expr1 op1)
                expr1))
          #f))))


;;----------------------------------------------------------------------
;; type
;;----------------------------------------------------------------------

(define (parse-type-params port del-token? params)
  (if (apt-id? current-token)
      (let ((type (parse-type port)))
        (cond ((apt-punct-eq? current-token ",")
               (let ((comma current-token))
                 (next-token port)
                 (parse-type-params port del-token?
                                    (append params
                                            (list type comma)))))
              ((del-token? current-token)
               (begin
                 (append params (list type))))

              (else (syntax-error "Unexpected token" current-token)) ))
      params))


(define (parse-complex-type-del port type-list)
  (cond ((apt-punct-eq? current-token ",")
         (begin
           (next-token port)
           (parse-complex-type port type-list)))
        ((apt-punct-eq? current-token ")")
         (begin
           (next-token port)
           (apt-seq* type-list)))
        (else (syntax-error "complex type: expected , or ), got: "
                            current-token))))


(define (parse-complex-type port type-list)
  (cond ((apt-id? current-token)
         (let ((ty (parse-simple-type port #t)))
           (if ty
               (parse-complex-type-del port (append type-list (list ty)))
               #f)))
        ((apt-punct-eq? current-token "(")
         (let ((ty (parse-complex-type port '())))
           (if ty
               (parse-complex-type-del port (append type-list (list ty)))
               #f)))
        (else (syntax-error "complex type: expected ID or (, got: "
                            current-token))))


(define (parse-array-type port base-type)
  (if (apt-punct-eq? current-token "[")
      (let ((size-param #f))
        (next-token port)
        (if (not (apt-punct-eq? current-token "]"))
            (set! size-param (parse-expr port)))
        (if (apt-punct-eq? current-token "]")
            (begin
              (next-token port)
              (if size-param
                  (apt-seq base-type (apt-nested "[" "]" size-param))
                  (apt-seq base-type (apt-nested "[" "]"))))
            (syntax-error "Expected ], got" current-token)))
      base-type))


(define (parse-simple-type port expect-constraint?)
  (let* ((sym current-token))
    (next-token port)
    (cond ((apt-punct-eq? current-token "(")
           (begin
             (next-token port)
             (let ((params (parse-type-params port
                                              (lambda (token)
                                                (apt-punct-eq? token ")"))
                                              '())))
               (if (apt-punct-eq? current-token ")")
                   (begin
                     (next-token port)
                     (parse-array-type port
                                       (apt-seq sym (apt-nested* "(" ")"
                                                                 params))))
                   (syntax-error "Expected ), got" current-token)))) )
          ((and expect-constraint? 
                (apt-punct-eq? current-token "="))
           (begin
             (next-token port)
             (let ((constraint (parse-expr port)))
               (if constraint
                   (apt-seq* (list sym (apt-punct "=") constraint))
                   #f))))
          (else (parse-array-type port sym) ))))


(define (parse-type port)
  (cond
   ((apt-id? current-token) (parse-simple-type port #f))
   ((apt-punct-eq? current-token "(") (begin
                                        (next-token port)
                                        (parse-array-type port 
                                                          (parse-complex-type port
                                                                              '()))))
   (else (syntax-error "Expected type, got" current-token))))


;;----------------------------------------------------------------------
;; def type
;;----------------------------------------------------------------------

(define (parse-type-slot-decl-params port params)
  (cond ((apt-id-keyarg? current-token)
         (let* ((key current-token)
                (val #f))
           (next-token port)
           (set! val (parse-expr port))
           (if val
               (if (apt-punct-eq? current-token ",")
                   (let ((comma current-token))
                     (next-token port)
                     (parse-type-slot-decl-params port
                                                  (append params
                                                          (list (apt-seq key val)
                                                                comma))))
                   (append params (list (apt-seq key val))) )
               #f)))
        ((apt-id? current-token)
         (let ((sym current-token))
           (next-token port)
           (if (apt-punct-eq? current-token ",")
               (let ((comma current-token))
                 (next-token port)
                 (parse-type-slot-decl-params port
                                              (append params
                                                      (list sym comma))))
               (append params (list sym)))))
        (else params)))


(define (parse-type-slot-decl port)
  (if (apt-id? current-token)
      (let* ((sym current-token)
             (type #f)
             (init-value #f)
             (params #f))
        (next-token port)
        (if (apt-punct-eq? current-token ":")
            (begin
              (next-token port)
              (set! type (parse-type port))))
        (if (apt-punct-eq? current-token "=")
            (begin
              (next-token port)
              (set! init-value (parse-expr port))))
        (if (apt-punct-eq? current-token ",")
            (begin
              (next-token port)
              (set! params (parse-type-slot-decl-params port '()))))

        (let ((res (list (apt-id "slot") sym)))
          (if type
              (set! res (append res (list (apt-punct ":") type))))
          (if init-value
              (set! res (append res (list (apt-punct "=") init-value))))
          (if params
              (set! res (append res (append (list (apt-punct ","))
                                            params))))
          (apt-seq* res)))
      (syntax-error "Expected symbol, got: " current-token)))


(define (parse-typedef-decls port)
  (let loop ((res '()))
    (cond ((apt-id? current-token)
           (let* ((sym (apt-id-value current-token))
                  (expr (cond ((equal? sym "slot") (begin
                                                     (next-token port)
                                                     (parse-type-slot-decl port)))
                              ((equal? sym "on") (begin
                                                   (next-token port)
                                                   (parse-on port '("init" "delete"))))
                              (else (syntax-error "Unexpected symbol:" current-token)))))
             (if expr
                 (loop (append res (list expr)))
                 #f)))
          ((apt-punct-eq? current-token "}") res)
          (else (syntax-error "Unexpect token:" current-token)))))


(define (parse-type-def port modifiers)
  (if (apt-id? current-token)
      (let ((sym current-token))
        (next-token port)
        (if (apt-punct-eq? current-token "(")
            (begin
              (next-token port)
              (let ((params (parse-functions-params port '()))
                    (isatype #f)
                    (decls #f))
                (if (apt-punct-eq? current-token ":")
                    (begin
                      (next-token port)
                      (set! isatype (parse-type port))))
                (if (apt-punct-eq? current-token "{")
                    (begin
                      (next-token port)
                      (set! decls (parse-typedef-decls port))
                      (if (apt-punct-eq? current-token "}")
                          (begin
                            (next-token port)
                            (apt-class sym params isatype decls))
                          (error-expected-token 'type-def "}")))
                    (error-expected-token 'type-def "{"))) )
            (error-expected-token 'type-def "(")))
      (error-expected-token 'type-def "symbol")))


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


(define (parse-macro-comp port)
  (if (apt-punct-eq? current-token "{")
      (begin
        (next-token port)
        (let loop ((res '())
                   (brace-count 1))
          (let ((token current-token))
            (cond ((apt-punct-eq? current-token "}")
                   (let ((count (- brace-count 1)))
                     (next-token port)
                     (if (<= count 0)
                         res
                         (loop (append res (list token))
                               count))))
                  ((apt-punct-eq? current-token "{")
                   (let ((count (+ brace-count 1)))
                     (next-token port)
                     (loop (append res (list token))
                           count)))
                  (else (begin
                          (next-token port)
                          (loop (append res (list token))
                                brace-count)) )))))
      (syntax-error "Expected {, got" current-token)))


(define (parse-macro-basic-pattern port pattern-name)
  (let ((pattern (parse-macro-comp port))
        (replc #f))
    (if pattern
        (if (apt-id-eq? current-token "->")
            (begin
              (next-token port)
              (set! replc (parse-macro-comp port))
              (list 'prod
                    ':name pattern-name
                    ':pattern pattern ':replc replc))
            (syntax-error "Expected ->, got" current-token))
        #f)))


(define (parse-macro-patterns port)
  (let loop ((res '())
             (last-pattern-name #f))
    (cond ((apt-punct-eq? current-token "}")  res)
          ((apt-punct-eq? current-token "{") (let ((expr (parse-macro-basic-pattern
                                                  port
                                                  last-pattern-name)))
                                       (if expr
                                           (loop (append res (list expr))
                                                 last-pattern-name)
                                           #f)))
          ((apt-id-keyarg? current-token) (let ((sym current-token))
                                            (next-token port)
                                            (loop res (apt-id-keyarg-value sym))))
          (else (syntax-error "Unexpected token:" current-token)))))


(define (parse-macro-def port modifiers)
  (if (apt-id? current-token)
      (let ((sym (apt-id-value current-token)))
        (next-token port)
        (if (apt-punct-eq? current-token "{")
            (begin
              (next-token port)
              (let* ((patterns (parse-macro-patterns port))
                     (macro-type (macro-determine-type patterns)))
                (if (apt-punct-eq? current-token "}")
                    (begin
                      (next-token port)
                      (if patterns
                          (begin
                            (macro-register macro-type sym
                                            (vector ':macro
                                                    patterns
                                                    (macro-compile (list 'macro
                                                                         ':patterns patterns))))
                            'ignore)
                          #f))
                    (syntax-error "Expected }, got" current-token)) ))
            (syntax-error "Expected {, got" current-token)))
      (syntax-error "Expected symbol, got" current-token)))


(define (parse-alias-def port modifiers)
  (if (apt-id? current-token)
      (let* ((sym current-token))
        (next-token port)
        (if (apt-punct-eq? current-token "=")
            (begin
              (next-token port)
              (let ((type (parse-type port)))
                (apt-seq (apt-id "alias") sym (apt-punct ":") type)))
            (syntax-error "Expected =, got" current-token)))
      (syntax-error "Expected symbol, got" current-token)))


(define (apt-param sym flag type init-value)
  (if (eq? flag 'rest)
      (apt-seq sym (apt-id "..."))
      (if (or (eq? flag 'spec) type init-value)
          (let ((nl (list sym)))
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


(define (parse-functions-params port param-list)
  (cond
   ((apt-id? current-token)
    (let* ((sym current-token)
           (type #f)
           (spec? #f)
           (init-value #f))
      (next-token port)

      (if (apt-id-eq? current-token "...")
          (begin
            (next-token port)

            (if (apt-punct-eq? current-token ")")
                (begin
                  (next-token port)
                  (append param-list (apt-param sym 'rest type init-value)))
                (syntax-error "Rest argument must be last in parameter list"
                              current-token)))

          (cond
           ((apt-punct-eq? current-token ":")
            (begin
              (next-token port)

              (if (apt-punct-eq? current-token "@")
                  (begin
                    (next-token port)
                    (set! spec? 'spec)))

              (set! type (parse-type port))

              (if (apt-punct-eq? current-token "=")
                  (begin
                    (next-token port)
                    (if (eq? spec? 'spec)
                        (syntax-error "Keyed parameters can not be specialized"
                                      current-token))
                    (set! spec? 'key)
                    (set! init-value (parse-expr port))))

              (cond
               ((apt-punct-eq? current-token ",")
                (begin
                  (next-token port)
                  (parse-functions-params port
                                          (append param-list
                                                  (list (apt-param sym spec? type
                                                                   init-value))))))

               ((apt-punct-eq? current-token ")")
                (begin
                  (next-token port)
                  (append param-list (list (apt-param sym spec? type init-value)))))

               (else (syntax-error "Unexpected token (2)" current-token)) )))

           ((apt-punct-eq? current-token "=")
            (begin
              (next-token port)
              (set! spec? 'key)
              (set! init-value (parse-expr port))

              (cond
               ((apt-punct-eq? current-token ")")
                (begin
                  (next-token port)
                  (append param-list (list (apt-param sym spec? type init-value)))))
               ((apt-punct-eq? current-token ",")
                (begin
                  (next-token port)
                  (parse-functions-params port
                                          (append param-list
                                                  (list (apt-param sym spec? type
                                                                   init-value))))))
               (else (syntax-error "Unexpected token (3)" current-token)) )))

           ((apt-punct-eq? current-token ",")
            (begin
              (next-token port)
              (parse-functions-params port
                                      (append param-list
                                              (list (apt-param sym spec? type
                                                               init-value))))))

           ((apt-punct-eq? current-token ")")
            (begin
              (next-token port)
              (append param-list (list (apt-param sym spec? type init-value)))))

           (else (syntax-error "Unexpected token (4)" current-token)) ))))

   ((apt-punct-eq? current-token ")")
    (begin
      (next-token port)
      param-list))

   (else (syntax-error "Unexpected token (5)" current-token)) ))


(define (apt-map-scope scope)
  (cond ((eq? scope 'global) (apt-id "def"))
        ((eq? scope 'local) (apt-id "let"))
        (else (apt-id "bad"))))


(define (apt-function params type body)
  (apt-seq (apt-punct "#function")
           (apt-nested* "(" ")" params)
           (if type (apt-punct ":") #f)
           (if type type #f)
           (apt-nested* "{" "}" body)))


(define (apt-funcdef scope sym params type body)
  (apt-seq (apt-map-scope scope)
           ;; modifiers
           sym
           (apt-nested* "(" ")" params)
           (if type (apt-punct ":") #f)
           (if type type #f)
           (apt-nested* "{" "}" body)))


(define (parse-func-def port sym modifiers scope)
  (let* ((params (parse-functions-params port '()))
         (type (if (apt-punct-eq? current-token ":")
                   (begin
                     (next-token port)
                     (parse-type port))
                   #f))
         (body (if (eq? scope 'global)
                   (parse-exprlist-until-def port '())
                   (list (parse-expr port)))) )
    (if sym
        (apt-funcdef scope sym params type body)
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


(define (parse-func-or-var-def port scope modifiers)
  (let* ((sym current-token)
         (macro-id (apt-id-value sym))
         (macro    (macro-lookup macro-id))
         (type     (macro-type macro-id)))
    (if macro
        (begin
          (let ((expr (parse-make-macro-call port sym '() macro type #t
                                             scope)))
            (if expr
                expr
                (syntax-error "Incomplete macro appliance: " macro-id))))
        (begin
          (next-token port)
          (cond
           ((apt-punct-eq? current-token "(")
            (begin
              (next-token port)
              (parse-func-def port sym modifiers scope)))

           ((apt-punct-eq? current-token ":")
            (begin
              (next-token port)
              (let ((type (parse-type port)))
                (if (apt-punct-eq? current-token "=")
                    (begin
                      (next-token port)
                      (let ((init-value (parse-expr port)))
                        (apt-vardef scope sym type modifiers init-value)))
                    (apt-vardef scope sym type modifiers #f)))
              ))

           ((apt-punct-eq? current-token "=")
            (begin
              (next-token port)
              (let ((init-value (parse-expr port)))
                (apt-vardef scope sym #f modifiers init-value))))

           (else (apt-vardef scope sym #f modifiers #f))
           )))))


(define (parse-def port scope)
  (next-token port)
  (let* ((modifiers (parse-modifiers port '("meth" "fluid" "const"))))
    (if (apt-id? current-token)
        (cond
         ((apt-id-eq? current-token "type")  (begin
                                               (next-token port)
                                               (parse-type-def port modifiers)))
         ((apt-id-eq? current-token "macro") (begin
                                               (next-token port)
                                               (parse-macro-def port modifiers)))
         ((apt-id-eq? current-token "alias") (begin
                                               (next-token port)
                                               (parse-alias-def port modifiers)))
         ((apt-id? current-token) (parse-func-or-var-def port scope modifiers))
         (else (syntax-error "Expected symbol, got" current-token)))
        (syntax-error "Expected symbol, got" current-token))
     ))


(define (parse-namespace port)
  (next-token port)
  (if (apt-id? current-token)
      (let ((nsname current-token))
        (next-token port)
        (if (apt-punct-eq? current-token "(")
            (begin
              (next-token port)
              (let ((str (parse-expr port)))
                (if (apt-punct-eq? current-token ")")
                    (begin
                      (next-token port)
                      (apt-seq (apt-id "namespace")
                               nsname
                               (apt-nested "(" ")" str)))
                    (syntax-error "Expected ), got" current-token))))
            (apt-seq (apt-id "namespace")
                     nsname)))
      (syntax-error "Expected SYMBOL, got " current-token)))


(define (parse-import port)
  (next-token port)
  (if (apt-punct-eq? current-token "(")
      (begin
        (next-token port)
        (let ((prms (parse-funcall-params port '())))
          (apt-seq (apt-id "import") (apt-nested* "(" ")" prms))))
      (syntax-error "import: Expected (, got" current-token)))


(define (parse-next-top port)
  (let loop ((apt (apt-alloc)))
    (if (eq? current-token 'EOF)
        apt
        (begin
          (if (apt-id? current-token)
              (let* ((sym (apt-id-value current-token))
                     (expr (cond
                            ((equal? sym "def") (parse-def port 'global))
                            ((equal? sym "namespace") (parse-namespace port))
                            ((equal? sym "import") (parse-import port))
                            (else (syntax-error "Unexpected symbol" current-token)))) )
                (cond ((eq? expr 'ignore) (loop apt))
                      ((not expr) #f)
                      (else (loop (append apt (list expr))))))
              (syntax-error "Unexpected token (6)" current-token))))) )



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
