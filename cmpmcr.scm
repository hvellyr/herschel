(load "../string.scm")


;;;(define mcr '(macro :patterns
;;;               ((prod :name #f
;;;                      :pattern ((SYM . "incr!") PRO (MACROPARAM . "place:expr") PRC)
;;;                      :replc ((MACROPARAM . "place") ASSIGN
;;;                              (MACROPARAM . "place") ADD (INT . 1)))
;;;                (prod :name #f
;;;                      :pattern ((SYM . "incr!") PRO (MACROPARAM . "place:expr") COMMA
;;;                                (MACROPARAM . "amount:expr") PRC)
;;;                      :replc ((MACROPARAM . "place") ASSIGN
;;;                              (MACROPARAM . "place") ADD (MACROPARAM . "amount"))))))
;;;
;;;(define mcr2 '(macro :patterns ((prod :name #f
;;;                                      :pattern ((SYM . "def") (SYM . "funcvar") (MACROPARAM . "name:id") (MACROPARAM . "init:opt-expr"))
;;;                                      :replc ((SYM . "def") (MACROPARAM . "name") SANGHASH (SYM . "-var") ASSIGN (MACROPARAM . "init") (SYM . "def") (MACROPARAM . "name") PRO PRC (MACROPARAM . "name") SANGHASH (SYM . "-var") (SYM . "def") (MACROPARAM . "name") PRO (SYM . "value") PRC BRCO (MACROPARAM . "name") SANGHASH (SYM . "-var") ASSIGN (SYM . "value") (SYM . "value") BRCC))
;;;                                (prod :name "opt-expr"
;;;                                      :pattern ()
;;;                                      :replc ())
;;;                                (prod :name "opt-expr"
;;;                                      :pattern (ASSIGN (MACROPARAM . "e:expr"))
;;;                                      :replc ((MACROPARAM . "e"))))))
;;;

;; { for (?abcs) } -> 1
;; abcs:
;; { abc } -> $1
;; { abc, abcs } ->
;;
;; 0 for  -> get-next -> 1
;; 1 (    -> get-next -> 3
;; 2 )    -> DONE
;; 3 abc  -> get-next -> 4
;; 4 ,    -> get-next -> 3
;;   ?    -> 2

(define mcr3 '(macro :patterns
               ((prod :name #f
                      :pattern ((SYM . "hello") PRO PRC)
                      :replc ((SYM . "world")))
                (prod :name #f
                      :pattern ((SYM . "hallo") PRO (SYM . "then") PRC)
                      :replc ((SYM . "world")))
                (prod :name #f
                      :pattern ((SYM . "hello") (SYM . "I") (SYM . "like") (SYM . "you"))
                      :replc ((STR . "hello I like you")))
                )))


(define (macro-patterns macr)
  (let ((p (member ':patterns macr)))
    (if p
        (cadr p)
        '())))

(define (macro-prod-name prod)
  (let ((n (member ':name prod)))
    (if n
        (cadr n)
        #f)))

(define (macro-prod-pattern prod)
  (let ((n (member ':pattern prod)))
    (if n
        (cadr n)
        #f)))

(define (macro-prod-replacement prod)
  (let ((n (member ':replc prod)))
    (if n
        (cadr n)
        #f)))



(define (macro-get-prods-by-name macr name)
  (let loop ((res '())
             (nl (macro-patterns macr)))
    (if (null? nl)
        res
        (if (equal? (macro-prod-name (car nl)) name)
            (loop (append res (list (car nl)))
                  (cdr nl))
            (loop res (cdr nl))))) )


(define (macro-split-macroparam str)
  (let ((res (arc:split-string str #\:)))
    res))

(define (macro-builtin-matchtype type)
  (or (equal? type "expr")
      (equal? type "int")
      (equal? type "bool")
      (equal? type "string")
      (equal? type "id") ))


(define (syntax-table-alloc)
  (vector 'syntax-table
          (list)))


(define (syntax-table-find-pattern st name)
  (let ((tree (assoc name (vector-ref st 1))))
    (if tree
        (cadr tree)
        (vector '() 'NONE))))

(define (syntax-table-set-pattern! st name pattern)
  (let ((tree (assoc name (vector-ref st 1))))
    (if tree
        (set-cdr! tree (list pattern))
        (vector-set! st 1 (append (vector-ref st 1)
                                  (list (list name pattern)))))
    st))


(define (macroparam? token)
  (and (pair? token)
       (equal? (car token) 'MACROPARAM)))

(define (token=? tok1 tok2)
  (if (and (pair? tok1) (pair? tok2))
      (if (and (eq? (car tok1) 'macro-param)
               (eq? (car tok2) 'macro-param))
          #t
          (and (equal? (car tok1) (car tok2))
               (equal? (cdr tok1) (cdr tok2))))
      (equal? tok1 tok2)) )


(define (st-find-node node token)
  (let loop ((nl (vector-ref node 0)))
    (if (null? nl)
        #f
        (if (token=? (caar nl) token)
            (cadar nl)
            (loop (cdr nl))))))


(define (st-has-eof-set? node)
  (let loop ((nl (vector-ref node 0)))
    (if (null? nl)
        #f
        (if (eq? (caar nl) 'END)
            #t
            (loop (cdr nl))))))


(define (st-find-macro-param node)
  (st-find-node node (apt-macro-param "*:expr")))


(define (st-extract-macro-param node)
  (let ((token (apt-macro-param "*:expr")))
    (let loop ((nl (vector-ref node 0)))
      (if (null? nl)
          #f
          (if (token=? (caar nl) token)
              (caar nl)
              (loop (cdr nl)))))))


(define (st-replacement node)
  (vector-ref node 1))


(define (syntax-table-mixin-pattern-part tree src-pattern replc)
  (let loop ((res tree)
             (node tree)
             (nl src-pattern))
    (if (null? nl)
        (begin
          (vector-set! node 0 (append (vector-ref node 0)
                                      (list (list 'END))))
          (vector-set! node 1 replc)
          res)
        (let* ((token (car nl)))
          (if (macroparam? token)
              (let ((smt (macro-split-macroparam (cdr token))))
                (set! token (list 'MP (cadr smt) (car smt)))))

          (let ((step (st-find-node node token)))
            (if (not step)
                (begin
                  (set! step (vector '() 'NONE))
                  (vector-set! node 0 (append (vector-ref node 0)
                                              (list (list token step))))))
            (loop res step (cdr nl))) ))))


(define (syntax-table-mixin-pattern! st name pattern replc)
  (let* ((pattern-tree (syntax-table-find-pattern st name))
         (new-pattern-tree (syntax-table-mixin-pattern-part pattern-tree
                                                            pattern
                                                            replc)))
    (syntax-table-set-pattern! st name new-pattern-tree)))


(define (macro-compile macr)
  (let loop ((res (syntax-table-alloc))
             (nl (macro-patterns macr)))
    (if (null? nl)
        res
        (let* ((prod    (car nl))
               (name    (macro-prod-name prod))
               (pattern (macro-prod-pattern prod))
               (replc   (macro-prod-replacement prod)))
          ;;(display "Compile: ") (display name) (newline)
          (syntax-table-mixin-pattern! res name pattern replc)
          (loop res (cdr nl))))))

;(display (member ':patterns mcr)) (newline)

;(display (macro-compile mcr)) (newline)



