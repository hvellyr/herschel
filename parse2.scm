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


(define (parse-modifiers-2p nl target possible-modifiers)
  (if (or (null? nl)
          (not (apt-id? (car nl))))
      nl
      (if (member (apt-id-value (car nl)) possible-modifiers)
          (begin
            (vector-set! target 0 (append (vector-ref target 0)
                                          (list (apt-id-value (car nl)))))
            (parse-modifiers-2p (cdr nl) target
                                possible-modifiers))
          nl)))


(define (parse-id-2p node)
  (make-object <apt:symbol> (list (apt-id-value node))))


(define (parse-lit-2p node)
  (let ((type (cond ((apt-lit-string? node)  'str)
                    ((apt-lit-keyword? node) 'keyw)
                    ((apt-lit-int? node)     'int)
                    ((apt-lit-char? node)    'char)
                    ((apt-lit-bool? node)    'bool)
                    ((apt-lit-nil? node)     'nil)
                    ((apt-lit-eof? node)     'eof)
                    (else 'unknown)
                    )))
    (make-object <apt:const> (list type (apt-lit-value node)))))


(define (parse-expr-2p node)
  (cond ((apt-id? node)     (parse-id-2p node))
        ((apt-lit? node)    (parse-lit-2p node))
        ((apt-seq? node)    (parse-seq-2p node))
        ((apt-nested? node) (parse-nested-2p node))
        ((apt-punct? node)  (syntax-error "Unexpected punctuation: "
                                          (apt-punct-value node)))
        (else (syntax-error "Unexpected node: " node))))


(define (parse-nested-2p node)
  (cond ((apt-nested-left? node "(")
         ;; TODO gck
         ...)
        ((apt-nested-left? node "{")
         (parse-block-2p (apt-nested-body node)))
        ((apt-nested-left? node "#(")
         (parse-vector-2p (apt-nested-body node)))
        ((apt-nested-left? node "#[")
         (parse-array-2p (apt-nested-body node)))
        (syntax-error "Unknown nested syntax: " node)))


(define (parse-vector-2p token-list)
  (let* ((dict? #f)
         (parsed-exprs
          (let loop ((res '())
                     (nl token-list))
            (if (null? nl)
                res
                (cond ((apt-punct-eq? (car nl) ",")
                       (loop res (cdr nl)))
                      (else (let ((expr (parse-expr-2p (car nl))))
                              (if (and (is-a? expr <apt:binary>)
                                       (equal? (operator expr) "->"))
                                  (set! dict? #t)
                                  (if dict?
                                      (syntax-error "Inhomogenous dict notation"
                                                    token-list)))
                              (loop (append res (list expr))
                                    (cdr nl)))))) )))
    (if dict?
        (make-object <apt:dictionary> (list 'dict parsed-exprs))
        (make-object <apt:vector> (list 'vector parsed-exprs)))))


(define (parse-array-2p token-list)
  (let ((parsed-exprs (let loop ((res '())
                                 (nl token-list))
                        (if (null? nl)
                            res
                            (cond ((apt-punct-eq? (car nl) ",")
                                   (loop res (cdr nl)))
                                  (else (loop (append res
                                                      (list (parse-expr-2p
                                                             (car nl))))
                                              (cdr nl)))))) ))
    (make-object <apt:array> (list 'array parsed-exprs))))


;;;----------------------------------------------------------------------

;;; parse types

;;; (id "Int")
;;; (seq (id "HashMap") (nested "(" ")" (id "String") (punct ",") (id "Int")))
;;; (seq (id "Byte") (nested "[" "]"))
;;; (seq (id "String") (id "Bool"))
;;; (seq (seq (id "Int") (punct "=") (seq (lit -127) (id "..") (lit 127))))
(define (parse-union-type-2p token-list type-list)
  (let loop ((res type-list)
             (nl token-list))
    (if (null? nl)
        (make-object <apt:union-type> (list res))
        (if (apt-id? (car nl))
            (loop (append res (list (parse-type-2p (car nl))))
                  (cdr nl))
            (syntax-error "Unexpected node in union type expr" token-list)))))


(define (parse-array-type-2p base token-list)
  (cond ((equal? (length token-list) 1)
         (make-object <apt:array-type> (list base (parse-expr-2p (car token-list)))))
        ((equal? (length token-list) 0)
         (make-object <apt:array-type> (list base #f)))
        (syntax-error "unexpected nodes in array type size" token-list)))


(define (parse-constraint-type-2p base token-list)
  (if (not (null? token-list))
      (make-object <apt:constraint-type> (list base (parse-expr-2p (car token-list))))
      (syntax-error "expected constraint expression" token-list)))


(define (parse-param-type-args-2p nl res)
  (if (null? nl)
      res
      (let ((ty (parse-type-2p (car nl))))
        (if ty
            (parse-param-type-args-del-2p (cdr nl)
                                          (append res (list ty)))
            (syntax-error "Unexpected node in param type" nl)))))

(define (parse-param-type-args-del-2p nl res)
  (cond ((null? nl) res)
        ((apt-punct-eq? (car nl) ",")
         (parse-param-type-args-2p (cdr nl) res))
        (else (syntax-error "Unexpected node in param type" nl))))


(define (parse-param-type-2p base token-list)
  (make-object <apt:param-type>
               (list base
                     (parse-param-type-args-2p token-list '()))))


(define (parse-type-seq-2p token-list)
  (if (not (null? token-list))
      (let ((first (parse-type-2p (car token-list))))
        (if (not (null? (cdr token-list)))
            (cond ((apt-nested-left? (cadr token-list) "(")
                   (parse-param-type-2p first (apt-nested-body (cadr token-list))))
                  ((apt-nested-left? (cadr token-list) "[")
                   (parse-array-type-2p first (apt-nested-body (cadr token-list))))
                  ((apt-id? (cadr token-list)) (parse-union-type-2p (cdr token-list)
                                                                    (list first)))
                  ((apt-punct-eq? (cadr token-list) "=")
                   (parse-constraint-type-2p first (cddr token-list)))
                  (else (syntax-error "Unhandled type expression (2)" token-list)))
            first))
      (syntax-error "Unhandled type expression (3)" token-list)))


(define (parse-type-2p node)
  (cond ((apt-id? node) (make-object <apt:simple-type> (list (apt-id-value node))))
        ((apt-seq? node) (parse-type-seq-2p (apt-seq-body node)))
        (else (syntax-error "Unhandled type expression" node))))


(define (parse-func-param-2p node)
  (cond ((apt-id? node) (make-object <apt:param> (list #f (apt-id-value node) 'normal #f
                                                       #f)))
        ((apt-seq? node)
         (let ((token-list (apt-seq-body node)))
           (if (not (null? token-list))
               (let* ((nl token-list)
                      (keyarg #f))
                 (if (apt-id-keyarg? (car nl))
                     (begin
                       (set! keyarg (apt-id-value (car nl)))
                       (set! nl (cdr nl))))
                 (if (not (null? nl))
                     (begin
                       (if (apt-id? (car nl))
                           (let ((sym (apt-id-value (car nl))))
                             (set! nl (cdr nl))
                             (if (not (null? nl))
                                 (if (apt-id-eq? (car nl) "...")
                                     (make-object <apt:param> (list #f sym 'rest #f #f))
                                     (let ((ty #f)
                                           (init #f)
                                           (param-type 'normal))
                                       (if (apt-punct-eq? (car nl) ":")
                                           (if (not (null? (cdr nl)))
                                               (begin
                                                 (set! ty (parse-type-2p (cadr nl)))
                                                 (set! nl (cddr nl)))
                                               (syntax-error "Expected type" nl)))
                                       (if (and (not (null? nl))
                                                (apt-punct-eq? (car nl) "="))
                                           (if (not (null? (cdr nl)))
                                               (begin
                                                 (set! init (parse-expr-2p (cadr nl)))
                                                 (set! nl (cddr nl))
                                                 (set! param-type 'key)
                                                 (if (not keyarg)
                                                     (set! keyarg (string-append sym ":"))))
                                               (syntax-error "expected =" node)))
                                       (make-object <apt:param> (list keyarg sym
                                                                      param-type ty
                                                                      init))))
                                 ;; else
                                 (make-object <apt:param> (list keyarg sym 'normal #f #f))))
                           ;; else
                           (syntax-error "expected symbol" node)))
                     ;; else
                     (syntax-error "Expected symbol node" node)))
               ;; else
               (syntax-error "Bad param node" node))))
        ;; else
        (else (syntax-error "Bad param node (2)" node))))


(define (parse-function-2p token-list sym meth? scope)
  (let* ((retval #f)
         (params '())
         (body #f)
         (nl token-list))
    (if (apt-nested? (car nl))
        (set! params (map (lambda (n)
                            (parse-func-param-2p n))
                          (apt-nested-body (car nl))))
        (syntax-error "Bad node.  Nested expected: " nl))
    (if (not (null? (cdr nl)))
        (begin
          (set! nl (cdr nl))
          (if (apt-punct-eq? (car nl) ":")
              (if (not (null? (cdr nl)))
                  (begin
                    (set! retval (parse-type-2p (cadr nl)))
                    (set! nl (cddr nl)))))
          (set! body (parse-expr-2p (car nl)))
          (if sym
              (make-object <apt:def>
                           (list scope sym
                                 (make-object <apt:function>
                                              (list retval params body meth?))))
              (make-object <apt:function> (list retval params body meth?))))
        (syntax-error "Bad node: " token-list))))


(define (parse-funcdef-2p token-list meth? scope)
  (if (apt-id? (car token-list))
      (let* ((sym (apt-id-value (car token-list))))
        (if (not (null? (cdr token-list)))
            (parse-function-2p (cdr token-list) sym meth? scope)
            (syntax-error "Bad node: " token-list)))
      (syntax-error "Bad node. Symbol expected: " token-list)))


(define (parse-methdef-2p token-list)
  (parse-funcdef-2p token-list #t 'global))


(define (parse-vardef-2p token-list const? fluid?)
  (if (apt-id? (car token-list))
      (let* ((sym (apt-id-value (car token-list)))
             (type #f)
             (init-value #f))
        (if (not (null? (cdr token-list)))
            (let ((nl (cdr token-list)))
              (if (apt-punct-eq? (car nl) ":")
                  (if (not (null? (cdr nl)))
                      (begin
                        (set! type (parse-type-2p (cadr nl)))
                        (if (not (null? (cddr nl)))
                            (set! nl (cddr nl))
                            (set! nl '())))))
              (if (not (null? nl))
                  (if (apt-punct-eq? (car nl) "=")
                      (if (not (null? (cdr nl)))
                          (set! init-value (parse-expr-2p (cadr nl))))))))
        (make-object <apt:vardef> (list sym type init-value
                                        const? fluid?)))
      #f))


(define (parse-const-2p token-list)
  (parse-vardef-2p token-list #t #f))


(define (parse-fluid-2p token-list)
  (parse-vardef-2p token-list #f #t))


(define (parse-class-param-2p node)
  (cond ((apt-id? node) (make-object <apt:type-param> (list (apt-id-value node) #f)))
        ((apt-seq? node)
         (let* ((token-list (apt-seq-body node))
                (nl token-list))
           (if (not (null? nl))
               (begin
                 (if (apt-id? (car nl))
                     (let ((sym (apt-id-value (car nl))))
                       (set! nl (cdr nl))
                       (if (not (null? nl))
                           (let ((ty #f)
                                 (init #f)
                                 (param-type 'normal))
                             (if (apt-punct-eq? (car nl) "=")
                                 (if (not (null? (cdr nl)))
                                     (begin
                                       (set! init (parse-type-2p (cadr nl)))
                                       (set! nl (cddr nl)))
                                     (syntax-error "expected default value" node)))
                             (make-object <apt:type-param> (list sym init)))
                           ;; else
                           (make-object <apt:type-param> (list sym #f))))
                     ;; else
                     (syntax-error "expected symbol" node)))
               ;; else
               (syntax-error "Expected symbol node" node))))
        ;; else
        (else (syntax-error "Bad class param node" node))))


(define (parse-class-params-2p token-list)
  (map (lambda (n)
         (parse-class-param-2p n))
       token-list))


(define (parse-class-decls-2p token-list)
  #f)


(define (parse-classdef-2p token-list)
  (let ((nl token-list)
        (sym #f)
        (params #f)
        (derives-from #f))
    (if (not (null? nl))
        (begin
          (set! sym (apt-id-value (car nl)))
          (set! nl (cdr nl))
          (if (and (not (null? nl))
                   (apt-nested? (car nl)))
              (begin
                (set! params (parse-class-params-2p (apt-nested-body (car nl))))
                (set! nl (cdr nl))))
          (if (and (not (null? nl))
                   (apt-punct-eq? (car nl) ":"))
              (if (not (null? (cdr nl)))
                  (begin
                    (set! derives-from (parse-type-2p (cadr nl)))
                    (set! nl (cddr nl)))))
          (if (and (not (null? nl))
                   (apt-nested? (car nl)))
              (let ((decls (parse-class-decls-2p (apt-nested-body (car nl)))))
                (make-object <apt:classdef> (list sym params derives-from decls)))
              (syntax-error "Expected class declarations { }" token-list)))
        (syntax-error "Bad class nodes" token-list))))


(define (parse-typedef-2p token-list)
  (arc:display "Parsing 'type' not done yet" 'nl)
  #f)

(define (parse-alias-2p token-list)
  (arc:display "Parsing 'alias' not done yet" 'nl)
  #f)


(define (parse-def-2p token-list scope)
  (let ((node (car token-list)))
    (cond ((apt-id-eq? node "meth")  (parse-methdef-2p (cdr token-list)))
          ((apt-id-eq? node "const") (parse-const-2p (cdr token-list)))
          ((apt-id-eq? node "fluid") (parse-fluid-2p (cdr token-list)))
          ((apt-id-eq? node "class") (parse-classdef-2p (cdr token-list)))
          ((apt-id-eq? node "type")  (parse-typedef-2p (cdr token-list)))
          ((apt-id-eq? node "alias") (parse-alias-2p (cdr token-list)))
          ((apt-id? node)
           (if (not (null? (cdr token-list)))
               (cond ((apt-nested? (cadr token-list))
                      (parse-funcdef-2p token-list #f scope))
                     ((apt-punct-eq? (cadr token-list) "=")
                      (parse-vardef-2p token-list #f #f))
                     ((apt-punct-eq? (cadr token-list) ":")
                      (parse-vardef-2p token-list #f #f))
                     (else (syntax-error "error: unsupported node: " node)) )
               (parse-vardef-2p token-list #f #f)))
          (else (syntax-error "error: unsupported node: " node)) )))


(define (parse-namespace-2p token-list)
  (if (apt-id? (car token-list))
      (let ((sym (apt-id-value (car token-list))))
        (if (and (not (null? (cdr token-list)))
                 (apt-nested? (cadr token-list))
                 (> (length (apt-nested-body (cadr token-list))) 0)
                 (apt-lit-string? (car (apt-nested-body (cadr token-list)))))
            (make-object <apt:namespace>
                         (list sym
                               (apt-lit-value (car (apt-nested-body
                                                    (cadr token-list))))) )
            (make-object <apt:namespace>
                         (list sym #f))
            #f))
      #f))


(define (parse-operator-2p? elt)
  (or (apt-punct-eq? elt "=")
      (apt-id-eq? elt "+")
      (apt-id-eq? elt "-")
      (apt-id-eq? elt "*")
      (apt-id-eq? elt "/")
      (apt-id-eq? elt "mod")
      (apt-id-eq? elt "%")
      (apt-id-eq? elt "**")
      (apt-id-eq? elt "==")
      (apt-id-eq? elt "<>")
      (apt-id-eq? elt "<")
      (apt-id-eq? elt "<=")
      (apt-id-eq? elt ">")
      (apt-id-eq? elt ">=")
      (apt-id-eq? elt "<=>")
      (apt-id-eq? elt "&")
      (apt-id-eq? elt "|")
      (apt-id-eq? elt "^")
      (apt-id-eq? elt "and")
      (apt-id-eq? elt "or")
      (apt-id-eq? elt "->")
      (apt-id-eq? elt "..")
      (apt-id-eq? elt "by")
      (apt-id-eq? elt "...")
      (apt-id-eq? elt "<<")
      (apt-id-eq? elt ">>")
      (apt-id-eq? elt "in")
      (apt-id-eq? elt ".")))


(define (parse-binary-2p left op right)
  (make-object <apt:binary> (list left op right)))


(define (parse-assign-2p lvalue rvalue)
  (make-object <apt:assign> (list lvalue rvalue)))


(define (parse-funcall-2p func args)
  (let ((parsed-args (let loop ((res '())
                                (nl args))
                       (if (null? nl)
                           res
                           (cond ((apt-punct-eq? (car nl) ",")
                                  (loop res (cdr nl)))

;;                                 ((apt-id-keyarg? (car nl))
;;                                  (loop (append res
;;                                                (list (make-object <apt:const>
;;                                                                   (list 'keyw
;;                                                                         (apt-id-value (car nl))))))))
                                 (else (loop (append res
                                                     (list (parse-expr-2p
                                                            (car nl))))
                                             (cdr nl))))) )))
    (make-object <apt:apply> (list func parsed-args))))


(define (parse-if-2p token-list)
  (if (not (null? token-list))
      (if (apt-nested? (car token-list))
          (let ((test-list (apt-nested-body (car token-list))))
            (if (and test-list
                     (>= (length test-list) 1))
                (let ((test (parse-expr-2p (car test-list)))
                      (nl (cdr token-list)))
                  (if (not (null? nl))
                      (let ((consequent (parse-expr-2p (car nl))))
                        (set! nl (cdr nl))
                        (if (not (null? nl))
                            (if (apt-id-eq? (car nl) "else")
                                (begin
                                  (set! nl (cdr nl))
                                  (if (not (null? nl))
                                      (make-object <apt:if>
                                                   (list test
                                                         consequent
                                                         (parse-expr-2p (car nl))))
                                      (syntax-error "if: missing alternate node: "
                                                    token-list)))
                                (syntax-error "if: expected 'else': " token-list))
                            (make-object <apt:if> (list test consequent #f))))
                      (syntax-error "if: missing consequent node: " token-list)))
                (syntax-error "If: Missing test node: " token-list)))
          (syntax-error "If: Missing test node: " token-list))
      (syntax-error "if: missing test node: " token-list)))


(define (parse-range-2p token-list by-expr)
  (let ((from (parse-expr-2p (car token-list)))
        (incl? (apt-id-eq? (cadr token-list) "..."))
        (to (parse-expr-2p (caddr token-list)))
        (nl (cdddr token-list)) )
    (make-object <apt:range> (list from to by-expr incl?))))


(define (parse-range-2p* token-list)
  (if (apt-seq? (car token-list))
      (let ((body (apt-seq-body (car token-list))))
        (if (and (equal? (length body) 3)
                 (or (apt-id-eq? (cadr body) "..")
                     (apt-id-eq? (cadr body) "...")))
            (parse-range-2p body (parse-expr-2p (caddr token-list)))
            (syntax-error "range: lvalue must be range: " body )))
      (syntax-error "range: misused 'by' operator: " token-list)))


(define (parse-keyarg-2p token-list)
  (if (equal? (length token-list) 2)
      (make-object <apt:keyarg> (list (apt-id-value (car token-list))
                                      (parse-expr-2p (cadr token-list))))
      (syntax-error "key argument: missing second argument: " token-list)))


(define (parse-on-2p token-list)
  (arc:display "Parsing 'ON' not done yet" 'nl)
  #f)


(define (parse-seq-2p elt)
  (let* ((token-list (apt-seq-body elt))
         (node (car token-list)))
    (cond
     ((apt-id-keyarg? node) (parse-keyarg-2p token-list))
     ((apt-id-eq? node "def") (parse-def-2p (cdr token-list) 'global))
     ((apt-id-eq? node "let") (parse-def-2p (cdr token-list) 'local))
     ((apt-id-eq? node "namespace") (parse-namespace-2p (cdr token-list)))
     ((apt-id-eq? node "if") (parse-if-2p (cdr token-list)))
     ((apt-id-eq? node "on") (parse-on-2p (cdr token-list)))

     ((apt-punct-eq? node "#function") (parse-function-2p (cdr token-list)
                                                          #f #f 'anon))

     (else (if (not (null? (cdr token-list)))
               (cond ((parse-operator-2p? (cadr token-list))
                      (if (not (null? (cddr token-list)))
                          (cond
                           ((apt-punct-eq? (cadr token-list) "=")
                            (parse-assign-2p (parse-expr-2p node)
                                             (parse-expr-2p (caddr token-list))))
                           ((apt-id-eq? (cadr token-list) "..")
                            (parse-range-2p token-list #f))
                           ((apt-id-eq? (cadr token-list) "...")
                            (parse-range-2p token-list #f))
                           ((apt-id-eq? (cadr token-list) "by")
                            (parse-range-2p* token-list))
                           (else
                            (parse-binary-2p (parse-expr-2p node)
                                             (apt-id-value (cadr token-list))
                                             (parse-expr-2p (caddr token-list)))))
                          (syntax-error "Required a right hand operator: " token-list)))
                     ((apt-nested? (cadr token-list))
                      (parse-funcall-2p (parse-expr-2p node)
                                        (apt-nested-body (cadr token-list))))
                     (else (syntax-error "Unexpected second node: " token-list)) )
               (parse-expr-2p node))) )))


(define (parse-block-2p token-list)
  (if (equal? (length token-list) 1)
      (parse-expr-2p (car token-list))
      (let ((parsed-exprs (map (lambda (e)
                                 (parse-expr-2p e))
                               token-list)))
        (make-object <apt:block> (list parsed-exprs)))))


(define (parse-next-top-2p expr-tree)
  (let loop ((res ())
             (nl expr-tree))
    (if (null? nl)
        res
        (let* ((elt (car nl))
               (expr3 (parse-expr-2p elt)))
          (if expr3
              (->xml expr3))
          (if expr3
              (loop (append res (list expr3)) (cdr nl))
              (loop res (cdr nl)))))))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
