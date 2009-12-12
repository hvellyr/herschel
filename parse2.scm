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


(define (parse-funcdef-2p token-list meth?)
  (if (apt-id? (car token-list))
      (let* ((sym (apt-id-value (car token-list)))
             (retval #f)
             (params '())
             (body #f))
        (let ((nl token-list))
          (if (not (null? (cdr nl)))
              (begin
                (set! nl (cdr nl))
                (if (apt-nested? (car nl))
                    (set! params (apt-nested-body (car nl)))
                    (syntax-error "Bad node.  Nested expected: " nl))
                (if (not (null? (cdr nl)))
                    (begin
                      (set! nl (cdr nl))
                      (if (apt-punct-eq? (car nl) ":")
                          (if (not (null? (cdr nl)))
                              (begin
                                (set! retval (cadr nl))
                                (set! nl (cddr nl)))))
                      (set! body (parse-expr-2p (car nl)))
                      (make-object <apt:funcdef> (list sym retval params
                                                       body meth?)))))
              (syntax-error "Bad node: " token-list))))
      (syntax-error "Bad node. Symbol expected: " token-list)))


(define (parse-methdef-2p token-list)
  (parse-funcdef-2p token-list #t))


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
                        (set! type (cadr nl))
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


(define (parse-typedef-2p token-list)
  #f)


(define (parse-def-2p token-list scope)
  (let ((node (car token-list)))
    (cond ((apt-id-eq? node "meth")  (parse-methdef-2p (cdr token-list)))
          ((apt-id-eq? node "const") (parse-const-2p (cdr token-list)))
          ((apt-id-eq? node "fluid") (parse-fluid-2p (cdr token-list)))
          ((apt-id-eq? node "type")  (parse-typedef-2p (cdr token-list)))
          ((apt-id-eq? node "alias") (parse-const-2p (cdr token-list)))
          ((apt-id? node)
           (if (not (null? (cdr token-list)))
               (cond ((apt-nested? (cadr token-list))
                      (parse-funcdef-2p token-list #f))
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


(define (parse-seq-2p elt)
  (let* ((token-list (apt-seq-body elt))
         (node (car token-list)))
    (cond 
     ((apt-id-keyarg? node) (parse-keyarg-2p token-list))
     ((apt-id-eq? node "def") (parse-def-2p (cdr token-list) 'global))
     ((apt-id-eq? node "let") (parse-def-2p (cdr token-list) 'local))
     ((apt-id-eq? node "namespace") (parse-namespace-2p (cdr token-list)))
     ((apt-id-eq? node "if") (parse-if-2p (cdr token-list)))

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


(define (parse-nested-2p node)
  (cond ((equal? (apt-nested-left node) "(")
         ;; TODO gck
         ...)
        ((equal? (apt-nested-left node) "{")
         (parse-block-2p (apt-nested-body node)))
        (syntax-error "Unknown nested syntax: " node)))


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
