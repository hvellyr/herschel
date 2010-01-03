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

(load "../string.scm")


;;----------------------------------------------------------------------

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
  (let pbn-loop ((res '())
             (nl (macro-patterns macr)))
    (if (null? nl)
        res
        (if (equal? (macro-prod-name (car nl)) name)
            (pbn-loop (append res (list (car nl)))
                      (cdr nl))
            (pbn-loop res (cdr nl))))) )


(define (macro-split-macroparam str)
  (let ((res (hea:split-string str #\:)))
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
  (let fn-loop ((nl (vector-ref node 0)))
    (if (null? nl)
        #f
        (if (token=? (caar nl) token)
            (cadar nl)
            (fn-loop (cdr nl))))))


(define (st-has-eof-set? node)
  (let he-loop ((nl (vector-ref node 0)))
    (if (null? nl)
        #f
        (if (eq? (caar nl) 'END)
            #t
            (he-loop (cdr nl))))))


(define (st-find-macro-param node)
  (st-find-node node (apt-macro-param "*:expr")))


(define (st-extract-macro-param node)
  (let ((token (apt-macro-param "*:expr")))
    (let exmp-loop ((nl (vector-ref node 0)))
      (if (null? nl)
          #f
          (if (token=? (caar nl) token)
              (caar nl)
              (exmp-loop (cdr nl)))))))


(define (st-replacement node)
  (vector-ref node 1))


(define (syntax-table-mixin-pattern-part tree src-pattern replc)
  (let mixpp-loop ((res tree)
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
            (mixpp-loop res step (cdr nl))) ))))


(define (syntax-table-mixin-pattern! st name pattern replc)
  (let* ((pattern-tree (syntax-table-find-pattern st name))
         (new-pattern-tree (syntax-table-mixin-pattern-part pattern-tree
                                                            pattern
                                                            replc)))
    (syntax-table-set-pattern! st name new-pattern-tree)))


(define (macro-compile macr)
  (let maccmp-loop ((res (syntax-table-alloc))
                    (nl (macro-patterns macr)))
    (if (null? nl)
        res
        (let* ((prod    (car nl))
               (name    (macro-prod-name prod))
               (pattern (macro-prod-pattern prod))
               (replc   (macro-prod-replacement prod)))
          ;;(display "Compile: ") (display name) (newline)
          (syntax-table-mixin-pattern! res name pattern replc)
          (maccmp-loop res (cdr nl))))))

;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
