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

(define (apt? apt)
  (and (list? apt)
       (>= (length apt) 1)
       (symbol? (car apt))))


(define (apt-append-list dst src)
  (append dst (hea:reduce (lambda (o nl)
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


(define (apt-lit value type)
  (list 'lit value type))


(define (apt-lit? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) 'lit)))


(define (apt-lit-value expr)
  (cadr expr))


(define (apt-lit-type expr)
  (caddr expr))


(define (apt-lit-keyword? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'keyw)))


(define (apt-lit-string? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'str)))


(define (apt-lit-int? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'int)))


(define (apt-lit-char? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'char)))


(define (apt-lit-bool? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'bool)))


(define (apt-lit-nil? expr)
  (and (list? expr)
       (eq? (apt-lit-type expr) 'nil)))


(define (apt-lit-eof? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'eof)))


(define (apt-lit-imaginary? expr)
  (and (apt-lit? expr)
       (eq? (apt-lit-type expr) 'imaginary)))


(define (apt-id id)
  (list 'id id #f))


(define (apt-id* id ns)
  (list 'id id ns))


(define (apt-id? id)
  (and (list? id)
       (>= (length id) 2)
       (eq? (car id) 'id)))


(define (apt-id-value id)
  (cadr id))


(define (apt-id-ns id)
  (caddr id))


(define (apt-id-set-ns! id ns)
  (set-car! (cddr id) ns))


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


(define (apt-symbol value)
  (let* ((ns-pos (if (char=? (string-ref value 0) #\<)
                     (string-find value #\> 1)
                     #f))
         (ns (if ns-pos
                 (substring value 1 ns-pos)
                 #f))
         (sym (if ns-pos
                  (substring value (+ ns-pos 1) (string-length value))
                  value)))
    (if ns
        (apt-id* sym ns)
        (apt-id sym))))


(define (qualified-id* id fallback-ns)
  (let* ((value (apt-id-value id))
         (ns-pos (if (char=? (string-ref value 0) #\<)
                     (string-find value #\> 1)
                     #f))
         (ns (if ns-pos
                 (substring value 1 ns-pos)
                 #f))
         (name (if ns-pos
                   (substring value (+ ns-pos 1) (string-length value))
                   value)))
    (if ns
        (cons name ns)
        (cons name fallback-ns))))


(define (qualified-id id)
  (qualified-id* id ""))


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
  (apt-append-list (list 'nested left right)
                   (if contained contained '())))


(define (apt-nested? expr)
  (and (list? expr)
       (>= (length expr) 3)
       (eq? (car expr) 'nested)))


(define (apt-nested-body expr)
  (cdddr expr))


(define (apt-nested-left expr)
  (cadr expr))


(define (apt-nested-left? expr c)
  (and (apt-nested? expr)
       (equal? (apt-nested-left expr) c)))


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


(define (apt-class type sym params isatype decls)
  (let* ((res #f))
    (set! res (list (apt-id "def") (apt-id type) sym))
    (if params
        (set! res (append res (list (apt-nested* "(" ")" params)))))
    (if isatype
        (set! res (append res (list (apt-punct ":")
                                    isatype))))
    (if decls
        (set! res (append res (list (apt-nested* "{" "}" decls)))))

    (apt-seq* res)))



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
