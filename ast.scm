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

;;;---------------------------------------------------------------------------

(define-class <apt:node> (<class>) ())

(define-generic (debug->xml))

(define (->xml x)
  (if (vector? x)
      (debug->xml x)
      (arc:display #f)))


;;;---------------------------------------------------------------------------

(define-class <apt:namespace> (<apt:node>) (ns id))

(define-method (initialise <apt:namespace> args)
  (call-next-method)
  (slot-set! self 'ns (car args))
  (slot-set! self 'id (cadr args))
  self)

(define-method (debug->xml <apt:namespace>)
  (call-next-method)
  (arc:display "<namespace name='" (slot-ref self 'ns) "'")
  (if (slot-ref self 'id)
      (arc:display " id='" (slot-ref self 'id) "'"))
  (arc:display "/>" 'nl))


;;;---------------------------------------------------------------------------

(define-class <apt:vardef> (<apt:node>) (sym type init-value const? fluid?))

(define-method (initialise <apt:vardef> args)
  (call-next-method)
  (slot-set! self 'sym (car args))
  (slot-set! self 'type (cadr args))
  (slot-set! self 'init-value (caddr args))
  (slot-set! self 'const? (car (cdddr args)))
  (slot-set! self 'fluid? (cadr (cdddr args)))
  self)

(define-method (debug->xml <apt:vardef>)
  (call-next-method)
  (arc:display "<def><var scope='global' name='" (slot-ref self 'sym) "'>" 'nl)
  (arc:display "<type>")
  (->xml (slot-ref self 'type))
  (arc:display "</type>" 'nl)
  (arc:display "<init-value>")
  (->xml (slot-ref self 'init-value))
  (arc:display "</init-value></var></def>" 'nl))


;;;---------------------------------------------------------------------------

(define-class <apt:funcdef> (<apt:node>) (sym retval params body meth?))

(define-method (initialise <apt:funcdef> args)
  (call-next-method)
  (slot-set! self 'sym    (car args))
  (slot-set! self 'retval (cadr args))
  (slot-set! self 'params (caddr args))
  (slot-set! self 'body   (car (cdddr args)))
  (slot-set! self 'meth?  (cadr (cdddr args)))
  self)

(define-method (debug->xml <apt:funcdef>)
  (call-next-method)
  (arc:display "<def>")
  (if (slot-ref self 'meth?)
      (arc:display "<meth name='" (slot-ref self 'sym) "'>")
      (arc:display "<func name='" (slot-ref self 'sym) "'>"))

  (arc:display "<params>")
  (for-each (lambda (p)
              (->xml p))
            (slot-ref self 'params))
  (arc:display "</params>")
  (arc:display "<retval>")
  (->xml (slot-ref self 'retval))
  (arc:display "</retval>")
  (arc:display "<body>")
  (->xml (slot-ref self 'body))
  (arc:display "</body>")
  (if (slot-ref self 'meth?)
      (arc:display "</meth>")
      (arc:display "</func>"))
  (arc:display "</def>" 'nl))


;;;---------------------------------------------------------------------------

(define-class <apt:const> (<apt:node>) (type value))

(define-method (initialise <apt:const> args)
  (call-next-method)
  (slot-set! self 'type (car args))
  (slot-set! self 'value (cadr args))
  self)

(define-method (debug->xml <apt:const>)
  (call-next-method)
  (case (slot-ref self 'type)
    ((str)    (arc:display "<str>"  (slot-ref self 'value) "</str>"))
    ((int)    (arc:display "<int>"  (slot-ref self 'value) "</int>"))
    ((keyw)   (arc:display "<keyw>" (slot-ref self 'value) "</keyw>"))
    ((char)   (arc:display "<chr>"  (slot-ref self 'value) "</chr>"))
    ((bool)   (arc:display "<bool>" (slot-ref self 'value) "</bool>"))
    (else (arc:display "<unknown/>"))))


;;;---------------------------------------------------------------------------

(define-class <apt:symbol> (<apt:node>) (id))

(define-method (initialise <apt:symbol> args)
  (call-next-method)
  (slot-set! self 'id (car args))
  self)

(define-method (debug->xml <apt:symbol>)
  (call-next-method)
  (arc:display "<symbol id='" (slot-ref self 'id) "'/>"))


;;;---------------------------------------------------------------------------

(define-class <apt:binary> (<apt:node>) (left op right))

(define-method (initialise <apt:binary> args)
  (call-next-method)
  (slot-set! self 'left  (car args))
  (slot-set! self 'op    (cadr args))
  (slot-set! self 'right (caddr args))
  self)

(define-method (debug->xml <apt:binary>)
  (call-next-method)
  (arc:display "<bin op='" (slot-ref self 'op) "'>")
  (->xml (slot-ref self 'left))
  (->xml (slot-ref self 'right))
  (arc:display "</bin>"))


;;;---------------------------------------------------------------------------

(define-class <apt:assign> (<apt:node>) (lvalue rvalue))

(define-method (initialise <apt:assign> args)
  (call-next-method)
  (slot-set! self 'lvalue (car args))
  (slot-set! self 'rvalue (cadr args))
  self)

(define-method (debug->xml <apt:assign>)
  (call-next-method)
  (arc:display "<assign>")
  (->xml (slot-ref self 'lvalue))
  (->xml (slot-ref self 'rvalue))
  (arc:display "</assign>"))


;;;---------------------------------------------------------------------------

(define-class <apt:apply> (<apt:node>) (func args))

(define-method (initialise <apt:apply> args)
  (call-next-method)
  (slot-set! self 'func (car args))
  (slot-set! self 'args (cadr args))
  self)

(define-method (debug->xml <apt:apply>)
  (call-next-method)
  (arc:display "<apply>")
  (->xml (slot-ref self 'func))
  (arc:display "<args>")
  (for-each (lambda (a)
              (->xml a))
            (slot-ref self 'args))
  (arc:display "</args></apply>"))


;;;---------------------------------------------------------------------------

(define-class <apt:block> (<apt:node>) (exprs))

(define-method (initialise <apt:block> args)
  (call-next-method)
  (slot-set! self 'exprs (car args))
  self)

(define-method (debug->xml <apt:block>)
  (call-next-method)
  (arc:display "<block>")
  (for-each (lambda (e)
              (->xml e))
            (slot-ref self 'exprs))
  (arc:display "</block>"))


;;;---------------------------------------------------------------------------

(define-class <apt:if> (<apt:node>) (test true false))

(define-method (initialise <apt:if> args)
  (call-next-method)
  (slot-set! self 'test  (car args))
  (slot-set! self 'true  (cadr args))
  (slot-set! self 'false (caddr args))
  self)

(define-method (debug->xml <apt:if>)
  (call-next-method)
  (arc:display "<if><test>")
  (->xml (slot-ref self 'test))
  (arc:display "</test><then>")
  (->xml (slot-ref self 'true))
  (arc:display "</then>")
  (if (slot-ref self 'false)
      (begin
        (arc:display "<else>")
        (->xml (slot-ref self 'false))
        (arc:display "</else>")))
  (arc:display "</if>"))


;;;---------------------------------------------------------------------------

(define-class <apt:range> (<apt:node>) (from to by incl?))

(define-method (initialise <apt:range> args)
  (call-next-method)
  (slot-set! self 'from  (car args))
  (slot-set! self 'to    (cadr args))
  (slot-set! self 'by    (caddr args))
  (slot-set! self 'incl? (cadddr args))
  self)

(define-method (debug->xml <apt:range>)
  (call-next-method)
  (arc:display "<range type='"
               (if (slot-ref self 'incl?) "incl" "excl")
               "'><from>")
  (->xml (slot-ref self 'from))
  (arc:display "</from><to>")
  (->xml (slot-ref self 'to))
  (arc:display "</to>")
  (if (slot-ref self 'by)
      (begin
        (arc:display "<by>")
        (->xml (slot-ref self 'by))
        (arc:display "</by>")))
  (arc:display "</range>"))


;;;---------------------------------------------------------------------------

(define-class <apt:keyarg> (<apt:node>) (key value))

(define-method (initialise <apt:keyarg> args)
  (call-next-method)
  (slot-set! self 'key  (car args))
  (slot-set! self 'value (cadr args))
  self)

(define-method (debug->xml <apt:keyarg>)
  (call-next-method)
  (arc:display "<key-arg key='" (slot-ref self 'key) "'>")
  (->xml (slot-ref self 'value))
  (arc:display "</key-arg>"))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
