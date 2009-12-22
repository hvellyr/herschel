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
(define-generic (debug-slot->xml tag slotnm))


(define (->xml x)
  (if (vector? x)
      (debug->xml x)
      (arc:display #f)))


(define-method (debug-slot->xml <apt:node> tag slotnm)
  (call-next-method)
  (let ((slot (slot-ref self slotnm)))
    (cond ((and (list? slot)
                (> (length slot) 0))
           (begin
             (arc:display "<" tag ">")
             (for-each (lambda (p)
                         (->xml p))
                       slot)
             (arc:display "</" tag ">")))
          ((vector? slot) (begin
                            (arc:display "<" tag ">")
                            (->xml slot)
                            (arc:display "</" tag ">")))
          )))



;;;---------------------------------------------------------------------------

(define-class <apt:namespace> (<apt:node>) (ns id))

(define-method (initialise <apt:namespace> args)
  (call-next-method)
  (slot-set! self 'ns (list-ref args 0))
  (slot-set! self 'id (list-ref args 1))
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
  (slot-set! self 'sym (list-ref args 0))
  (slot-set! self 'type (list-ref args 1))
  (slot-set! self 'init-value (list-ref args 2))
  (slot-set! self 'const? (list-ref args 3))
  (slot-set! self 'fluid? (list-ref args 4))
  self)

(define-method (debug->xml <apt:vardef>)
  (call-next-method)
  (arc:display "<def><var scope='global' name='" (slot-ref self 'sym) "'>" 'nl)
  (debug-slot->xml self "type" 'type)
  (debug-slot->xml self "init-value" 'init-value)
  (arc:display "</var></def>" 'nl))


;;;---------------------------------------------------------------------------

(define-class <apt:def> (<apt:node>) (scope sym func))

(define-method (initialise <apt:def> args)
  (call-next-method)
  (slot-set! self 'scope  (list-ref args 0))
  (slot-set! self 'sym    (list-ref args 1))
  (slot-set! self 'func   (list-ref args 2))
  self)

(define-method (debug->xml <apt:def>)
  (call-next-method)
  (arc:display "<def scope='" (slot-ref self 'scope) "' "
               "name='" (slot-ref self 'sym) "'>")
  (->xml (slot-ref self 'func))
  (arc:display "</def>" 'nl))


;;;---------------------------------------------------------------------------

(define-class <apt:function> (<apt:node>) (retval params body meth?
                                                  abstract?))

(define-method (initialise <apt:function> args)
  (call-next-method)
  (slot-set! self 'retval    (list-ref args 0))
  (slot-set! self 'params    (list-ref args 1))
  (slot-set! self 'body      (list-ref args 2))
  (slot-set! self 'meth?     (list-ref args 3))
  (slot-set! self 'abstract? (list-ref args 4))
  self)

(define-method (debug->xml <apt:function>)
  (call-next-method)
  (if (slot-ref self 'meth?)
      (begin
        (arc:display "<meth")
        (if (slot-ref self 'abstract?)
            (arc:display " abstract='true'"))
        (arc:display ">"))
      (arc:display "<func>"))
  (debug-slot->xml self "params" 'params)
  (debug-slot->xml self "retval" 'retval)
  (debug-slot->xml self "body" 'body)
  (if (slot-ref self 'meth?)
      (arc:display "</meth>")
      (arc:display "</func>")))


;;;---------------------------------------------------------------------------

(define-class <apt:param> (<apt:node>) (keyarg sym flag type init-value
                                               specialized?))

(define-method (initialise <apt:param> args)
  (call-next-method)
  (slot-set! self 'keyarg       (list-ref args 0))
  (slot-set! self 'sym          (list-ref args 1))
  (slot-set! self 'flag         (list-ref args 2))
  (slot-set! self 'type         (list-ref args 3))
  (slot-set! self 'init-value   (list-ref args 4))
  (slot-set! self 'specialized? (list-ref args 5))
  self)

(define-method (debug->xml <apt:param>)
  (call-next-method)
  (arc:display "<prm")
  (if (slot-ref self 'keyarg)
      (arc:display " key='" (slot-ref self 'keyarg) "'"))
  (if (eq? (slot-ref self 'flag) 'rest)
      (arc:display " what='rest'"))
  (if (slot-ref self 'specialized?)
      (arc:display " specialized='true'"))
  (arc:display " name='" (slot-ref self 'sym) "'>")
  (debug-slot->xml self "type" 'type)
  (debug-slot->xml self "init-value" 'init-value)
  (arc:display "</prm>"))


;;;---------------------------------------------------------------------------

(define-class <apt:const> (<apt:node>) (type value))

(define-method (initialise <apt:const> args)
  (call-next-method)
  (slot-set! self 'type (list-ref args 0))
  (slot-set! self 'value (list-ref args 1))
  self)

(define-method (debug->xml <apt:const>)
  (call-next-method)
  (case (slot-ref self 'type)
    ((str)    (arc:display "<str>"  (slot-ref self 'value) "</str>"))
    ((int)    (arc:display "<int>"  (slot-ref self 'value) "</int>"))
    ((keyw)   (arc:display "<keyw>" (slot-ref self 'value) "</keyw>"))
    ((char)   (arc:display "<chr>"  (slot-ref self 'value) "</chr>"))
    ((bool)   (arc:display "<bool>" (slot-ref self 'value) "</bool>"))
    ((nil)    (arc:display "<nil/>"))
    ((eof)    (arc:display "<eof/>"))
    (else (arc:display "<unknown/>"))))


;;;---------------------------------------------------------------------------

(define-class <apt:const-container> (<apt:const>) ())

(define-method (initialise <apt:const-container> args)
  (call-next-method)
  self)

(define-method (debug->xml <apt:const-container>)
  (let ((tag (case (slot-ref self 'type)
               ((array)  "array")
               ((vector) "vector")
               ((dict)   "dict")
               (else (syntax-error "What's this?" self)))))
    (arc:display "<" tag ">")
    (for-each (lambda (e)
                (->xml e))
              (slot-ref self 'value))
    (arc:display "</" tag ">")))


;;;---------------------------------------------------------------------------

(define-class <apt:array> (<apt:const-container>) ())

(define-method (initialise <apt:array> args)
  (call-next-method)
  self)


;;;---------------------------------------------------------------------------

(define-class <apt:vector> (<apt:const-container>) ())

(define-method (initialise <apt:vector> args)
  (call-next-method)
  self)


;;;---------------------------------------------------------------------------

(define-class <apt:dictionary> (<apt:const-container>) ())

(define-method (initialise <apt:dictionary> args)
  (call-next-method)
  self)


;;;---------------------------------------------------------------------------

(define-class <apt:symbol> (<apt:node>) (id))

(define-method (initialise <apt:symbol> args)
  (call-next-method)
  (slot-set! self 'id (list-ref args 0))
  self)

(define-method (debug->xml <apt:symbol>)
  (call-next-method)
  (arc:display "<symbol id='" (slot-ref self 'id) "'/>"))


;;;---------------------------------------------------------------------------

(define-class <apt:binary> (<apt:node>) (left op right))

(define-generic (operator))

(define-method (initialise <apt:binary> args)
  (call-next-method)
  (slot-set! self 'left  (list-ref args 0))
  (slot-set! self 'op    (list-ref args 1))
  (slot-set! self 'right (list-ref args 2))
  self)

(define-method (debug->xml <apt:binary>)
  (call-next-method)
  (arc:display "<bin op='" (slot-ref self 'op) "'>")
  (->xml (slot-ref self 'left))
  (->xml (slot-ref self 'right))
  (arc:display "</bin>"))

(define-method (operator <apt:binary>)
  (slot-ref self 'op))


;;;---------------------------------------------------------------------------

(define-class <apt:assign> (<apt:node>) (lvalue rvalue))

(define-method (initialise <apt:assign> args)
  (call-next-method)
  (slot-set! self 'lvalue (list-ref args 0))
  (slot-set! self 'rvalue (list-ref args 1))
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
  (slot-set! self 'func (list-ref args 0))
  (slot-set! self 'args (list-ref args 1))
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
  (slot-set! self 'exprs (list-ref args 0))
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
  (slot-set! self 'test  (list-ref args 0))
  (slot-set! self 'true  (list-ref args 1))
  (slot-set! self 'false (list-ref args 2))
  self)

(define-method (debug->xml <apt:if>)
  (call-next-method)
  (arc:display "<if><test>")
  (->xml (slot-ref self 'test))
  (arc:display "</test><then>")
  (->xml (slot-ref self 'true))
  (arc:display "</then>")
  (debug-slot->xml self "else" 'false)
  (arc:display "</if>"))


;;;---------------------------------------------------------------------------

(define-class <apt:range> (<apt:node>) (from to by incl?))

(define-method (initialise <apt:range> args)
  (call-next-method)
  (slot-set! self 'from  (list-ref args 0))
  (slot-set! self 'to    (list-ref args 1))
  (slot-set! self 'by    (list-ref args 2))
  (slot-set! self 'incl? (list-ref args 3))
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
  (debug-slot->xml self "by" 'by)
  (arc:display "</range>"))


;;;---------------------------------------------------------------------------

(define-class <apt:keyarg> (<apt:node>) (key value))

(define-method (initialise <apt:keyarg> args)
  (call-next-method)
  (slot-set! self 'key (list-ref args 0))
  (slot-set! self 'value (list-ref args 1))
  self)

(define-method (debug->xml <apt:keyarg>)
  (call-next-method)
  (arc:display "<key-arg key='" (slot-ref self 'key) "'>")
  (->xml (slot-ref self 'value))
  (arc:display "</key-arg>"))



;;;---------------------------------------------------------------------------
;;;---------------------------------------------------------------------------

(define-class <apt:type> (<apt:node>) ())

(define-method (initialise <apt:type> args)
  (call-next-method)
  self)


;;;---------------------------------------------------------------------------

(define-class <apt:simple-type> (<apt:type>) (sym))

(define-method (initialise <apt:simple-type> args)
  (call-next-method)
  (slot-set! self 'sym (list-ref args 0))
  self)

(define-method (debug->xml <apt:simple-type>)
  (call-next-method)
  (arc:display "<simple-type sym='" (slot-ref self 'sym) "'/>"))


;;;---------------------------------------------------------------------------

(define-class <apt:array-type> (<apt:type>) (base initial-size))

(define-method (initialise <apt:array-type> args)
  (call-next-method)
  (slot-set! self 'base (list-ref args 0))
  (slot-set! self 'initial-size (list-ref args 1))
  self)

(define-method (debug->xml <apt:array-type>)
  (call-next-method)
  (arc:display "<array-type>")
  (->xml (slot-ref self 'base))
  (debug-slot->xml self "initial" 'initial-size)
  (arc:display "</array-type>"))


;;;---------------------------------------------------------------------------

(define-class <apt:union-type> (<apt:type>) (type-list))

(define-method (initialise <apt:union-type> args)
  (call-next-method)
  (slot-set! self 'type-list (list-ref args 0))
  self)

(define-method (debug->xml <apt:union-type>)
  (call-next-method)
  (arc:display "<union-type>")
  (for-each (lambda (e)
              (->xml e))
            (slot-ref self 'type-list))
  (arc:display "</union-type>"))


;;;---------------------------------------------------------------------------

(define-class <apt:constraint-type> (<apt:type>) (base constraint))

(define-method (initialise <apt:constraint-type> args)
  (call-next-method)
  (slot-set! self 'base (list-ref args 0))
  (slot-set! self 'constraint (list-ref args 1))
  self)

(define-method (debug->xml <apt:constraint-type>)
  (call-next-method)
  (arc:display "<constraint-type>")
  (->xml (slot-ref self 'base))
  (debug-slot->xml self "constraint" 'constraint)
  (arc:display "</constraint-type>"))


;;;---------------------------------------------------------------------------

(define-class <apt:param-type> (<apt:type>) (base params))

(define-method (initialise <apt:param-type> args)
  (call-next-method)
  (slot-set! self 'base (list-ref args 0))
  (slot-set! self 'params (list-ref args 1))
  self)

(define-method (debug->xml <apt:param-type>)
  (call-next-method)
  (arc:display "<param-type>")
  (->xml (slot-ref self 'base))
  (debug-slot->xml self "params" 'params)
  (arc:display "</param-type>"))



;;;---------------------------------------------------------------------------

(define-class <apt:base-typedef> (<apt:node>) (sym params derives))

(define-method (initialise <apt:base-typedef> args)
  (call-next-method)
  (slot-set! self 'sym     (list-ref args 0))
  (slot-set! self 'params  (list-ref args 1))
  (slot-set! self 'derives (list-ref args 2))
  self)


;;;---------------------------------------------------------------------------

(define-class <apt:typedef> (<apt:base-typedef>) ())

(define-method (initialise <apt:typedef> args)
  (call-next-method)
  self)

(define-method (debug->xml <apt:typedef>)
  (call-next-method)
  (arc:display "<def><type name='" (slot-ref self 'sym) "'>")
  (debug-slot->xml self "params" 'params)
  (debug-slot->xml self "derives" 'derives)
  (arc:display "</type></def>"))


;;;---------------------------------------------------------------------------

(define-class <apt:classdef> (<apt:base-typedef>) (decls))

(define-method (initialise <apt:classdef> args)
  (call-next-method)
  (slot-set! self 'decls   (list-ref args 3))
  self)

(define-method (debug->xml <apt:classdef>)
  (call-next-method)
  (arc:display "<def><class name='" (slot-ref self 'sym) "'>")
  (debug-slot->xml self "params" 'params)
  (debug-slot->xml self "derives" 'derives)
  (debug-slot->xml self "decls" 'decls)
  (arc:display "</class></def>"))


;;;---------------------------------------------------------------------------

(define-class <apt:type-param> (<apt:node>) (keyarg sym default))

(define-method (initialise <apt:type-param> args)
  (call-next-method)
  (slot-set! self 'sym     (list-ref args 0))
  (slot-set! self 'default (list-ref args 1))
  self)

(define-method (debug->xml <apt:type-param>)
  (call-next-method)
  (arc:display "<type-prm name='" (slot-ref self 'sym) "'>")
  (debug-slot->xml self "default-type" 'default)
  (arc:display "</type-prm>"))


;;;---------------------------------------------------------------------------

(define-class <apt:slotdef> (<apt:node>) (sym type props init))

(define-method (initialise <apt:slotdef> args)
  (call-next-method)
  (slot-set! self 'sym   (list-ref args 0))
  (slot-set! self 'type  (list-ref args 1))
  (slot-set! self 'props (list-ref args 2))
  (slot-set! self 'init  (list-ref args 3))
  self)

(define-method (debug->xml <apt:slotdef>)
  (call-next-method)
  (arc:display "<slot name='" (slot-ref self 'sym) "'>")
  (debug-slot->xml self "type" 'type)
  (debug-slot->xml self "props" 'props)
  (debug-slot->xml self "init-value" 'init)
  (arc:display "</slot>"))


;;;---------------------------------------------------------------------------

(define-class <apt:slot-init-prop> (<apt:node>) (type value))

(define-method (initialise <apt:slot-init-prop> args)
  (call-next-method)
  (slot-set! self 'type  (list-ref args 0))
  (slot-set! self 'value (list-ref args 1))
  self)

(define-method (debug->xml <apt:slot-init-prop>)
  (call-next-method)
  (arc:display "<slot-init type='" (slot-ref self 'type) "'>")
  (debug-slot->xml self "value" 'value)
  (arc:display "</slot-init>"))


;;;---------------------------------------------------------------------------

(define-class <apt:slot-prop> (<apt:node>) (value))

(define-method (initialise <apt:slot-prop> args)
  (call-next-method)
  (slot-set! self 'value (list-ref args 0))
  self)

(define-method (debug->xml <apt:slot-prop>)
  (call-next-method)
  (arc:display "<slot-prop value='" (slot-ref self 'value) "'/>"))


;;;---------------------------------------------------------------------------

(define-class <apt:on> (<apt:node>) (key params expr))

(define-method (initialise <apt:on> args)
  (call-next-method)
  (slot-set! self 'key (list-ref args 0))
  (slot-set! self 'params (list-ref args 1))
  (slot-set! self 'expr (list-ref args 2))
  self)

(define-method (debug->xml <apt:on>)
  (call-next-method)
  (arc:display "<on key='" (slot-ref self 'key) "'>")
  (debug-slot->xml self "params" 'params)
  (debug-slot->xml self "body" 'expr)
  (arc:display "</on>"))


;;;---------------------------------------------------------------------------

(define-class <apt:alias> (<apt:node>) (key type-equiv))

(define-method (initialise <apt:alias> args)
  (call-next-method)
  (slot-set! self 'type-equiv (list-ref args 0))
  self)

(define-method (debug->xml <apt:alias>)
  (call-next-method)
  (debug-slot->xml self "alias" 'type-equiv))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
