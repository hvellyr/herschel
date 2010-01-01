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

(define-generic (next-token))
(define-generic (fetch-next-token))
(define-generic (unread-token token))
(define-generic (current-token))
(define-generic (current-token-set! token))
(define-generic (push-port port))
(define-generic (pop-port))
(define-generic (current-port))


;;----------------------------------------------------------------------

(define-class <hea:token-port> (<class>) ())


(define-method (initialise <hea:token-port> args)
  (call-next-method)
  self)


(define-method (next-token <hea:token-port>)
  'EOF)


;;----------------------------------------------------------------------

(define-class <hea:parse-port> (<class>) (port
                                          port-stack
                                          current-token unread-stack))


(define-method (initialise <hea:parse-port> args)
  (call-next-method)
  (slot-set! self 'port (list-ref args 0))
  (slot-set! self 'port-stack '())
  (slot-set! self 'current-token '())
  (slot-set! self 'unread-stack '())
  self)


(define-method (push-port <hea:parse-port> port)
  (slot-set! self 'port-stack (cons (slot-ref self 'port)
                                    (slot-ref self 'port-stack)))
  (slot-set! self 'port port))


(define-method (pop-port <hea:parse-port>)
  (let ((ps (slot-ref self 'port-stack))
        (old-port (slot-ref self 'port)))
    (slot-set! self 'port (car ps))
    (slot-set! self 'port-stack (cdr ps))
    old-port))


(define-method (current-token <hea:parse-port>)
  (slot-ref self 'current-token))


(define-method (current-token-set! <hea:parse-port> token)
  (slot-set! self 'current-token token))


(define-method (unread-token <hea:parse-port> token)
  (slot-set! self 'unread-stack
             (cons token (slot-ref self 'unread-stack))))


(define-method (next-token <hea:parse-port>)
  (let* ((urs (slot-ref self 'unread-stack))
         (token (if (not (null? urs))
                    (let ((val (car urs)))
                      (slot-set! self 'unread-stack (cdr urs))
                      val)
                    (next-token (slot-ref self 'port)))))
    (slot-set! self 'current-token token)
    token))


;;----------------------------------------------------------------------

(define-class <hea:parse-context> (<class>) (macro-registry
                                             port
                                             port-stack
                                             ns-mappings
                                             current-namespace))

(define-method (initialise <hea:parse-context> args)
  (call-next-method)
  (slot-set! self 'macro-registry '())
  (slot-set! self 'port #f)
  (slot-set! self 'port-stack '())
  (slot-set! self 'ns-mappings '())
  (slot-set! self 'current-namespace "")
  self)

(define-generic (macro-registry))
(define-generic (register-macro type name ns macro))
(define-generic (lookup-macro qual-name))
(define-generic (lookup-macro-type qual-name))


(define-method (macro-registry <hea:parse-context>)
  (slot-ref self 'macro-registry))


(define (macro-assoc pattern reg)
  (let macasc-loop ((nl reg)
                    (res '()))
    ;;(arc:display "MACRO=? " pattern " ->? " nl 'nl)
    (if (null? nl)
        (cond ((= (length res) 1) (begin ;;(arc:display "XXX " (cddr (car res)) 'nl)
                                         (car res)))
              ((= (length res) 0) #f)
              (else (begin
                      (arc:display "Ambigous macro name usage: " (car pattern) 'nl)
                      #f)))
        (begin
          ;;(arc:display "          ->? " (caar nl) " --- " (caaar nl) 'nl)
          (if (equal? (car pattern) (caaar nl))
              (begin
                ;;(arc:display "      x55 " (cdr pattern) 'nl)
                (cond ((equal? (cdr pattern) '*)
                       (macasc-loop (cdr nl) (append res (list (car nl)))))
                      ((equal? (cdr pattern) (cdaar nl))
                       (car nl))
                      (else (macasc-loop (cdr nl) res))))
              (macasc-loop (cdr nl) res))))))


(define-method (register-macro <hea:parse-context> type name ns macro)
  (let* ((macro-id (cons name ns))
         (mp (macro-assoc macro-id (slot-ref self 'macro-registry))))
    (if mp
        (set-cdr! mp (cons type macro))
        (slot-set! self 'macro-registry
                   (append (slot-ref self 'macro-registry)
                           (list (cons macro-id (cons type macro))))))))


(define-method (lookup-macro <hea:parse-context> qual-name)
  (let ((mp (macro-assoc qual-name (slot-ref self 'macro-registry))))
    (if mp
        (cddr mp)
        #f)))


(define-method (lookup-macro-type <hea:parse-context> qual-name)
  (let ((mp (macro-assoc qual-name (slot-ref self 'macro-registry))))
    (if mp
        (cadr mp)
        #f)))


;;;----------------------------------------------------------------------

(define-generic (register-namespace-mapping abbrv ns))
(define-generic (set-current-namespace! ns))
(define-generic (current-namespace))
(define-generic (lookup-namespace abbrv))

(define-method (register-namespace-mapping <hea:parse-context> abbrv ns)
  (let* ((nsp (assoc abbrv (slot-ref self 'ns-mappings))))
    (if nsp
        (set-cdr! nsp ns)
        (slot-set! self 'ns-mappings
                   (append (slot-ref self 'ns-mappings)
                           (list (cons abbrv ns)))))))


(define-method (set-current-namespace! <hea:parse-context> ns)
  (arc:display "Current namespace: " ns 'nl)
  (slot-set! self 'current-namespace ns))


(define-method (current-namespace <hea:parse-context>)
  (slot-ref self 'current-namespace))


(define-method (lookup-namespace <hea:parse-context> abbrv)
  (let ((nsp (assoc abbrv (slot-ref self 'ns-mappings))))
    (if nsp
        (cdr nsp)
        #f)))


(define (qualified-id-for-lookup ctx id)
  (arc:display "Namespaces: " (slot-ref ctx 'ns-mappings) 'nl)
  (let ((qual-id (qualified-id* id '*)))
    (if (not (eq? (cdr qual-id) '*))
        (let ((ns (lookup-namespace ctx (cdr qual-id))))
          (arc:display "((" (cdr qual-id) " -- " ns "))" 'nl)
          (cons (car qual-id) ns))
        qual-id)))


;;;----------------------------------------------------------------------

(define-method (push-port <hea:parse-context> port)
  (slot-set! self 'port-stack (cons (slot-ref self 'port)
                                    (slot-ref self 'port-stack)))
  (slot-set! self 'port port))


(define-method (pop-port <hea:parse-context>)
  (let ((ps (slot-ref self 'port-stack))
        (old-port (slot-ref self 'port)))
    (slot-set! self 'port (car ps))
    (slot-set! self 'port-stack (cdr ps))
    old-port))


;;;----------------------------------------------------------------------

(define-method (current-token <hea:parse-context>)
  (let ((p (slot-ref self 'port)))
    (if p
        (current-token p)
        'EOF)))


(define-method (current-token-set! <hea:parse-context> token)
  (let ((p (slot-ref self 'port)))
    (if p
        (current-token-set! p token)
        #f)))


(define-method (next-token <hea:parse-context>)
  (let ((p (slot-ref self 'port)))
    (if p
        (next-token p)
        'EOF)))


(define-method (unread-token <hea:parse-context> token)
  (let ((p (slot-ref self 'port)))
    (if p
        (unread-token p token)
        #f)))


(define-method (current-port <hea:parse-context>)
  (slot-ref self 'port))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
