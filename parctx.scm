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

(define-class <hea:token-port> (<class>) (current-token unread-stack))

(define-generic (next-token))
(define-generic (fetch-next-token))
(define-generic (unread-token token))
(define-generic (current-token))
(define-generic (current-token-set! token))


(define-method (initialise <hea:token-port> args)
  (call-next-method)
  (slot-set! self 'current-token '())
  (slot-set! self 'unread-stack '())
  self)


(define-method (current-token <hea:token-port>)
  (slot-ref self 'current-token))


(define-method (current-token-set! <hea:token-port> token)
  (slot-set! self 'current-token token))


(define-method (unread-token <hea:token-port> token)
  (slot-set! self 'unread-stack
             (cons token (slot-ref self 'unread-stack))))


(define-method (next-token <hea:token-port>)
  (let* ((urs (slot-ref self 'unread-stack))
         (token (if (not (null? urs))
                    (let ((val (car urs)))
                      (slot-set! self 'unread-stack (cdr urs))
                      val)
                    (fetch-next-token self))))
    (slot-set! self 'current-token token)
    token))


;;----------------------------------------------------------------------

(define-class <parse-context> (<class>) (macro-registry port
                                                        unread-stack))

(define-method (initialise <parse-context> args)
  (call-next-method)
  (slot-set! self 'macro-registry '())
  (slot-set! self 'port #f)
  self)

(define-generic (macro-registry))
(define-generic (register-macro type name macro))
(define-generic (lookup-macro name))
(define-generic (lookup-macro-type name))

(define-method (macro-registry <parse-context>)
  (slot-ref self 'macro-registry))

(define-method (register-macro <parse-context> type name macro)
  (let* ((mp (assoc name (slot-ref self 'macro-registry))))
    (if mp
        (set-cdr! mp (cons type macro))
        (slot-set! self 'macro-registry
                   (append (slot-ref self 'macro-registry
                           (list (cons name (cons type macro)))))))))
  

(define-method (lookup-macro <parse-context> name)
  (let ((mp (assoc name (slot-ref self 'macro-registry))))
    (if mp
        (cddr mp)
        #f)))


(define-method (lookup-macro-type <parse-context> name)
  (let ((mp (assoc name (slot-ref self 'macro-registry))))
    (if mp
        (cadr mp)
        #f)))


;;;----------------------------------------------------------------------

(define-method (current-token <parse-context>)
  (let ((p (slot-ref self 'port)))
    (if p
        (current-token p)
        'EOF)))


(define-method (current-token-set! <parse-context> token)
  (let ((p (slot-ref self 'port)))
    (if p
        (current-token-set! p token)
        #f)))


(define-method (next-token <parse-context>)
  (let ((p (slot-ref self 'port)))
    (if p
        (next-token p)
        'EOF)))


(define-method (unread-token <parse-context> token)
  (let ((p (slot-ref self 'port)))
    (if p
        (unread-token p token)
        #f)))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
