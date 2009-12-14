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

(define-class <parse-context> (<class>) (macro-registry))

(define-method (initialise <parse-context> args)
  (call-next-method)
  (slot-set! self 'macro-registry '())
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



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
