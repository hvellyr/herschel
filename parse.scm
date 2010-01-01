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

(load "../apt.scm")
(load "../cmpmcr.scm")
(load "../misc.scm")
(load "../oop.scm")
(load "../parctx.scm")
(load "../tokenizer.scm")
(load "../ast.scm")

(load "../parse1.scm")
(load "../parse2.scm")


(define (parse-file filename)
  (let* ((file-port (open-input-file filename))
         (ftport (make-object <hea:file-token-port> (list file-port)))
         (port (make-object <hea:parse-port> (list ftport)))
         (ctx (make-object <hea:parse-context> '()))
         (expr1-tree (begin
                       (next-token port)
                       (push-port ctx port)
                       (parse-next-top ctx)))
         (expr2-tree (if expr1-tree 
                         (parse-next-top-2p expr1-tree)
                         #f)))
    (close-input-port file-port)

    (if (hea:verbose)
        (begin
          (arc:display 'nl 'nl)
          (arc:display "------- macros ------------------------" 'nl)
          (for-each (lambda (m)
                      (arc:display m 'nl))
                    (macro-registry ctx))
          (arc:display "---------------------------------------" 'nl)))
    expr1-tree))


;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
