;;  This file is part of the arc package
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

(define *arc:optind* 0)
(define *arc:optarg* #f)
(define *arc:optopt* #f)

;; get the next option
;;
;; returns #f if no further arguments available; returns #\? if an unknown
;; options is parsed; returns #\: if the options is known, but a value is
;; missing; else returns car of the corresponding table entry.  If no
;; further options are detected and pure value is found, it is returned as
;; string.
;;
;; if cadddr of a table entry is #t, the option requires an value
;; (separated by ws), if it is #f is doesn't
;;
;;
;; the table
;; '((argument "-a" "--argument" #t)
;;   (scheme "-s" "--scheme" #t))
;; 
(define (arc:getopt argv table)
  (if (>= *arc:optind* (length argv))
      #f
      (let* ((arg (list-tail argv *arc:optind*))
             (cmpf (cond
                    ((arc:string-prefix? (car arg) "--") caddr)
                    ((arc:string-prefix? (car arg) "-") cadr)
                    (else #f))) )
        (set! *arc:optind* (+ *arc:optind* 1))
        (set! *arc:optarg* '())
        (set! *arc:optopt* (car arg))
        (if cmpf
            (let loop ((t table))
              (if (null? t)
                  #\?
                  (if (string=? (car arg) (apply cmpf (list (car t))))
                      (if (cadddr (car t))
                          (if (and (cdr t)
                                   (not (arc:string-prefix? (cadr arg) "-")))
                              (begin
                                (set! *arc:optarg* (cadr arg))
                                (set! *arc:optind* (+ *arc:optind* 1))
                                (car (car t)))
                              #\:)
                          (car (car t)))
                      (loop (cdr t)))))
            (car arg)) )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
