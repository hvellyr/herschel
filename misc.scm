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

(define (arc:reduce fn base-value lst)
  (if (null? lst)
      base-value
      (fn (car lst)
          (arc:reduce fn base-value (cdr lst)))))


(define (arc:display . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((nl) (newline))
            (else (display (car v))))
          (loop (cdr v))))))


(define (string-find str c start-idx)
  (let ((len (string-length str)))
    (let loop ((idx start-idx))
      (cond ((>= idx len) #f)
            ((char=? (string-ref str idx) c) idx)
            (else (loop (+ idx 1)))))))


(define (string-split str c)
  (let* ((pos (string-find str c 0)))
    (if pos
        (list (substring str 0 pos)
              (substring str (+ pos 1) (string-length str)))
        #f)))

;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
