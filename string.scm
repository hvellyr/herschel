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

;; splits a string into its tokens divided by a separator
(define (arc:split-string str sep)
  (let* ((res '())
         (strl (string-length str)))
    (let loop ((lp 0) (cp 0))
      (if (>= cp strl)
          (if (> cp lp)
              (set! res (append res (list (substring str lp cp)))))
          (let* ((npos (let iloop ((lp2 cp))
                         (if (>= lp2 strl)
                             strl
                             (if (equal? (string-ref str lp2) sep)
                                 lp2
                                 (iloop (+ lp2 1))))) ) )
            (if (> npos lp)
                (set! res (append res (list (substring str lp npos)))))
            (loop (+ npos 1) (+ npos 1)))
          ))
    res))


;; indicates whether a string is a suffix of another string
(define (arc:string-suffix? str suffix)
  (let* ((strl (string-length str))
         (sfxl (string-length suffix)))
    (and (<= sfxl strl)
         (equal? suffix (substring str
                                   (- strl sfxl)
                                   strl)))))

(define (arc:string-prefix? str prefix)
  (let* ((strl (string-length str))
         (pfxl (string-length prefix)))
    (and (<= pfxl strl)
         (equal? prefix (substring str
                                   0
                                   pfxl)))))


(define (arc:to-str . d)
  (apply string-append (map (lambda (x)
                              (cond
                               ((string? x) x)
                               ((char? x) (string x))
                               ((number? x) (number->string x))
                               ((symbol? x) (symbol->string x))
                               (else "")))
                            d)))

;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
