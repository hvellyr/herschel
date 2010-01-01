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


(define %arc:argv% (vector->list *args*))

(define %arc:version% "0.1")

(define %hea:verbose% #f)
(define (hea:verbose) %hea:verbose%)


(load "../string.scm")
(load "../getopt.scm")
(load "../misc.scm")
(load "../parse.scm")



;;----------------------------------------------------------------------

;; display a help text
(define (hea:display-help)
  (arc:display
   "arc - a scheme based make and config system" 'nl
   "usage:" 'nl
   "  arc [options] [statement]" 'nl
   'nl
   "Options:" 'nl
   " -v        be verbose" 'nl
   " -h        This help" 'nl
   " -V        Print the version and exit" 'nl
   'nl
   "if no statement is given on the command line arc uses the default statement " 'nl
   "as stated in the defproject statement in the build script used." 'nl
   'nl 'nl))

(define (arc:display-version)
  (arc:display
   "Arc version " %arc:version% 'nl
   "Copyright (C) 2002, 2003 Gregor Klinke" 'nl
   "This is free software; see the source for copying conditions." 'nl
   "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A" 'nl
   "PARTICULAR PURPOSE." 'nl))

(define *list-of-files* '())
(define *working-dir* "")

(define arc:opts '((verbose   "-v" "--verbose" #f)
                   (dir       "-d" "--dir" #t)
                   (help      "-h" "--help" #f)
                   (version   "-V" "--version" #f)))
(let heaopt-loop ((opt (arc:getopt %arc:argv% arc:opts)))
  (if (not opt)
      #f
      (let ((done #f))
        (case opt
          ((verbose) (set! %hea:verbose% #t))
          ((help) (begin
                    (hea:display-help)
                    (set! done #t)))
          ((version) (begin
                       (arc:display-version)
                       (set! done #t)))
          ((dir) (set! *working-dir* *arc:optarg*))
          ((#\?) (arc:display "ERROR: bad option: " *arc:optopt*))
          ((#\:) (begin
                   (arc:display "ERROR: missing arg " *arc:optopt*)))
          (else (begin
                  (set! *list-of-files* (append *list-of-files* (list opt)))) ))
        (if (not done)
            (heaopt-loop (arc:getopt %arc:argv% arc:opts)))) ))



(sys:chdir *working-dir*)

(arc:display *working-dir* 'nl)

(if (> (length *list-of-files*) 0)
    (for-each (lambda (x)
                (arc:display x 'nl))
              (parse-file (list-ref *list-of-files* 0))))



;;Keep this comment at the end of the file
;;Local variables:
;;mode: scheme
;;End:
