;;; heather-mode.el --- major mode for editing heather files and modules.
;;
;; copyright (c) 2003, 2007, 2008, 2009 Gregor Klinke
;;
;; Some parts of this mode are based on the vera-mode by Reto Zimmermann,
;; Synopsys Inc. (c 1999)
;;
;; Author:      Gregor Klinke <gck@eyestep.org>
;; Maintainer:  Gregor Klinke <gck@eyestep.org>
;; Version:     0.0.1
;; Keywords:    languages heather
;; WWW:         http://www.eyestep.org/heather
;;
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;; Commentary:
;; This package provides a simple Emacs major mode for editing heather code
;; and modules.  It includes the following features:
;;
;; Documentation:
;; See comment string of function `heather-mode' or type `C-c C-h' in Emacs.
;;
;; Installation:
;; Put `heather-mode.el' into the `site-lisp' directory of your Emacs
;; installation or into an arbitrary directory that is added to the load
;; path by the following line in your Emacs start-up file (`.emacs'):
;;
;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))
;;
;; If you already have the compiled `heather-mode.elc' file, put it in the
;; same directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  heather-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f byte-compile heather-mode.el
;;
;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):
;;
;;   (autoload 'heather-mode "heather-mode" "Heather Mode" t)
;;   (setq auto-mode-alist (cons '("\\.hea\\'" . heather-mode) auto-mode-alist))


;; ----------------------------------------------------------------------
;; VARIABLES
;; Set a number of global variable for customization, global constants, etc.
;; ----------------------------------------------------------------------
(defgroup heather nil
  "Customizations for Heather Mode."
  :prefix "heather-"
  :group 'languages)

(defcustom heather-basic-offset 2
  "*Amount of basic offset used for indentation."
  :type 'integer
  :group 'heather)

(defconst heather-version "0.0.1"
  "Heather Mode version number.")

;; XEmacs handling
(defconst heather-xemacs (string-match "XEmacs" emacs-version)
  "Non-nil if XEmacs is used.")


;; ----------------------------------------------------------------------
;; KEY BINDINGS
;; Define the keymap for heather map.
;; ----------------------------------------------------------------------
(defvar heather-mode-map ()
  "Keymap for Heather Mode.")

(setq heather-mode-map (make-sparse-keymap))
;; backspace/delete key bindings
(define-key heather-mode-map [backspace] 'backward-delete-char-untabify)
(unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
  (define-key heather-mode-map [delete]       'delete-char)
  (define-key heather-mode-map [(meta delete)] 'kill-word))

;; standard key bindings
(define-key heather-mode-map "\M-e"     'heather-forward-statement) ;; TODO
(define-key heather-mode-map "\M-a"     'heather-backward-statement) ;; TODO

;; mode specific key bindings
(define-key heather-mode-map "\C-c\t"   'indent-relative)
(define-key heather-mode-map "\M-\C-\\" 'heather-indent-region)
(define-key heather-mode-map "\C-c\C-c" 'heather-comment-uncomment-region)
(define-key heather-mode-map "\C-c\C-f" 'heather-fontify-buffer)
(define-key heather-mode-map "\C-c\C-h" 'heather-doc-mode)
(define-key heather-mode-map "\C-c\C-v" 'heather-version)
(define-key heather-mode-map "\M-\t"    'tab-to-tab-stop)


;; ----------------------------------------------------------------------
;; Menu
;; ----------------------------------------------------------------------
(defvar heather-mode-menu-list
  '("Heather"
    ["(Un)Comment Out Region"	   heather-comment-uncomment-region (mark)]
    "--"
    ["Move Forward Statement"	   heather-forward-statement t]
    ["Move Backward Statement"	 heather-backward-statement t]
    ["Move Forward Same Indent"  heather-forward-same-indent t]
    ["Move Backward Same Indent" heather-backward-same-indent t]
    "--"
    ["Indent Region"             heather-indent-region (mark)]
    ["Indent Buffer"             heather-indent-buffer t]
    "--"
    ["Documentation"             heather-doc-mode :keys "C-c C-h"]
    ["Version"                   heather-version t]
    ["Bug Report..."             heather-mode-submit-bug-report t]
    "--"
    ["Customize..."              heather-customize t]
    )
  "Heather Mode menu.")

(require 'easymenu)


;;; ----------------------------------------------------------------------
;;; Syntax table
;;; ----------------------------------------------------------------------
(defvar heather-mode-syntax-table nil
  "Syntax table used in `heather-mode' buffers.")

(setq heather-mode-syntax-table (make-syntax-table))
;; punctuation
(modify-syntax-entry ?\. "."    heather-mode-syntax-table)
(modify-syntax-entry ?\, "."    heather-mode-syntax-table)
(modify-syntax-entry ?\< "."    heather-mode-syntax-table)
(modify-syntax-entry ?\= "."    heather-mode-syntax-table)
(modify-syntax-entry ?\> "."    heather-mode-syntax-table)
(modify-syntax-entry ?\\ "."    heather-mode-syntax-table)
(modify-syntax-entry ?\| "."    heather-mode-syntax-table)
(modify-syntax-entry ?\@ "."	  heather-mode-syntax-table)
;; string
(modify-syntax-entry ?\' "."    heather-mode-syntax-table)
(modify-syntax-entry ?\" "\""   heather-mode-syntax-table)
;; words and symbols
(modify-syntax-entry ?\% "w"    heather-mode-syntax-table)
(modify-syntax-entry ?\/ "w"    heather-mode-syntax-table)
(modify-syntax-entry ?\+ "."    heather-mode-syntax-table)
(modify-syntax-entry ?\- "."    heather-mode-syntax-table)
(modify-syntax-entry ?\* "."    heather-mode-syntax-table)
(modify-syntax-entry ?\: "."    heather-mode-syntax-table)
(modify-syntax-entry ?\! "w"    heather-mode-syntax-table)
(modify-syntax-entry ?\? "w"    heather-mode-syntax-table)
(modify-syntax-entry ?\_ "w"	  heather-mode-syntax-table)
(modify-syntax-entry ?\# "w"	  heather-mode-syntax-table)
(modify-syntax-entry ?\& "w"	  heather-mode-syntax-table)
(modify-syntax-entry ?\$ "w"  	heather-mode-syntax-table)
;; parentheses to match
(modify-syntax-entry ?\( "()"   heather-mode-syntax-table)
(modify-syntax-entry ?\) ")("   heather-mode-syntax-table)
(modify-syntax-entry ?\[ "(]"   heather-mode-syntax-table)
(modify-syntax-entry ?\] ")["   heather-mode-syntax-table)
(modify-syntax-entry ?\{ "(}"   heather-mode-syntax-table)
(modify-syntax-entry ?\} "){"   heather-mode-syntax-table)

;;(modify-syntax-entry ?\; "<"    heather-mode-syntax-table)
(modify-syntax-entry ?\n ">"    heather-mode-syntax-table)


;;; ----------------------------------------------------------------------
;;; Mode definition
;;; ----------------------------------------------------------------------

;;;###autoload
(defun heather-mode ()
  "Major mode for editing Heather code.

Usage:
------

- WORD/COMMAND COMPLETION:  Typing `\\[heather-electric-tab]' after a (not completed) word looks
  for a word in the buffer or a Heather keyword that starts alike, inserts it
  and adjusts case.  Re-typing `\\[heather-electric-tab]' toggles through alternative word
  completions.

  Typing `\\[heather-electric-tab]' after a non-word character inserts a tabulator stop (if
  not at the beginning of a line).  `\\[tab-to-tab-stop]' always inserts a tabulator stop.

- COMMENTS:  `\\[heather-comment-uncomment-region]' comments out a region if not commented out, and
  uncomments a region if already commented out.

- HIGHLIGHTING (fontification):  Heather keywords, predefined types and constants,
  function names, declaration names, directives, as well as comments and
  strings are highlighted using different colors.


Maintenance:
------------

To submit a bug report, use the corresponding menu entry within Heather Mode.
Add a description of the problem and include a reproducible test case.

Feel free to send questions and enhancement requests to <gck@eyestep.org>.

Official distribution is at <http://www.eyestep.org/heather/heather-mode.html>.


                                                  The Heather Mode Maintainer
                                               Gregor Klinke <gck@eyestep.org>

Key bindings:
-------------

\\{heather-mode-map}"

  (interactive)

  ;; kill all local variables
  (kill-all-local-variables)
  ;; set the major mode
  (setq major-mode 'heather-mode)
  ;; specify the mode name
  (setq mode-name "Heather")
  ;; set maps and tables
  (use-local-map heather-mode-map)
  ;; set the syntax table
  (set-syntax-table heather-mode-syntax-table)

  ;; set local variables
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent) ;; TODO
  (set (make-local-variable 'end-comment-column) 79)
  (set (make-local-variable 'paragraph-start) "^$")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'indent-relative);; 'heather-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t) ;; ???

  ;; set local abbreviation table
  ;; TODO:

  ;; set local comment syntax
  ;; TODO:

  ;; initialize font locking
  (require 'font-lock)
  (set (make-local-variable 'font-lock-defaults)
       '(heather-font-lock-keywords nil nil ((?\_ . "w"))))
;  (set (make-local-variable 'font-lock-beginning-of-syntax-function)
;       (heather-beginning-of-syntax-function)) ;; TODO
  (turn-on-font-lock)

  ;; add menu
  (easy-menu-add heather-mode-menu-list) ; for XEmacs
  (easy-menu-define heather-mode-menu heather-mode-map
		    "Menu keymap for Heather Mode." heather-mode-menu-list)
  (run-hooks 'menu-bar-update-hook)

  ;; miscellaneous
  (message "Heather Mode %s.  Type C-c C-h for documentation." heather-version)

  ;; run hooks
  (run-hooks 'heather-mode-hook))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heather definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ----------------------------------------------------------------------
;; Keywords
;; ----------------------------------------------------------------------
(defconst heather-keywords
  '(
    "namespace" "import" "def" "let"
    "type" "class" "macro" "meth" "alias" "fluid" "const"
    "slot" "slot!"
    "if" "else" "on"
    )
  "List of Heather keywords.")

(defconst heather-types
  '(
    "Any"
    "Bool" "Char"
    "Int" "Rational" "Real" "Complex" "Short" "UShort" "Word" "UWord"
    "Long" "ULong" "Float" "Double" "LongDouble"
    "String"
    "Vector"
    "Number" "ExactNumber" "ApproxNumber" "Ordered" "Unordered"
    )
  "List of Heather predefined types.")

(defconst heather-builtin
  '(
    "for" "select" "until" "then" "while"
    )
  "List of heather special forms.")

(defconst heather-operator
  '(
    "and" "or" "mod" "by" "in" "not"
    ".." "..." "<" ">" "==" "<>" "<=" ">=" "<=>"
    "%"
    )
  "List of heather operators.")

(defconst heather-constants
  '(
    "#nil" "#t" "#f" "#eof" "#true" "#false" "#function" "#func"
    )
  "List of heather predefined constants.")


;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst heather-keywords-regexp
  (concat "\\<\\(" (regexp-opt heather-keywords) "\\)\\>")
  "Regexp for heather keywords.")

(defconst heather-types-regexp
  (concat "\\<\\(" (regexp-opt heather-types) "\\)\\>")
  "Regexp for heather predefined types.")

(defconst heather-builtin-regexp
  (concat "\\<\\(" (regexp-opt heather-builtin) "\\)\\>")
  "Regexp for heather predefined system functions and methods.")

(defconst heather-builtin-operator
  (concat "\\<\\(" (regexp-opt heather-operator) "\\)\\>")
  "Regexp for heather predefined operators.")

(defconst heather-constants-regexp
  (concat "\\<\\(" (regexp-opt heather-constants) "\\)\\>")
  "Regexp for heather predefined constants.")


;;; ----------------------------------------------------------------------
;;; Font locking
;;; ----------------------------------------------------------------------
;; XEmacs compatibility
(when heather-xemacs
  (require 'font-lock)
  (copy-face 'font-lock-reference-face 'font-lock-constant-face)
  (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face))

(defun heather-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point. Adapted from
`font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\(\\w+\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*\\(\\[[^]]*\\]\\s-*\\)?,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))


;;; ident: [a-zA-Z-_$?!&%<>]+[a-zA-Z0-9-_$?!&%*+<>]*

(defvar heather-font-lock-keywords
  (list
   ;; highlight comments
   '("\\(--.*\\)" (1 font-lock-comment-face))

   ;; highlight keywords
   (list heather-keywords-regexp 1 'font-lock-keyword-face)

   ;; highlight keywords (begining with ')
   '("\\('[a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 font-lock-constant-face))

   ;; highlight parameter names (ending with ':')
   '("\\<\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\>:"
     (1 heather-font-lock-param-name-face))

   ;; highlight function, method, hook declarations.
   '("\\(def\\|let\\|on\\)\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-*("
     (2 font-lock-function-name-face))
   '("def\\s-+meth\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-*("
     (1 font-lock-function-name-face))

   ;; highlight type parameters
   ;; '("#<\\([^>]*\\)>"
   ;; (1 heather-font-lock-type-def-face))

   ;; highlight type and class declarations.
   '("def\\s-+\\(type\\|class\\|alias\\)\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-*"
     (2 heather-font-lock-type-def-face))

   ;; highlight variable declarations.
   '("\\(def\\|let\\)\\s-+\\([a-zA-Z-_$?!&%|]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*"
     (2 font-lock-variable-name-face))
   '("\\(def\\|let\\)\\s-+\\(fluid\\|const\\)\\s-+\\([a-zA-Z-_$?!&%|]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*"
     (3 font-lock-variable-name-face))
   '("slot\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-*"
     (1 font-lock-variable-name-face))

   ;; highlight parameters and types
   '("\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-+:\\s-*\\(@\\)?\\s-*\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 font-lock-variable-name-face)
     (3 font-lock-type-face))

   ;; highlight types
   (list heather-types-regexp 1 'font-lock-type-face)
   ;; highlight constants
   (list heather-constants-regexp 1 'font-lock-constant-face)
   ;; highlight predefined functions, tasks and methods
   (list heather-builtin-regexp 1 'font-lock-builtin-face)
   ;; highlight predefined operators
   (list heather-builtin-operator 1 'heather-font-lock-operator-face)


   ;; highlight functions.  FIXME:
;;   '("[^:<>=+*/%-]\\s-+\\([a-zA-Z-$@?!%&_]+\\)[:)]"
;;   '("[]!?)$&@%a-zA-Z0-9]\\s-+\\([a-zA-Z-$@?!%&_]+\\)[^|]"
;;     1 font-lock-function-name-face)
   )
  "Regular expressions to highlight in Heather Mode.")

(defvar heather-font-lock-operator-face 'heather-font-lock-operator-face
  "Face name to use for operators.")

(defvar heather-font-lock-function-face 'heather-font-lock-function-face
  "Face name to use for predefined functions and tasks.")

(defvar heather-font-lock-param-name-face 'heather-font-lock-param-name-face
  "Face name to use for parameter names.")

(defvar heather-font-lock-type-def-face 'heather-font-lock-type-def-face
  "Face name for type definition names.")

(defface heather-font-lock-operator-face
  '((((class color) (background light)) (:foreground "Gold4"))
    (((class color) (background dark)) (:foreground "BurlyWood1"))
;   '((((class color) (background light)) (:foreground "SaddleBrown"))
;     (((class color) (background dark)) (:foreground "BurlyWood"))
    (t (:weight bold)))
  "Font lock mode face used to highlight @ definitions."
  :group 'font-lock-highlighting-faces)

(defface heather-font-lock-function-face
  '((((class color) (background light)) (:foreground "DarkCyan"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:weight bold)))
  "Font lock mode face used to highlight predefined functions and tasks."
  :group 'font-lock-highlighting-faces)

(defface heather-font-lock-param-name-face
  '((((class color) (background light)) (:foreground "DarkBlue"))
    (((class color) (background dark)) (:foreground "White"))
    (t (:weight bold)))
  "Font lock mode face used for parameter names."
  :group 'font-lock-highlighting-faces)

(defface heather-font-lock-type-def-face
  '((((class color) (background light)) (:foreground "DarkGreen"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:weight bold)))
  "Face name for type definition names."
  :group 'font-lock-highlighting-faces)

(defun heather-fontify-buffer ()
  "Fontify buffer."
  (interactive)
  (font-lock-fontify-buffer))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar heather-echo-syntactic-information-p t
  "If non-nil, syntactic info is echoed when the line is indented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset functions

(defconst heather-offsets-alist
  '((comment        . heather-lineup-C-comments)
    (string         . -1000)
    (directive      . -1000)
    (block-open     . 0)
    (block-intro    . +)
    (block-close    . 0)
    (arglist-intro  . +)
    (arglist-cont   . +)
    (arglist-cont-nonempty . 0)
    (arglist-close  . 0)
    (statement      . 0)
    (statement-cont . +)
    (substatement   . +)
    (else-clause    . 0))
  "Association list of syntactic element symbols and indentation offsets.
Adapted from `c-offsets-alist'.")

(defun heather-evaluate-offset (offset langelem symbol)
  "Offset can be a number, a function, a variable, a list, or one of
the symbols + or -."
  (cond
   ((eq offset '+)         (setq offset heather-basic-offset))
   ((eq offset '-)         (setq offset (- heather-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 heather-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- heather-basic-offset))))
   ((eq offset '*)         (setq offset (/ heather-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- heather-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (heather-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		0
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset))))
  offset)

(defun heather-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
heather-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol heather-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(setq offset 0
	      relpos 0)
      (setq offset (heather-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (save-excursion (beginning-of-line) (point))))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (heather-evaluate-offset offset langelem symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help functions

(defsubst heather-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line
  eol  -- end of line
  boi  -- back to indentation
  ionl -- indentation of next line
  iopl -- indentation of previous line
  bonl -- beginning of next line
  bopl -- beginning of previous line
This function does not modify point or mark."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl) (forward-line -1) (back-to-indentation))
     ((eq position 'ionl) (forward-line 1) (back-to-indentation))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun heather-in-literal (&optional lim)
  "Determine if point is in a Heather literal."
  (save-excursion
    (let ((state (parse-partial-sexp (or lim (point-min)) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun heather-skip-forward-literal ()
  "Skip forward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-forward "\"") t) ; inside string
     ((nth 7 state) (forward-line 1) t)	     ; inside // comment
     ((nth 4 state) (search-forward "*/") t) ; inside /* */ comment
     (t nil))))

(defun heather-skip-backward-literal ()
  "Skip backward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-backward "\"") t) ; inside string
     ((nth 7 state) (search-backward "//") t) ; inside // comment
     ((nth 4 state) (search-backward "/*") t) ; inside /* */ comment
     (t nil))))

(defsubst heather-re-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-forward regexp bound noerror)
	      (heather-skip-forward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (< (point) bound) t))))
  (match-end 0))

(defsubst heather-re-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-backward regexp bound noerror)
	      (heather-skip-backward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (> (point) bound) t))))
  (match-end 0))

(defun heather-forward-syntactic-ws (&optional lim skip-directive)
  "Forward skip of syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)
	(when (and skip-directive (looking-at "^\\s-*#"))
	  (end-of-line))))))

(defun heather-backward-syntactic-ws (&optional lim skip-directive)
  "Backward skip over syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (when (< lim (point))
	(narrow-to-region lim (point))
	(while (/= here (point))
	  (setq here (point))
	  (forward-comment hugenum)
	  (when (and skip-directive
		     (save-excursion (back-to-indentation)
				     (= (following-char) ?\#)))
	    (beginning-of-line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment indentation functions

(defsubst heather-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of langelem's relpos.
Leaves point at the relpos unless preserve-point is non-nil."
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here)))))

(defun heather-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Nicked from `c-lineup-C-comments'."
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (heather-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (heather-point 'eol) t))
	  (progn
	    (if (not (looking-at "[*]+"))
		(progn
		  ;; we now have to figure out where this comment begins.
		  (goto-char here)
		  (back-to-indentation)
		  (if (looking-at "[*]+/")
		      (progn (goto-char (match-end 0))
			     (forward-comment -1))
		    (goto-char (cdr langelem))
		    (back-to-indentation))))
	    (- (current-column) langelem-col))
	(if (zerop stars)
	    (progn
	      (skip-chars-forward " \t")
	      (- (current-column) langelem-col))
	  ;; how many stars on comment opening line?  if greater than
	  ;; on current line, align left.  if less than or equal,
	  ;; align right.  this should also pick up Javadoc style
	  ;; comments.
	  (if (> (length (match-string 1)) stars)
	      (progn
		(back-to-indentation)
		(- (current-column) -1 langelem-col))
	    (- (current-column) stars langelem-col)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move functions

(defconst heather-beg-block-re "{\\|\\<\\(begin\\|fork\\)\\>")

(defconst heather-end-block-re "}\\|\\<\\(end\\|join\\(\\s-+\\(all\\|any\\|none\\)\\)?\\)\\>")

(defconst heather-beg-substatement-re "\\<\\(else\\|for\\|if\\|repeat\\|while\\)\\>")

(defun heather-corresponding-begin ()
  "Find corresponding block begin if cursor is at a block end."
  (while (and (heather-re-search-backward
	       (concat "\\(" heather-end-block-re "\\)\\|" heather-beg-block-re)
	       nil t)
	      (match-string 1))
    (heather-corresponding-begin))
  (heather-beginning-of-substatement))

(defun heather-corresponding-if ()
  "Find corresponding `if' if cursor is at `else'."
  (while (and (heather-re-search-backward "}\\|\\<\\(if\\|else\\)\\>" nil t)
	      (not (equal (match-string 0) "if")))
    (if (equal (match-string 0) "else")
	(heather-corresponding-if)
      (forward-char)
      (backward-sexp))))

(defun heather-beginning-of-statement ()
  "Go to beginning of current statement."
  (let (pos)
    (while
	(progn
	  ;; search for end of previous statement
	  (while
	      (and (heather-re-search-backward
		    (concat "[',]\\|" heather-beg-block-re
			    "\\|" heather-end-block-re) nil t)
		   (equal (match-string 0) ")"))
	    (forward-char)
	    (backward-sexp))
	  (setq pos (match-beginning 0))
	  ;; go back to beginning of current statement
	  (goto-char (or (match-end 0) 0))
	  (heather-forward-syntactic-ws nil t)
	  (when (looking-at "(")
	    (forward-sexp)
	    (heather-forward-syntactic-ws nil t))
	  ;; if "else" found, go to "if" and search again
	  (when (looking-at "\\<else\\>")
	    (heather-corresponding-if)
	    (setq pos (point))
	    t))
      ;; if search is repeated, go to beginning of last search
      (goto-char pos))))

(defun heather-beginning-of-substatement ()
  "Go to beginning of current substatement."
  (let ((lim (point))
	pos)
  ;; go to beginning of statement
    (heather-beginning-of-statement)
    (setq pos (point))
    ;; go forward all substatement opening statements until at LIM
    (while (and (< (point) lim)
		(heather-re-search-forward heather-beg-substatement-re lim t))
      (setq pos (match-beginning 0)))
    (heather-forward-syntactic-ws nil t)
    (when (looking-at "(")
      (forward-sexp)
      (heather-forward-syntactic-ws nil t))
    (when (< (point) lim)
      (setq pos (point)))
    (goto-char pos)))

(defun heather-forward-statement ()
  "Move forward one statement."
  (interactive)
  (while (and (heather-re-search-forward
	       (concat "[(;]\\|" heather-beg-block-re "\\|" heather-end-block-re)
	       nil t)
	      (equal (match-string 0) "("))
    (backward-char)
    (forward-sexp))
  (heather-beginning-of-substatement))

(defun heather-backward-statement ()
  "Move backward one statement."
  (interactive)
  (heather-backward-syntactic-ws nil t)
  (unless (= (preceding-char) ?\))
    (backward-char))
  (heather-beginning-of-substatement))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax analysis

(defmacro heather-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in symbol to the syntax list.
try to increase performance by using this macro."
  `(setq syntax (cons (cons ,symbol ,(or relpos 0)) syntax)))

(defun heather-guess-basic-syntax ()
  "Determine syntactic context of current line of code."
  (save-excursion
    (beginning-of-line)
    (let (syntax state placeholder pos)
      ;; determine syntax state
      (setq state (parse-partial-sexp (point-min) (point)))
      (cond
       ;; CASE 1: in a comment?
       ((nth 4 state)
	;; skip empty lines
	(while (and (zerop (forward-line -1))
		    (looking-at "^\\s-*$")))
	(heather-add-syntax 'comment (heather-point 'boi)))
       ;; CASE 2: in a string?
       ((nth 3 state)
	(heather-add-syntax 'string))
       ;; CASE 3: at a directive?
       ((save-excursion (back-to-indentation) (= (following-char) ?\#))
	(heather-add-syntax 'directive (point)))
       ;; CASE 4: after an opening parenthesis (argument list continuation)?
       ((= (char-after (nth 1 state)) ?\()
	(goto-char (1+ (nth 1 state)))
	;; is there code after the opening parenthesis on the same line?
	(if (looking-at "\\s-*$")
	    (heather-add-syntax 'arglist-cont (heather-point 'boi))
	  (heather-add-syntax 'arglist-cont-nonempty (point))))
       ;; CASE 5: at a block closing?
       ((save-excursion (back-to-indentation) (looking-at heather-end-block-re))
	;; look for the corresponding begin
	(heather-corresponding-begin)
	(heather-add-syntax 'block-close (heather-point 'boi)))
       ;; CASE 6: at a block intro (the first line after a block opening)?
       ((and (save-excursion
	       (heather-backward-syntactic-ws nil t)
	       ;; previous line ends with a block opening?
	       (or (/= (skip-chars-backward "{") 0) (backward-word 1))
	       (when (looking-at heather-beg-block-re)
		 ;; go to beginning of substatement
		 (heather-beginning-of-substatement)
		 (setq placeholder (point))))
	     ;; not if "fork" is followed by "{"
	     (save-excursion
	       (not (and (progn (back-to-indentation) (looking-at "{"))
			 (progn (goto-char placeholder)
				(looking-at "\\<fork\\>"))))))
	(goto-char placeholder)
	(heather-add-syntax 'block-intro (heather-point 'boi)))
       ;; CASE 7: at the beginning of an else clause?
       ((save-excursion (back-to-indentation) (looking-at "\\<else\\>"))
	;; find corresponding if
	(heather-corresponding-if)
	(heather-add-syntax 'else-clause (heather-point 'boi)))
       ;; CASE 8: at the beginning of a statement?
       ;; is the previous command completed?
       ((or (save-excursion
	      (heather-backward-syntactic-ws nil t)
	      (setq placeholder (point))
	      ;; at the beginning of the buffer?
	      (or (bobp)
		  ;; previous line ends with a semicolon or
		  ;; is a block opening or closing?
		  (when (or (/= (skip-chars-backward "{};") 0)
			    (progn (back-to-indentation)
				   (looking-at (concat heather-beg-block-re "\\|"
						       heather-end-block-re))))
		    ;; if at a block closing, go to beginning
		    (when (looking-at heather-end-block-re)
		      (heather-corresponding-begin))
		    ;; go to beginning of the statement
		    (heather-beginning-of-statement)
		    (setq placeholder (point)))
		  ;; at a directive?
		  (when (progn (back-to-indentation) (looking-at "#"))
		    ;; go to previous statement
		    (heather-beginning-of-statement)
		    (setq placeholder (point)))))
	    ;; at a block opening?
	    (when (save-excursion (back-to-indentation)
				  (looking-at heather-beg-block-re))
	      ;; go to beginning of the substatement
	      (heather-beginning-of-substatement)
	      (setq placeholder (point))))
	(goto-char placeholder)
	(heather-add-syntax 'statement (heather-point 'boi)))
       ;; CASE 9: at the beginning of a substatement?
       ;; is this line preceeded by a substatement opening statement?
       ((save-excursion (heather-backward-syntactic-ws nil t)
			(when (= (preceding-char) ?\)) (backward-sexp))
			(backward-word 1)
			(setq placeholder (point))
			(looking-at heather-beg-substatement-re))
	(goto-char placeholder)
	(heather-add-syntax 'substatement (heather-point 'boi)))
       ;; CASE 10: it must be a statement continuation!
       (t
	;; go to beginning of statement
	(heather-beginning-of-substatement)
	(heather-add-syntax 'statement-cont (heather-point 'boi))))
      syntax)))





;; ----------------------------------------------------------------------
;; indentation functions
;; indenting heather syntax is fairly simple, the algorithm is:
;;
;; 1) does the cur line begin with a + -
;;       -> regard it as method start
;;
;; 2) does the cur line begin with ident := class|type|decl
;;       -> regard it as a declaration start
;;
;; 3) does the line look like "string" load
;;       -> regard it as include line
;;
;; 4) does the preceding line contains
;;
;
;name := class
;    <type>
;    &elt _type
;(
;    + name <type> &key parm <type>
;                  &key parm <type> |
;        self value,
;	abc slkfjsd,
;	sdgsdf sdfhskf
;	     ' fhsdfhwez
;	     ' asgasg hasd
;	self.
;
;)

(defun heather-forward-comment ()
  "ashddhaksdasldashk"
  (interactive)
  (forward-comment -1))

;; ----------------------------------------------------------------------
(defun heather-indent-line ()
  "Indent the current line as heather code. Optional SYNTAX is the
syntactic information for the current line. Returns the amount of
indentation change (in columns)."
  (interactive)
  (let* ((syntax (heather-guess-basic-syntax))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'heather-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (when heather-echo-syntactic-information-p
      (message "syntax: %s, indent= %d" syntax indent))
    (unless (zerop shift-amt)
      (beginning-of-line)
      (delete-region (point) (heather-point 'boi))
      (indent-to indent))
    (if (< (point) (heather-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    shift-amt))

(defun heather-indent-buffer ()
  "Indent whole buffer as Heather code.
Calls `indent-region' for whole buffer."
  (interactive)
  (message "Indenting buffer...")
  (indent-region (point-min) (point-max) nil)
  (message "Indenting buffer...done"))

(defun heather-indent-region (start end column)
  "Indent region as Heather code."
  (interactive "r\nP")
  (message "Indenting region...")
  (indent-region start end column)
  (message "Indenting region...done"))

(defsubst heather-indent-block-closing ()
  "If previous word is a block closing or `else', indent line again."
  (when (= (char-syntax (preceding-char)) ?w)
    (save-excursion
      (backward-word 1)
      (when (and (not (heather-in-literal))
		 (looking-at (concat heather-end-block-re "\\|\\<else\\>")))
	(heather-indent-line)))))











;;; ----------------------------------------------------------------------
;; Comments
;;; ----------------------------------------------------------------------
(defun heather-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end -1)
    (comment-region beg end)))


;;; ----------------------------------------------------------------------
;;; Help functions
;;; ----------------------------------------------------------------------
(defun heather-customize ()
  "Call the customize function with `heather' as argument."
  (interactive)
  (customize-browse 'heather))


;;; ----------------------------------------------------------------------
;;; Other
;;; ----------------------------------------------------------------------
;; remove ".heather" and ".mod" from `completion-ignored-extensions'
(setq completion-ignored-extensions
      (delete ".heather" completion-ignored-extensions))
(setq completion-ignored-extensions
      (delete ".mod" completion-ignored-extensions))


;;; ----------------------------------------------------------------------
;;; Bug reports
;;; ----------------------------------------------------------------------
(defconst heather-mode-help-address "Heather Mode Maintainer <gck@eyestep.org>"
  "Address for Heather Mode bug reports.")

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun heather-mode-submit-bug-report ()
  "Submit via mail a bug report on Heather Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on Heather Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    heather-mode-help-address
    (concat "Heather Mode " heather-version)
    (list
     ;; report all important variables
     'heather-basic-offset
     )
    nil nil
    "Dear Heather Mode maintainer,")))

;;; ----------------------------------------------------------------------
;;; Documentation
;;; ----------------------------------------------------------------------
(defun heather-version ()
  "Echo the current version of Heather Mode in the minibuffer."
  (interactive)
  (message "Using Heather Mode version %s" heather-version))

(defun heather-doc-mode ()
  "Display Heather Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'heather-mode))
    (unless heather-xemacs
      (help-setup-xref (list #'heather-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

;;; ----------------------------------------------------------------------


;;;;;;;;; to be done .........
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of Heather commands)

(defvar heather-abbrev-list
  (append
   (list nil) heather-keywords
;;	  (list nil) heather-types
	  (list nil) heather-builtin
	  (list nil) heather-constants)
  "Predefined abbreviations for Heather.")

(defvar heather-expand-upper-case nil)

(eval-when-compile (require 'hippie-exp))

(defun heather-try-expand-abbrev (old)
  "Try expanding abbreviations from `heather-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list heather-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
;		  (equal (car he-expand-list) he-search-string)))
    (unless (stringp (car he-expand-list))
      (setq heather-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if heather-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; function for expanding abbrevs and dabbrevs
(defun heather-expand-abbrev (arg))
(fset 'heather-expand-abbrev (make-hippie-expand-function
			       '(try-expand-dabbrev
				 try-expand-dabbrev-all-buffers
				 heather-try-expand-abbrev)))

(provide 'heather-mode)

;;; heather-mode.el ends here
