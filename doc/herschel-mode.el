;;; herschel-mode.el --- major mode for editing herschel files and modules.
;;
;; copyright (c) 2003, 2007, 2008, 2009 Gregor Klinke
;;
;; Some parts of this mode are based on the vera-mode by Reto Zimmermann,
;; Synopsys Inc. (c 1999)
;;
;; Author:      Gregor Klinke <gck@eyestep.org>
;; Maintainer:  Gregor Klinke <gck@eyestep.org>
;; Version:     0.0.1
;; Keywords:    languages herschel
;; WWW:         http://www.eyestep.org/herschel
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
;; This package provides a simple Emacs major mode for editing herschel code
;; and modules.  It includes the following features:
;;
;; Documentation:
;; See comment string of function `herschel-mode' or type `C-c C-h' in Emacs.
;;
;; Installation:
;; Put `herschel-mode.el' into the `site-lisp' directory of your Emacs
;; installation or into an arbitrary directory that is added to the load
;; path by the following line in your Emacs start-up file (`.emacs'):
;;
;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))
;;
;; If you already have the compiled `herschel-mode.elc' file, put it in the
;; same directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  herschel-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f byte-compile herschel-mode.el
;;
;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):
;;
;;   (autoload 'herschel-mode "herschel-mode" "Herschel Mode" t)
;;   (setq auto-mode-alist (cons '("\\.hr\\'" . herschel-mode) auto-mode-alist))


;; ----------------------------------------------------------------------
;; VARIABLES
;; Set a number of global variable for customization, global constants, etc.
;; ----------------------------------------------------------------------
(defgroup herschel nil
  "Customizations for Herschel Mode."
  :prefix "herschel-"
  :group 'languages)

(defcustom herschel-basic-offset 2
  "*Amount of basic offset used for indentation."
  :type 'integer
  :group 'herschel)

(defconst herschel-version "0.0.2"
  "Herschel Mode version number.")

;; XEmacs handling
(defconst herschel-xemacs (string-match "XEmacs" emacs-version)
  "Non-nil if XEmacs is used.")


;; ----------------------------------------------------------------------
;; KEY BINDINGS
;; Define the keymap for herschel map.
;; ----------------------------------------------------------------------
(defvar herschel-mode-map ()
  "Keymap for Herschel Mode.")

(setq herschel-mode-map (make-sparse-keymap))
;; backspace/delete key bindings
(define-key herschel-mode-map [backspace] 'backward-delete-char-untabify)
(unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
  (define-key herschel-mode-map [delete]       'delete-char)
  (define-key herschel-mode-map [(meta delete)] 'kill-word))

;; standard key bindings
(define-key herschel-mode-map "\M-e"     'herschel-forward-statement) ;; TODO
(define-key herschel-mode-map "\M-a"     'herschel-backward-statement) ;; TODO

;; mode specific key bindings
(define-key herschel-mode-map "\C-c\t"   'indent-relative)
(define-key herschel-mode-map "\M-\C-\\" 'herschel-indent-region)
(define-key herschel-mode-map "\C-c\C-c" 'herschel-comment-uncomment-region)
(define-key herschel-mode-map "\C-c\C-f" 'herschel-fontify-buffer)
(define-key herschel-mode-map "\C-c\C-h" 'herschel-doc-mode)
(define-key herschel-mode-map "\C-c\C-v" 'herschel-version)
(define-key herschel-mode-map "\M-\t"    'tab-to-tab-stop)


;; ----------------------------------------------------------------------
;; Menu
;; ----------------------------------------------------------------------
(defvar herschel-mode-menu-list
  '("Herschel"
    ["(Un)Comment Out Region"	   herschel-comment-uncomment-region (mark)]
    "--"
    ["Move Forward Statement"	   herschel-forward-statement t]
    ["Move Backward Statement"	 herschel-backward-statement t]
    ["Move Forward Same Indent"  herschel-forward-same-indent t]
    ["Move Backward Same Indent" herschel-backward-same-indent t]
    "--"
    ["Indent Region"             herschel-indent-region (mark)]
    ["Indent Buffer"             herschel-indent-buffer t]
    "--"
    ["Documentation"             herschel-doc-mode :keys "C-c C-h"]
    ["Version"                   herschel-version t]
    ["Bug Report..."             herschel-mode-submit-bug-report t]
    "--"
    ["Customize..."              herschel-customize t]
    )
  "Herschel Mode menu.")

(require 'easymenu)


;;; ----------------------------------------------------------------------
;;; Syntax table
;;; ----------------------------------------------------------------------
(defvar herschel-mode-syntax-table nil
  "Syntax table used in `herschel-mode' buffers.")

(setq herschel-mode-syntax-table (make-syntax-table))
;; punctuation
(modify-syntax-entry ?\. "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\, "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\< "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\= "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\> "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\\ "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\| "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\@ "."	  herschel-mode-syntax-table)
;; string
(modify-syntax-entry ?\' "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\` "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\" "\""   herschel-mode-syntax-table)
;; words and symbols
(modify-syntax-entry ?\% "w"    herschel-mode-syntax-table)
(modify-syntax-entry ?\/ "w"    herschel-mode-syntax-table)
(modify-syntax-entry ?\+ "."    herschel-mode-syntax-table)
;(modify-syntax-entry ?\- "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\* "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\: "."    herschel-mode-syntax-table)
(modify-syntax-entry ?\! "w"    herschel-mode-syntax-table)
(modify-syntax-entry ?\? "w"    herschel-mode-syntax-table)
(modify-syntax-entry ?\_ "w"	  herschel-mode-syntax-table)
(modify-syntax-entry ?\# "."	  herschel-mode-syntax-table)
(modify-syntax-entry ?\& "."	  herschel-mode-syntax-table)
(modify-syntax-entry ?\$ "w"  	herschel-mode-syntax-table)
;; parentheses to match
(modify-syntax-entry ?\( "()"   herschel-mode-syntax-table)
(modify-syntax-entry ?\) ")("   herschel-mode-syntax-table)
(modify-syntax-entry ?\[ "(]"   herschel-mode-syntax-table)
(modify-syntax-entry ?\] ")["   herschel-mode-syntax-table)
(modify-syntax-entry ?\{ "(}"   herschel-mode-syntax-table)
(modify-syntax-entry ?\} "){"   herschel-mode-syntax-table)

(modify-syntax-entry ?\~ "!"    herschel-mode-syntax-table)

(modify-syntax-entry ?\- ". 12" herschel-mode-syntax-table)
(modify-syntax-entry ?\n ">"    herschel-mode-syntax-table)


;;; ----------------------------------------------------------------------
;;; Mode definition
;;; ----------------------------------------------------------------------

;;;###autoload
(defun herschel-mode ()
  "Major mode for editing Herschel code.

Usage:
------

- WORD/COMMAND COMPLETION:  Typing `\\[herschel-electric-tab]' after a (not completed) word looks
  for a word in the buffer or a Herschel keyword that starts alike, inserts it
  and adjusts case.  Re-typing `\\[herschel-electric-tab]' toggles through alternative word
  completions.

  Typing `\\[herschel-electric-tab]' after a non-word character inserts a tabulator stop (if
  not at the beginning of a line).  `\\[tab-to-tab-stop]' always inserts a tabulator stop.

- COMMENTS:  `\\[herschel-comment-uncomment-region]' comments out a region if not commented out, and
  uncomments a region if already commented out.

- HIGHLIGHTING (fontification):  Herschel keywords, predefined types and constants,
  function names, declaration names, directives, as well as comments and
  strings are highlighted using different colors.


Maintenance:
------------

To submit a bug report, use the corresponding menu entry within Herschel Mode.
Add a description of the problem and include a reproducible test case.

Feel free to send questions and enhancement requests to <gck@eyestep.org>.

Official distribution is at <http://www.eyestep.org/herschel/herschel-mode.html>.


                                                  The Herschel Mode Maintainer
                                               Gregor Klinke <gck@eyestep.org>

Key bindings:
-------------

\\{herschel-mode-map}"

  (interactive)

  ;; kill all local variables
  (kill-all-local-variables)
  ;; set the major mode
  (setq major-mode 'herschel-mode)
  ;; specify the mode name
  (setq mode-name "Herschel")
  ;; set maps and tables
  (use-local-map herschel-mode-map)
  ;; set the syntax table
  (set-syntax-table herschel-mode-syntax-table)

  ;; set local variables
  (set (make-local-variable 'comment-start-skip) "\\(--[!]?\\) *")
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent) ;; TODO
  (set (make-local-variable 'end-comment-column) 79)
  (set (make-local-variable 'paragraph-start) "^\s*~\\|def\\|module\\|export\\|import\\|on\\|when\\|where")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$\\|^\s*{\\|^\s*}")
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'indent-relative);; 'herschel-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t) ;; ???

  ;; set local abbreviation table
  ;; TODO:

  ;; set local comment syntax
  ;; TODO:

  ;; initialize font locking
  (require 'font-lock)
  (set (make-local-variable 'font-lock-defaults)
       '(herschel-font-lock-keywords nil nil ((?\_ . "w"))))
;  (set (make-local-variable 'font-lock-beginning-of-syntax-function)
;       (herschel-beginning-of-syntax-function)) ;; TODO
  (turn-on-font-lock)

  ;; add menu
  (easy-menu-add herschel-mode-menu-list) ; for XEmacs
  (easy-menu-define herschel-mode-menu herschel-mode-map
		    "Menu keymap for Herschel Mode." herschel-mode-menu-list)
  (run-hooks 'menu-bar-update-hook)

  ;; miscellaneous
  (message "Herschel Mode %s.  Type C-c C-h for documentation." herschel-version)

  ;; run hooks
  (run-hooks 'herschel-mode-hook))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Herschel definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ----------------------------------------------------------------------
;; Keywords
;; ----------------------------------------------------------------------
(defconst herschel-keywords
  '(
    "module" "import" "export" "extend" "when"
    "def" "let"
    "type" "class" "macro" "alias" "fluid" "const" "generic" "enum" "char"
    "measure" "unit"
    "slot" "slot!"
    "if" "else" "on"
    "public" "private" "protected" "final"
    "reify" "where"
    "function"
    "init" "delete" "signal" "exit" "sync"
    )
  "List of Herschel keywords.")

(defconst herschel-types
  '(
    "Any"
    "Bool" "Char"
    "Int" "Rational" "Real" "Complex"
    "Int8" "UInt8" "Int16" "UInt16" "Int32" "UInt32" "Int64" "UInt64"
     "Octet" "Ordinal"
    "Float" "Double" "LongDouble"
    "String"
    "Vector"
    "Number" "ExactNumber" "ApproxNumber" "Ordered" "Unordered"
    "ApproxInt" "ApproxFloat"
    "Function"
    "Nil" "Eof" "Unspecified"
    )
  "List of Herschel predefined types.")

(defconst herschel-builtin
  '(
    "for" "while" "then"
    "select" "match"
    "nil" "eof" "true" "false" "unspecified"
    "return" "break" "continue"
    )
  "List of herschel special forms.")

(defconst herschel-operator
  '(
    "and" "or" "mod" "by" "in" "not" "isa" "xor" "as" "AND" "OR" "XOR"
    ".." "..." "<" ">" "==" "<>" "<=" ">=" "<=>"
    "%" "<<" ">>" "->"
    )
  "List of herschel operators.")

(defconst herschel-constants
  '(
    )
  "List of herschel predefined constants.")


;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst herschel-keywords-regexp
  (concat "\\<\\(" (regexp-opt herschel-keywords) "\\)\\>")
  "Regexp for herschel keywords.")

(defconst herschel-types-regexp
  (concat "\\<\\(" (regexp-opt herschel-types) "\\)\\>")
  "Regexp for herschel predefined types.")

(defconst herschel-builtin-regexp
  (concat "\\<\\(" (regexp-opt herschel-builtin) "\\)\\>")
  "Regexp for herschel predefined system functions and methods.")

(defconst herschel-builtin-operator
  (concat "\\<\\(" (regexp-opt herschel-operator) "\\)\\>")
  "Regexp for herschel predefined operators.")

(defconst herschel-constants-regexp
  (concat "\\<\\(" (regexp-opt herschel-constants) "\\)\\>")
  "Regexp for herschel predefined constants.")


;;; ----------------------------------------------------------------------
;;; Font locking
;;; ----------------------------------------------------------------------
;; XEmacs compatibility
(when herschel-xemacs
  (require 'font-lock)
  (copy-face 'font-lock-reference-face 'font-lock-constant-face)
  (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face))

(defun herschel-font-lock-match-item (limit)
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

(defvar herschel-font-lock-keywords
  (list
   ;; highlight keywords
   (list herschel-keywords-regexp 1 'font-lock-keyword-face)

   ;; highlight generics parameter (begining with ')
   '("\\('[a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 herschel-font-lock-type-def-face))

   ;; highlight keywords (begining with ')
   '("\\(#[a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 font-lock-constant-face))

   ;; highlight parameters and types
   '("\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-+\\(:\\|@\\)\\s-*\\('\\)?\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 font-lock-variable-name-face)
     (4 font-lock-type-face))
   ;; highlight generics
   '("`\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)"
     (1 font-lock-type-face))

   ;; highlight parameter names (ending with ':')
   '("\\<\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\>:"
     (1 herschel-font-lock-param-name-face))

   ;; highlight function, method, hook declarations.
   '("on\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*("
     (2 font-lock-function-name-face))

   '("def\\s-+\\(\\(final\\|abstract\\)\\s-+\\)?\\(\\(generic\\)\\s-+\\)?\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*("
     (5 font-lock-function-name-face))

   '("module\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)"
     (2 font-lock-function-name-face))

   ;; highlight type and class declarations.
   '("def\\s-+\\(\\(final\\|abstract\\|singleton\\)\\s-+\\)?\\(type\\|class\\|alias\\|enum\\)\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)"
     (4 herschel-font-lock-type-def-face))

   ;; highlight local declarations.
   '("let\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*("
     (1 font-lock-function-name-face))


   ;; highlight variable declarations.
   '("\\(def\\|let\\)\\s-+\\(\\(const\\|fluid\\)\\s-+\\)?\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+|]*\\)\\s-*\\(:\\|=\\)"
     (4 font-lock-variable-name-face))

   '("slot\\s-+\\([a-zA-Z-_$?!&%]+[a-zA-Z0-9-_$?!&%*+]*\\)\\s-*"
     (1 font-lock-variable-name-face))


   ;; highlight types
   (list herschel-types-regexp 1 'font-lock-type-face)
   ;; highlight constants
   ;(list herschel-constants-regexp 1 'font-lock-constant-face)
   ;; highlight predefined functions, tasks and methods
   (list herschel-builtin-regexp 1 'font-lock-builtin-face)
   ;; highlight predefined operators
   (list herschel-builtin-operator 1 'herschel-font-lock-operator-face)


   ;; highlight functions.  FIXME:
;;   '("[^:<>=+*/%-]\\s-+\\([a-zA-Z-$@?!%&_]+\\)[:)]"
;;   '("[]!?)$&@%a-zA-Z0-9]\\s-+\\([a-zA-Z-$@?!%&_]+\\)[^|]"
;;     1 font-lock-function-name-face)
   )
  "Regular expressions to highlight in Herschel Mode.")

(defvar herschel-font-lock-operator-face 'herschel-font-lock-operator-face
  "Face name to use for operators.")

(defvar herschel-font-lock-function-face 'herschel-font-lock-function-face
  "Face name to use for predefined functions and tasks.")

(defvar herschel-font-lock-param-name-face 'herschel-font-lock-param-name-face
  "Face name to use for parameter names.")

(defvar herschel-font-lock-type-def-face 'herschel-font-lock-type-def-face
  "Face name for type definition names.")

(defvar herschel-docstring-face 'herschel-docstring-face
  "Face name for documentation strings.")

(defface herschel-font-lock-operator-face
  '((((class color) (background light)) (:foreground "Gold4"))
    (((class color) (background dark)) (:foreground "BurlyWood1"))
;   '((((class color) (background light)) (:foreground "SaddleBrown"))
;     (((class color) (background dark)) (:foreground "BurlyWood"))
    (t (:weight bold)))
  "Font lock mode face used to highlight @ definitions."
  :group 'font-lock-highlighting-faces)

(defface herschel-font-lock-function-face
  '((((class color) (background light)) (:foreground "DarkCyan"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:weight bold)))
  "Font lock mode face used to highlight predefined functions and tasks."
  :group 'font-lock-highlighting-faces)

(defface herschel-font-lock-param-name-face
  '((((class color) (background light)) (:foreground "DarkBlue"))
    (((class color) (background dark)) (:foreground "White"))
    (t (:weight bold)))
  "Font lock mode face used for parameter names."
  :group 'font-lock-highlighting-faces)

(defface herschel-font-lock-type-def-face
  '((((class color) (background light)) (:foreground "DarkGreen"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:weight bold)))
  "Face name for type definition names."
  :group 'font-lock-highlighting-faces)

(defface herschel-docstring-face
  '((((class color) (background light)) (:foreground "MediumOrchid4"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    )
  "Face name for documentation strings."
  :group 'font-lock-highlighting-faces)


(defun herschel-fontify-buffer ()
  "Fontify buffer."
  (interactive)
  (font-lock-fontify-buffer))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar herschel-echo-syntactic-information-p t
  "If non-nil, syntactic info is echoed when the line is indented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset functions

(defconst herschel-offsets-alist
  '((comment        . herschel-lineup-C-comments)
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

(defun herschel-evaluate-offset (offset langelem symbol)
  "Offset can be a number, a function, a variable, a list, or one of
the symbols + or -."
  (cond
   ((eq offset '+)         (setq offset herschel-basic-offset))
   ((eq offset '-)         (setq offset (- herschel-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 herschel-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- herschel-basic-offset))))
   ((eq offset '*)         (setq offset (/ herschel-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- herschel-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (herschel-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		0
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset))))
  offset)

(defun herschel-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
herschel-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol herschel-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(setq offset 0
	      relpos 0)
      (setq offset (herschel-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (save-excursion (beginning-of-line) (point))))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (herschel-evaluate-offset offset langelem symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help functions

(defsubst herschel-point (position)
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

(defun herschel-in-literal (&optional lim)
  "Determine if point is in a Herschel literal."
  (save-excursion
    (let ((state (parse-partial-sexp (or lim (point-min)) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun herschel-skip-forward-literal ()
  "Skip forward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-forward "\"") t) ; inside string
     ((nth 7 state) (forward-line 1) t)	     ; inside // comment
     ((nth 4 state) (search-forward "*/") t) ; inside /* */ comment
     (t nil))))

(defun herschel-skip-backward-literal ()
  "Skip backward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-backward "\"") t) ; inside string
     ((nth 7 state) (search-backward "//") t) ; inside // comment
     ((nth 4 state) (search-backward "/*") t) ; inside /* */ comment
     (t nil))))

(defsubst herschel-re-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-forward regexp bound noerror)
	      (herschel-skip-forward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (< (point) bound) t))))
  (match-end 0))

(defsubst herschel-re-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-backward regexp bound noerror)
	      (herschel-skip-backward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (> (point) bound) t))))
  (match-end 0))

(defun herschel-forward-syntactic-ws (&optional lim skip-directive)
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

(defun herschel-backward-syntactic-ws (&optional lim skip-directive)
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

(defsubst herschel-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of langelem's relpos.
Leaves point at the relpos unless preserve-point is non-nil."
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here)))))

(defun herschel-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Nicked from `c-lineup-C-comments'."
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (herschel-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (herschel-point 'eol) t))
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

(defconst herschel-beg-block-re "{\\|\\<\\(begin\\|fork\\)\\>")

(defconst herschel-end-block-re "}\\|\\<\\(end\\|join\\(\\s-+\\(all\\|any\\|none\\)\\)?\\)\\>")

(defconst herschel-beg-substatement-re "\\<\\(else\\|for\\|if\\|repeat\\|while\\)\\>")

(defun herschel-corresponding-begin ()
  "Find corresponding block begin if cursor is at a block end."
  (while (and (herschel-re-search-backward
	       (concat "\\(" herschel-end-block-re "\\)\\|" herschel-beg-block-re)
	       nil t)
	      (match-string 1))
    (herschel-corresponding-begin))
  (herschel-beginning-of-substatement))

(defun herschel-corresponding-if ()
  "Find corresponding `if' if cursor is at `else'."
  (while (and (herschel-re-search-backward "}\\|\\<\\(if\\|else\\)\\>" nil t)
	      (not (equal (match-string 0) "if")))
    (if (equal (match-string 0) "else")
	(herschel-corresponding-if)
      (forward-char)
      (backward-sexp))))

(defun herschel-beginning-of-statement ()
  "Go to beginning of current statement."
  (let (pos)
    (while
	(progn
	  ;; search for end of previous statement
	  (while
	      (and (herschel-re-search-backward
		    (concat "[',]\\|" herschel-beg-block-re
			    "\\|" herschel-end-block-re) nil t)
		   (equal (match-string 0) ")"))
	    (forward-char)
	    (backward-sexp))
	  (setq pos (match-beginning 0))
	  ;; go back to beginning of current statement
	  (goto-char (or (match-end 0) 0))
	  (herschel-forward-syntactic-ws nil t)
	  (when (looking-at "(")
	    (forward-sexp)
	    (herschel-forward-syntactic-ws nil t))
	  ;; if "else" found, go to "if" and search again
	  (when (looking-at "\\<else\\>")
	    (herschel-corresponding-if)
	    (setq pos (point))
	    t))
      ;; if search is repeated, go to beginning of last search
      (goto-char pos))))

(defun herschel-beginning-of-substatement ()
  "Go to beginning of current substatement."
  (let ((lim (point))
	pos)
  ;; go to beginning of statement
    (herschel-beginning-of-statement)
    (setq pos (point))
    ;; go forward all substatement opening statements until at LIM
    (while (and (< (point) lim)
		(herschel-re-search-forward herschel-beg-substatement-re lim t))
      (setq pos (match-beginning 0)))
    (herschel-forward-syntactic-ws nil t)
    (when (looking-at "(")
      (forward-sexp)
      (herschel-forward-syntactic-ws nil t))
    (when (< (point) lim)
      (setq pos (point)))
    (goto-char pos)))

(defun herschel-forward-statement ()
  "Move forward one statement."
  (interactive)
  (while (and (herschel-re-search-forward
	       (concat "[(;]\\|" herschel-beg-block-re "\\|" herschel-end-block-re)
	       nil t)
	      (equal (match-string 0) "("))
    (backward-char)
    (forward-sexp))
  (herschel-beginning-of-substatement))

(defun herschel-backward-statement ()
  "Move backward one statement."
  (interactive)
  (herschel-backward-syntactic-ws nil t)
  (unless (= (preceding-char) ?\))
    (backward-char))
  (herschel-beginning-of-substatement))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax analysis

(defmacro herschel-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in symbol to the syntax list.
try to increase performance by using this macro."
  `(setq syntax (cons (cons ,symbol ,(or relpos 0)) syntax)))

(defun herschel-guess-basic-syntax ()
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
	(herschel-add-syntax 'comment (herschel-point 'boi)))
       ;; CASE 2: in a string?
       ((nth 3 state)
	(herschel-add-syntax 'string))
       ;; CASE 3: at a directive?
       ((save-excursion (back-to-indentation) (= (following-char) ?\#))
	(herschel-add-syntax 'directive (point)))
       ;; CASE 4: after an opening parenthesis (argument list continuation)?
       ((= (char-after (nth 1 state)) ?\()
	(goto-char (1+ (nth 1 state)))
	;; is there code after the opening parenthesis on the same line?
	(if (looking-at "\\s-*$")
	    (herschel-add-syntax 'arglist-cont (herschel-point 'boi))
	  (herschel-add-syntax 'arglist-cont-nonempty (point))))
       ;; CASE 5: at a block closing?
       ((save-excursion (back-to-indentation) (looking-at herschel-end-block-re))
	;; look for the corresponding begin
	(herschel-corresponding-begin)
	(herschel-add-syntax 'block-close (herschel-point 'boi)))
       ;; CASE 6: at a block intro (the first line after a block opening)?
       ((and (save-excursion
	       (herschel-backward-syntactic-ws nil t)
	       ;; previous line ends with a block opening?
	       (or (/= (skip-chars-backward "{") 0) (backward-word 1))
	       (when (looking-at herschel-beg-block-re)
		 ;; go to beginning of substatement
		 (herschel-beginning-of-substatement)
		 (setq placeholder (point))))
	     ;; not if "fork" is followed by "{"
	     (save-excursion
	       (not (and (progn (back-to-indentation) (looking-at "{"))
			 (progn (goto-char placeholder)
				(looking-at "\\<fork\\>"))))))
	(goto-char placeholder)
	(herschel-add-syntax 'block-intro (herschel-point 'boi)))
       ;; CASE 7: at the beginning of an else clause?
       ((save-excursion (back-to-indentation) (looking-at "\\<else\\>"))
	;; find corresponding if
	(herschel-corresponding-if)
	(herschel-add-syntax 'else-clause (herschel-point 'boi)))
       ;; CASE 8: at the beginning of a statement?
       ;; is the previous command completed?
       ((or (save-excursion
	      (herschel-backward-syntactic-ws nil t)
	      (setq placeholder (point))
	      ;; at the beginning of the buffer?
	      (or (bobp)
		  ;; previous line ends with a semicolon or
		  ;; is a block opening or closing?
		  (when (or (/= (skip-chars-backward "{};") 0)
			    (progn (back-to-indentation)
				   (looking-at (concat herschel-beg-block-re "\\|"
						       herschel-end-block-re))))
		    ;; if at a block closing, go to beginning
		    (when (looking-at herschel-end-block-re)
		      (herschel-corresponding-begin))
		    ;; go to beginning of the statement
		    (herschel-beginning-of-statement)
		    (setq placeholder (point)))
		  ;; at a directive?
		  (when (progn (back-to-indentation) (looking-at "#"))
		    ;; go to previous statement
		    (herschel-beginning-of-statement)
		    (setq placeholder (point)))))
	    ;; at a block opening?
	    (when (save-excursion (back-to-indentation)
				  (looking-at herschel-beg-block-re))
	      ;; go to beginning of the substatement
	      (herschel-beginning-of-substatement)
	      (setq placeholder (point))))
	(goto-char placeholder)
	(herschel-add-syntax 'statement (herschel-point 'boi)))
       ;; CASE 9: at the beginning of a substatement?
       ;; is this line preceeded by a substatement opening statement?
       ((save-excursion (herschel-backward-syntactic-ws nil t)
			(when (= (preceding-char) ?\)) (backward-sexp))
			(backward-word 1)
			(setq placeholder (point))
			(looking-at herschel-beg-substatement-re))
	(goto-char placeholder)
	(herschel-add-syntax 'substatement (herschel-point 'boi)))
       ;; CASE 10: it must be a statement continuation!
       (t
	;; go to beginning of statement
	(herschel-beginning-of-substatement)
	(herschel-add-syntax 'statement-cont (herschel-point 'boi))))
      syntax)))



(defun herschel-forward-comment ()
  "ashddhaksdasldashk"
  (interactive)
  (forward-comment -1))

;; ----------------------------------------------------------------------
(defun herschel-indent-line ()
  "Indent the current line as herschel code. Optional SYNTAX is the
syntactic information for the current line. Returns the amount of
indentation change (in columns)."
  (interactive)
  (let* ((syntax (herschel-guess-basic-syntax))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'herschel-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (when herschel-echo-syntactic-information-p
      (message "syntax: %s, indent= %d" syntax indent))
    (unless (zerop shift-amt)
      (beginning-of-line)
      (delete-region (point) (herschel-point 'boi))
      (indent-to indent))
    (if (< (point) (herschel-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    shift-amt))

(defun herschel-indent-buffer ()
  "Indent whole buffer as Herschel code.
Calls `indent-region' for whole buffer."
  (interactive)
  (message "Indenting buffer...")
  (indent-region (point-min) (point-max) nil)
  (message "Indenting buffer...done"))

(defun herschel-indent-region (start end column)
  "Indent region as Herschel code."
  (interactive "r\nP")
  (message "Indenting region...")
  (indent-region start end column)
  (message "Indenting region...done"))

(defsubst herschel-indent-block-closing ()
  "If previous word is a block closing or `else', indent line again."
  (when (= (char-syntax (preceding-char)) ?w)
    (save-excursion
      (backward-word 1)
      (when (and (not (herschel-in-literal))
		 (looking-at (concat herschel-end-block-re "\\|\\<else\\>")))
	(herschel-indent-line)))))











;;; ----------------------------------------------------------------------
;; Comments
;;; ----------------------------------------------------------------------
(defun herschel-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end -1)
    (comment-region beg end)))


;;; ----------------------------------------------------------------------
;;; Help functions
;;; ----------------------------------------------------------------------
(defun herschel-customize ()
  "Call the customize function with `herschel' as argument."
  (interactive)
  (customize-browse 'herschel))


;;; ----------------------------------------------------------------------
;;; Other
;;; ----------------------------------------------------------------------
;; remove ".herschel" and ".mod" from `completion-ignored-extensions'
(setq completion-ignored-extensions
      (delete ".herschel" completion-ignored-extensions))
(setq completion-ignored-extensions
      (delete ".mod" completion-ignored-extensions))


;;; ----------------------------------------------------------------------
;;; Bug reports
;;; ----------------------------------------------------------------------
(defconst herschel-mode-help-address "Herschel Mode Maintainer <gck@eyestep.org>"
  "Address for Herschel Mode bug reports.")

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun herschel-mode-submit-bug-report ()
  "Submit via mail a bug report on Herschel Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on Herschel Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    herschel-mode-help-address
    (concat "Herschel Mode " herschel-version)
    (list
     ;; report all important variables
     'herschel-basic-offset
     )
    nil nil
    "Dear Herschel Mode maintainer,")))

;;; ----------------------------------------------------------------------
;;; Documentation
;;; ----------------------------------------------------------------------
(defun herschel-version ()
  "Echo the current version of Herschel Mode in the minibuffer."
  (interactive)
  (message "Using Herschel Mode version %s" herschel-version))

(defun herschel-doc-mode ()
  "Display Herschel Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'herschel-mode))
    (unless herschel-xemacs
      (help-setup-xref (list #'herschel-doc-mode) (interactive-p)))
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
;; Hippie expand customization (for expansion of Herschel commands)

(defvar herschel-abbrev-list
  (append
   (list nil) herschel-keywords
	  (list nil) herschel-types
	  (list nil) herschel-builtin
	  (list nil) herschel-constants)
  "Predefined abbreviations for Herschel.")

(defvar herschel-expand-upper-case nil)

(eval-when-compile (require 'hippie-exp))

(defun herschel-try-expand-abbrev (old)
  "Try expanding abbreviations from `herschel-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list herschel-abbrev-list)
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
      (setq herschel-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if herschel-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; function for expanding abbrevs and dabbrevs
(defun herschel-expand-abbrev (arg))
(fset 'herschel-expand-abbrev (make-hippie-expand-function
			       '(try-expand-dabbrev
				 try-expand-dabbrev-all-buffers
				 herschel-try-expand-abbrev)))

(provide 'herschel-mode)

;;; herschel-mode.el ends here
