;;; haskell.el --- major mode for editing Haskell

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages
;; Created: Sept 2003
;; $Revision: 1.4 $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a fairly simple mode for Haskell, mainly for use with my
;; haskell-latex mode.  It has some simple Imenu support, but can also
;; use haskell-decl-scan.el and haskell-doc.el from haskell.org.  The
;; sophisticated indentation support from haskell-indent.el needs
;; reworking to fit in with narrowing to the chunk done by
;; haskell-latex.el.

;; There is some support for the Bird-style `inverse comment'
;; convention for simple literate programming.  Everything outside the
;; `>'-delimited regions is treated as comments.  This relies on
;; font-lock being turned on to add syntax properties.

;;; Code:

(defvar haskell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\'" table) ; ??
    (modify-syntax-entry ?\` "$`" table)
    (mapc (lambda (x) (modify-syntax-entry x "_" table)) "!#$%.:?@^~")
    ;; The following get comment syntax right, similarly to C++ (but
    ;; the `b' style needs to be the other way round).  Proper
    ;; treatment of comments requires syntactic font-lock support.
    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  "_ 123" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table used in Haskell mode.")

(defconst haskell-syntactic-keywords
  '(
    ;; Character constants.  (Apostrophe can't be given string syntax
    ;; since it's an id constituent.)
    ("\\Sw\\('\\)\\([^\\']\\|\\\\.\\|\\\\[[:alnum:]]+\\)\\('\\)" (1 "|") (3 "|"))
    ;; Deal with instances of `--' which don't delimit a comment, e.g. `-->'.
    ("\\s_\\{3,\\}" (0 (unless (string-match "\\`-*\\'" (match-string 0))
			 ;; Do nothing in case of things like `{---'.
			 "_"))))
  "Basic `font-lock-syntactic-keywords' for Haskell.")

(defconst haskell-bird-syntactic-keywords
  '(("^\\([^\n>]\\)[^\n]*\\(\n\\)"  (1 "!") (2 "!")))
  "Additional `font-lock-syntactic-keywords' for Bird-style literate Haskell.")

;; Should probably just set bird and non-bird versions of
;; font-lock-keywords and imenu-generic-expression at load time.
(defun haskell-set-patterns (bird)
  "Set font-lock and imenu patterns.
BIRD non-nil means use Bird-style literate commenting."
  ;; The \b at the end here makes sense even with a trailing ' or _
  ;; because of the font-lock and imenu syntax definitions.
  (let* ((id "\\b[[:lower:]_][[:alnum:]'_]*\\b")
	 (cid "\\b[[:upper:]][[:alnum:]'_]*\\b")
	 ;; Add backslash to the symbol-syntax chars.  This seems to
	 ;; be thrown for some reason by backslash's escape syntax.
	 (sym "\\(?:\\s_\\|\\\\\\)+")
	 (lead (if bird "^> " "^")))
    (set (make-local-variable 'font-lock-keywords)
	 `((,(concat lead "#.*$") . 'font-lock-preprocessor-face)
	   ,(eval-when-compile
	      (regexp-opt
	       ;;`_' can go in here since it has temporary word syntax.
	       '("case" "class" "data" "default" "deriving" "do" "else"
		 "if" "import" "in" "infix" "infixl" "infixr" "instance"
		 "let" "module" "newtype" "of" "then" "type" "where" "_"
		 ;; These aren't in the Haskell98 reserved list and
		 ;; probably shouldn't be treated as keywords outside
		 ;; import declarations.
		 ;; "as" "hiding" "qualified"
		 )
	       'words))
	   ;; Reserved operations.  (Would lose at bol.)
	   (,(eval-when-compile
	       (concat
		"\\S_"
		(regexp-opt '(".." "::" "=" "\\" "|" "<-" "->" "@" "~" "=>") t)
		"\\S_"))
	    1 'font-lock-keyword-face)
	   ,@(if bird '(("^>" (0 'default t))))
	   ,(concat "`" id "`")	; Like haskell-font-lock, but dubious.
	   (,(concat "\\S_\\(:\\(?:" sym "\\)?\\)") 1 'font-lock-keyword-face)
	   ;; Fairly simple and probably not very reliable treatment
	   ;; of declarations.
	   (,(concat lead "\\(" id "\\)\\s-*\\(?:" id "\\|::\\|=\\||"
		     ;; (start of) numeric, char or string
		     "\\|[0-9\"']\\)")
	    1 'font-lock-variable-name-face)
	   (,(concat lead "\\(?:" id "\\)\\s-*\\(" sym "\\)")
	    1 'font-lock-variable-name-face)
	   (,(concat lead "\\(" id "\\)\\s-*\\s(")
	    1 'font-lock-variable-name-face)
	   (,(concat lead "(\\(" sym "\\))") 1 'font-lock-variable-name-face)
	   (,(concat lead "class\\>\\(?:[^=]*=>\\)?\\s-*\\(" cid "\\)")
	    1 'font-lock-type-face)
	   (,(concat lead
		     "\\(?:data\\|newtype\\|type\\)\\>\\(?:[^=]*=>\\)?\\s-*\\("
		     cid "\\)")
	    1 'font-lock-type-face)
	   (,(concat lead "module\\s-+\\([^[:space:]\n]+\\)")
	    1 'font-lock-type-face)
	   (,(concat lead
		     "import\\(?:\\s-+qualified\\)?\\s-+\\([^[:space:]\n]+\\>\\)")
	    1 'font-lock-type-face)))
    ;; Fairly simple and probably not very reliable Imenu stuff.  See
    ;; also haskell-decl-scan.el.
    (setq imenu-generic-expression
	  `((nil ,(concat lead id "\\s-*\\(" sym "\\)\\s-+" id) 1)
	    (nil ,(concat lead "\\(" id "\\)\\s-*" id) 1)
	    (nil ,(concat lead "\\(" id "\\)\\s-*\\(?:::\\|=\\||\\|[0-9\"']\\)") 1)
	    (nil ,(concat lead "\\(" id "\\)\\s-*\\s(") 1)
	    (nil ,(concat lead "(\\(" sym "\\))") 1)
	    ("Classes"
	     ,(concat lead "class\\>\\(?:[^=]*=>\\)?\\s-*\\(" cid "\\)") 1)
	    ;; Fixme: Do instances
	    ("Datatypes"
	     ,(concat lead
		      "\\(?:data\\|newtype\\|type\\)\\>\\(?:[^=]*=>\\)?\\s-*\\("
		      cid "\\)") 1)))))

(defconst haskell-font-lock-syntax
  ;; It's convenient to treat the non-ASCII punctuation characters as
  ;; symbol.  (We probably have to keep `,' and `;' as punctuation, so
  ;; we can't just consider sequences of punctuation and symbol
  ;; syntax.  We could also use categories.)
  (cons '(?_ . "w")
	(cons '(?' . "w")
	      (let (cs i lim)
		(let ((table (make-syntax-table)))
		  (map-char-table
		   (lambda (k v)
		     ;; The current Emacs 22 codebase can pass either a char
		     ;; or a char range.
		     (if (consp k)
			 (setq i (car k)
			       lim (cdr k))
		       (setq i k
			     lim k))
		     (if (<= i lim)
			 (when (and (> i 127) (equal v '(1)))
			   (push (cons i "-") cs))
		       (setq i (1+ i))))
		   (standard-syntax-table)))
		cs)))
  "Font-lock syntax alist for Haskell.")

(defun haskell-create-index-function ()
  "Create an Imenu index for Haskell.
Filter the result of `imenu--generic-function' to select only the
first occurrence of variable items in the list."
  (let ((ignore '("class" "data" "default" "import" "infix" "infixl" "infixr"
		  "instance" "module" "newtype" "type" "=" "::"))
	(last-syntax-point (point-min))
	syntax-state result)
    (dolist (elt (imenu--generic-function imenu-generic-expression))
      ;; Ignore if we've seen the name before or it's a
      ;; spuriously-matched keyword.
      (unless (or (member (car elt) ignore)
		  ;; Filter out anything that's actually in a comment
		  ;; or string.  (Imenu should have this built in.)
		  ;; Fixme: Should do so for classes &c too.
		  (when (number-or-marker-p (cdr elt))
		    (let ((syntax (parse-partial-sexp last-syntax-point
						      (cdr elt) nil nil
						      syntax-state)))
		      (setq syntax-state syntax
			    last-syntax-point (cdr elt))
		      (or (nth 3 syntax)
			  (nth 4 syntax)))))
	(push (car elt) ignore)
	(push elt result)))
    (nreverse result)))

;; Used by haskell.org add-ons.
(defvar haskell-literate nil)

(define-derived-mode haskell-mode fundamental-mode "Haskell"
  "Major mode for editing Haskell."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward "\n\n> " nil t)
	  (set (make-local-variable 'haskell-literate) 'bird))))
  (haskell-set-patterns haskell-literate)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-start-skip) "[-{]-+ [ \t]*")
  ;; Consistent with haskll.org mode:
  (set (make-local-variable 'comment-column) 40)
  ;; Fixme:
  ;;   (set (make-local-variable 'comment-indent-function) #'haskell-comment-indent)
  ;; Get help from font-lock-syntactic-keywords.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (setq font-lock-defaults
	(list font-lock-keywords nil nil haskell-font-lock-syntax
	      nil (cons 'font-lock-syntactic-keywords
			(if (eq 'bird haskell-literate)
			    (append haskell-bird-syntactic-keywords
				    haskell-syntactic-keywords)
			    haskell-syntactic-keywords))))
  (set (make-local-variable 'imenu-create-index-function)
       #'haskell-create-index-function)
  (set 'imenu-syntax-alist haskell-font-lock-syntax)
  ;; May be overridden by using haskell-indent.el.
  (set (make-local-variable 'indent-line-function) #'indent-relative))

(provide 'haskell)
;;; haskell.el ends here
