;;; cython-semantic-mode.el --- Major mode for editing Cython/Python files

(require 'python) ; Built-in python mode
(eval-when-compile
  (require 'rx))
(require 'cython-redefines)
(require 'semantic/wisent/python)
(require 'cython-wy)
(require 'cython-semantic-features)

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-semantic-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-semantic-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-semantic-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . cython-semantic-mode))

(defvar cython-buffer nil
  "Variable pointing to the cython buffer which was compiled.")

;; used automatically by `define-derived-mode
(defvar cython-semantic-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Will inherit from `python-mode-map' thanks to define-derived-mode.
    (define-key map "\C-c\C-c" 'cython-compile)
    map)
  "Keymap used in `cython-mode'.")


(defun cython-compile ()
  "Compile the file via Cython."
  (interactive)
  (let ((cy-buffer (current-buffer)))
    (with-current-buffer
        (compile compile-command)
      (set (make-local-variable 'cython-buffer) cy-buffer)
      (add-to-list (make-local-variable 'compilation-finish-functions)
                   'cython-compilation-finish))))


(defun cython-compilation-finish (buffer how)
  "Called when Cython compilation finishes."
  ;; XXX could annotate source here
  )


;;; Font Lock functions.
;; These functions must be robust, they are called by Font Lock.


(defun cython-font-lock-pre-anchor ()
  (let ((m-beg (match-beginning 1)) (m-end (match-end 1)))
    ;; propertize matched cdef or cpdef
    (add-text-properties m-beg m-end
			 `(face font-lock-keyword-face rear-sticky nil))))


(defun cython-collect-cdef ()
  "Advance point excursion to a right end of a cdef/cpdef declaration to be highlighted.
Returns declaration variant as a self-evaluated symbol,
`:assignment', `:block', `:function', `:simple'.
Recursive for multiline declarations(backslashed newline)."
  (search-forward-regexp "\\([^(=#\n]*\\)" (point-max) t)
  (let ((matched (match-string-no-properties 1)))
    (if (not (string-empty-p matched))
	(let ((last-char (string-to-char (substring matched -1))))
	  (cond
	   ((char-equal ?= (char-after))
	    :assignment)
	   ((char-equal ?: last-char)
	    :block)
	   ((char-equal ?\\ last-char) ; escaped newline, recurse for next lines
	    (forward-char)
	    (cython-collect-cdef))
	   ((char-equal ?\( (char-after)) ; func args open paren
	    ;;TODO highlight typed func arguments
	    ;; highlight only func type and name for now
	    :function)
	   (t ; ends on newline
	    :simple))))))


(defun cython-propertize-function (beg end)
  (goto-char beg)
  (let ((num-parts 0) (point-pairs nil))
    (while (< (point) end)
      (let (left right)
	(skip-chars-forward "^a-zA-Z0-9_" end) (setq left (point))
	(skip-chars-forward "a-zA-Z0-9_" end) (setq right (point))
	(when (not (equal left right))
	  (incf num-parts)
	  (push (cons left right) point-pairs))))
    (add-text-properties (caar point-pairs) (cdar point-pairs)
			 `(face font-lock-function-name-face rear-sticky nil))
    (if (> num-parts 1) ; return type
	(let ((left (car (cadr point-pairs)))
	      (right (cdr (cadr point-pairs))))
	  (unless (member (buffer-substring-no-properties left right)
			  cython-font-lock-builtin-types-rx)
	    (add-text-properties left right
				 `(face font-lock-type-face rear-sticky nil)))))))


(defvar cython-font-lock-builtin-types-rx
  '("object" "dict" "list" "tuple"
    ;; basic c type names
    "void" "char" "int" "float" "double" "bint"
    ;; longness/signed/constness
    "signed" "unsigned" "long" "short"
    ;; special basic c types
    "size_t" "Py_ssize_t" "Py_UNICODE" "Py_UCS4" "ssize_t" "ptrdiff_t"))


(defun cython-propertize-type (beg)
  (let (left right)
    (goto-char beg)
    (skip-chars-forward "^a-zA-Z0-9_" end) (setq left (point))
    (skip-chars-forward "a-zA-Z0-9_" end) (setq right (point))
    (unless (member (buffer-substring-no-properties left right)
		    cython-font-lock-builtin-types-rx)
      (add-text-properties left right
			   `(face font-lock-type-face rear-sticky nil)))))


(defun cython-find-type (beg end)
  (goto-char beg)
  (if (search-forward "," end t)
      (let ((parts (split-string (buffer-substring-no-properties beg end) "," t))) 
	(if (> (length parts) 1)
	    (let ((head (split-string (car parts) "[ \f\t\n\r\v]+" t)))
	      (if (> (length head) 1)
		  (cython-propertize-type beg)))))
    ;; else, decl without comma
    (let ((head (split-string (buffer-substring-no-properties beg end) "[ \f\t\n\r\v]+" t)))
      (if (> (length head) 1)
	  (cython-propertize-type beg)))))


(defun cython-propertize-cdef (anchor beg end decl-variant)
  "Highlight Cython types for cdef and cpdef.
BEG and END are buffer points of declaration, starting after ANCHOR.
ANCHOR is 'cdef' or 'cpdef'.
DECL-VARIANT is one of :symbols returned by `cython-collect-cdef'."
  (save-excursion
    (if (equal anchor "cdef")
	(cond
	 ((eq :function decl-variant)
	  (cython-propertize-function beg end))
	 ;;font-lock-type-face
	 ((eq :simple decl-variant)
	  ;;(cython-propertize-simple-decl beg end)
	  (cython-find-type beg end))
	 ((eq :assignment decl-variant)
	  (cython-find-type beg end)
	  ))
      ;; else for "cpdef"
      (when (eq :function decl-variant)
	(cython-propertize-function beg end)))))


(defun cython-font-lock-cdef-anchor (limit)
  "For `cython-font-lock-keywords', anchored function.
Must return `nil' when done."
  (let ((beg (point)) 
	(anchor (match-string-no-properties 1)) ; anchor is "cdef" or "cpdef"
	(decl-variant (cython-collect-cdef))) ; advances point
    (cython-propertize-cdef anchor beg (point) decl-variant))
  ;; return nil for the Font Lock to proceed, `font-lock-fontify-anchored-keywords'.
  nil)


(defvar cython-font-lock-keywords
  `(;; anchored-highlighter for cdef and cpdef
    (,(rx symbol-start (group (regexp "c\\(?:p\\)?def")))
     (cython-font-lock-cdef-anchor (cython-font-lock-pre-anchor) nil nil))
    ;; ctypedef statement: "ctypedef (...type... alias)?"
    (,(rx
       ;; keyword itself
       symbol-start (group "ctypedef")
       ;; type specifier: at least 1 non-identifier symbol + 1 identifier
       ;; symbol and anything but a comment-starter after that.
       (opt (regexp "[^a-zA-z0-9_\n]+[a-zA-Z0-9_][^#\n]*")
            ;; type alias: an identifier
            symbol-start (group (regexp "[a-zA-Z_]+[a-zA-Z0-9_]*"))
            ;; space-or-comments till the end of the line
            (* space) (opt "#" (* nonl)) line-end))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil 'noerror))
    ;; new keywords in Cython language
    (,(rx symbol-start
          (or "by" "cimport"
              "extern" "gil" "include" "nogil" "property" "public"
              "readonly" "DEF" "IF" "ELIF" "ELSE"
              "new" "del" "cppclass" "namespace" "const"
              "__stdcall" "__cdecl" "__fastcall" "inline" "api")
          symbol-end)
     . font-lock-keyword-face)
    ;; Question mark won't match at a symbol-end, so 'except?' must be
    ;; special-cased.  It's simpler to handle it separately than weaving it
    ;; into the lengthy list of other keywords.
    (,(rx symbol-start "except?") . font-lock-keyword-face)
    ;; C and Python types (highlight as builtins)
    (,(rx symbol-start
          (or "object" "dict" "list" "tuple"
	      ;; basic c type names
	      "void" "char" "int" "float" "double" "bint"
	      ;; longness/signed/constness
	      "signed" "unsigned" "long" "short"
	      ;; special basic c types
	      "size_t" "Py_ssize_t" "Py_UNICODE" "Py_UCS4" "ssize_t" "ptrdiff_t")
          symbol-end)
     . font-lock-builtin-face)
    (,(rx symbol-start "NULL" symbol-end)
     . font-lock-constant-face)
    ;; cdef is used for more than functions, so simply highlighting the next
    ;; word is problematic. struct, enum and property work though.
    (,(rx symbol-start
          (group (or "struct" "enum" "union"
                     (seq "ctypedef" (+ space "fused"))))
          (+ space) (group (regexp "[a-zA-Z_]+[a-zA-Z0-9_]*")))
     (1 font-lock-keyword-face prepend) (2 font-lock-type-face))
    ("\\_<property[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     1 font-lock-function-name-face))
  "Additional font lock keywords for Cython mode.")

;; For functions that are named identically to a language reserved keyword,
;; for example 'def property()', checks previous token in the stream,
;; if it's DEF, then the current one can be only a symbol, so skip keyword scan
(define-lex-analyzer cython-<keyword>-analyzer "Skips keyword lexer if previous token is DEF"
  (and
   (looking-at "\\(\\sw\\|\\s_\\)+")
   (not (eq 'DEF (caar semantic-lex-token-stream)))
   (let ((key (semantic-lex-keyword-p (match-string 0))))
     (when key
       (semantic-lex-push-token
	(semantic-lex-token key (match-beginning 0) (match-end 0)))))))

;; overriden to fix a bug in return ("\\", "/") 
(defconst wisent-python-string-re
  (rx
   (opt (any "uU")) (opt (any "rR"))
   (or
    ;; Triple-quoted string using apostrophes
    (: "'''" (zero-or-more (or "\\'"
                               (not (any "'"))
                               (: (repeat 1 2 "'") (not (any "'")))))
       "'''")
    ;; String using apostrophes
    (: "'" (zero-or-more (or (: (not (any "\\")) "\\'")
                             (not (any "'"))))
       "'")
    ;; Triple-quoted string using quotation marks.
    (: "\"\"\"" (zero-or-more (or "\\\""
                                  (not (any "\""))
                                  (: (repeat 1 2 "\"") (not (any "\"")))))
       "\"\"\"")
    ;; String using quotation marks.
    (: "\"" (zero-or-more (or (: (not (any "\\\"")) "\\\"")
                              (not (any "\""))))
       "\"")))
  "Regexp matching a complete Python string.")

(define-lex cython-lex
  "Cython lexer"
  ;; Must analyze beginning of line first to handle indentation.
  wisent-python-lex-beginning-of-line
  wisent-python-lex-end-of-line
  ;; Must analyze string before symbol to handle string prefix.
  wisent-python-lex-string
  cython-wy--<number>-regexp-analyzer
  cython-<keyword>-analyzer
  cython-wy--<symbol>-regexp-analyzer
  cython-wy--<block>-block-analyzer
  cython-wy--<punctuation>-string-analyzer
  ;; Ignored things.
  wisent-python-lex-ignore-backslash
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-comments
  ;; Signal error on unhandled syntax.
  semantic-lex-default-action)

(define-mode-local-override semantic-lex cython-semantic-mode
  (start end &optional depth length)
  "Lexically analyze Cython code in current buffer.
See the function `semantic-lex' for the meaning of the START, END,
DEPTH and LENGTH arguments.
This function calls `cython-lex' to actually perform the
lexical analysis, then emits the necessary Python DEDENT tokens from
what remains in the `wisent-python-indent-stack'."
  (let* ((wisent-python-indent-stack (list 0))
         (stream (cython-lex start end depth length))
         (semantic-lex-token-stream nil))
    ;; Emit DEDENT tokens if something remains in the INDENT stack.
    (while (> (pop wisent-python-indent-stack) 0)
      (semantic-lex-push-token (semantic-lex-token 'DEDENT end end)))
    (nconc stream (nreverse semantic-lex-token-stream))))

;;;###autoload
(defun wisent-cython-default-setup ()
  "Setup buffer for parse."
  (cython-wy--install-parser)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Give python mode the possibility to overwrite this:
  (if (not comment-start-skip)
      (set (make-local-variable 'comment-start-skip) "#+\\s-*"))
  (setq
   ;; Character used to separation a parent/child relationship
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; Parsing of some one line compound statements
   semantic-tag-expand-function 'cython-expand-tags

   ;; Semantic to take over from the one provided by python.
   ;; The python one, if it uses the senator advice, will hang
   ;; Emacs unrecoverably.
   imenu-create-index-function 'semantic-create-imenu-index
   
   semantic-symbol->name-assoc-list-for-type-parts '((variable . "Data attributes")
						     (function . "Methods"))
   semantic-symbol->name-assoc-list '((type . "Types")
				      (variable . "Globals")
				      (function . "Functions")
				      (include  . "Imports")
				      (package  . "Package")
				      (code . "Code")
				      (cdef_extern . "Extern"))))

(defun cython-end-of-defun ()
  (let ((curr-tag (semantic-current-tag)))
    (when curr-tag
      (goto-char (- (semantic-tag-end curr-tag) 2)))))

;; instead of (define-mode-local-override semantic-up-context cython (&optional point bounds-type)
(defun cython-up-context ()
  "Move point up one context from POINT.
Return non-nil if there are no more context levels.
This will find a tag of `function' or `type'
class and make sure non-nil is returned if you cannot
go up past the bounds of that tag."
  (let ((tags (semantic-find-tag-by-overlay)))
    (if tags (setq tags (nreverse tags))
      ;; No overlays found, then find previous from point
      (setq tags (nreverse (semantic-find-tag-by-overlay
			    (- (semantic-overlay-previous-change (point)) 1)))))
    (while (and tags (not (member (semantic-tag-class (car tags)) '(function type))))
      (setq tags (cdr tags)))
    (let ((curr-tag (car tags))
	  (parent-name (semantic-tag-named-parent (car tags)))) 
      (if (and (= (point) (semantic-tag-start curr-tag))
	       parent-name
	       (eq this-command 'beginning-of-defun))
	  (progn
	    (while (and tags (not (equal parent-name (caar tags))))
	      (setq tags (cdr tags)))
	    (goto-char (semantic-tag-start (car tags))))
	(and (not (= (point) (semantic-tag-start curr-tag)))
	     (eq this-command 'beginning-of-defun))
	(goto-char (semantic-tag-start curr-tag)))
      ;; Return non-nil if there are no more context levels.
      (if (semantic-tag-named-parent (car tags)) nil t))))

;; to create overlays for inner defs and classes
(define-mode-local-override semantic-tag-components cython-semantic-mode (tag)
  "Return a list of components for TAG.
Perform the described task in `semantic-tag-components'."
  (cond ((semantic-tag-of-class-p tag 'type)
	 (semantic-tag-type-members tag))
	((semantic-tag-of-class-p tag 'function)
	 (let ((all-tags (semantic-tag-function-arguments tag))
	       (suite (semantic-tag-get-attribute tag :suite)))
	   (while suite
	     (when (and (semantic-tag-p (car suite))
			(member (semantic-tag-class (car suite)) '(function type)))
	       (setq all-tags (cons (car suite) all-tags)))
	     (setq suite (cdr suite)))
	   all-tags))
	(t nil)))

(defun cython-check-jedi-package ()
  "Init Jedi.el package"
  (when (symbol-function 'jedi:setup)
    (jedi:setup)
    ;; Unhide Semantic's 'C-c ,' key prefix,
    ;; default 'C-c ,,' reparses a buffer
    (define-key jedi-mode-map (kbd "C-c ,") nil)
    ;; Jedi uses 'C-c ,' for `jedi:goto-definition-pop-marker', redefine
    (define-key jedi-mode-map (kbd "C-c p") 'jedi:goto-definition-pop-marker)))

;;;###autoload
(define-derived-mode cython-semantic-mode python-mode "Cy"
  "Major mode for Cython development.

\\{cython-semantic-mode-map}"  
  (semantic-mode)

  (set (make-local-variable 'tab-width) 4)
  (font-lock-add-keywords nil cython-font-lock-keywords)
  (set (make-local-variable 'beginning-of-defun-function) #'cython-up-context)
  (set (make-local-variable 'end-of-defun-function) #'cython-end-of-defun)

  (add-to-list 'semantic-new-buffer-setup-functions
	       (cons 'cython-semantic-mode 'wisent-cython-default-setup))
  
  ;; (set (make-local-variable 'compile-command)
  ;;      (format cython-default-compile-format (shell-quote-argument buffer-file-name)))
  ;; (add-to-list (make-local-variable 'compilation-finish-functions)
  ;;              'cython-compilation-finish)
  (set (make-local-variable 'add-log-current-defun-function)
       #'cython-semantic-current-defun)
  ;; Best 'go to definition' for python libraries
  (cython-check-jedi-package)

  ;; cython-semantic-features
  
  (advice-add 'semantic-stickyfunc-fetch-stickyline
	      :around #'cython-stickyfunc-fetch-stickyline)
  (advice-add 'semantic-highlight-func-highlight-current-tag
	      :around #'cython-highlight-func-highlight-current-tag))

(provide 'cython-semantic-mode)
