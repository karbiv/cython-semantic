;;; cython-semantic-mode.el --- Major mode for editing Cython/Python files

(require 'python) ; Built-in python mode
(eval-when-compile
  (require 'rx))
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
    (define-key map [remap forward-sentence] 'cython-nav-forward-block)
    map)
  "Keymap used in `cython-mode'.")

(eval-when-compile
  (defconst cython-rx-constituents
    `((block-start . ,(rx symbol-start
                          (or "def" "class" "cdef" "cpdef" "ctypedef" "if" "elif"
                              "else" "try" "except" "finally" "for" "while" "with")
                          symbol-end))
      (defun       . ,(rx symbol-start (or "def" "class") symbol-end)))
    "Additional Cython specific sexps for `cython-rx'")

  (defmacro cython-rx (&rest regexps)
    "Cython mode specialized rx macro."
    (let ((rx-constituents (append cython-rx-constituents rx-constituents)))
      (cond ((cdr regexps) (rx-to-string `(and ,@regexps) t))
            (t (rx-to-string (car regexps) t))))))

;; redefine
;; used in indenting
(defun python-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (python-nav-beginning-of-statement)
          ;; Cython's block-start
          (looking-at (cython-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (python-nav-beginning-of-statement) nil)
                      (python-info-current-line-comment-p)
                      (python-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) python-indent-offset)))
        (while
            (and (python-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (cython-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

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

(defun cython-comment-line-p ()
  "Return non-nil if current line is a comment."
  (save-excursion
    (back-to-indentation)
    (eq ?# (char-after (point)))))

(defun cython-in-string/comment ()
  "Return non-nil if point is in a comment or string."
  (nth 8 (syntax-ppss)))

(defun cython-beginning-of-defun ()
  "`beginning-of-defun-function' for Cython.
Finds beginning of innermost nested class or method definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (let ((ci (current-indentation))
        (def-re (rx line-start (0+ space) (or "def" "cdef" "cpdef" "class") (1+ space)
                    (group (1+ (or word (syntax symbol))))))
        found lep) ;; def-line
    (if (cython-comment-line-p)
        (setq ci most-positive-fixnum))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      ;;(setq def-line (looking-at def-re))
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
               ;; Must be less indented or matching top level, or
               ;; equally indented if we started on a definition line.
               (let ((in (current-indentation)))
                 (or (and (zerop ci) (zerop in))
                     (= lep (line-end-position)) ; on initial line
                     ;; Not sure why it was like this -- fails in case of
                     ;; last internal function followed by first
                     ;; non-def statement of the main body.
                     ;;(and def-line (= in ci))
                     (= in ci)
                     (< in ci)))
               (not (cython-in-string/comment)))
          (setq found t)))))

(defvar cython-font-lock-keywords
  `(;; ctypedef statement: "ctypedef (...type... alias)?"
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
          (or "by" "cdef" "cimport" "cpdef"
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
          (or
           "object" "dict" "list"
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

(defun cython-current-defun ()
  "`add-log-current-defun-function' for Cython."
  (save-excursion
    ;; Move up the tree of nested `class' and `def' blocks until we
    ;; get to zero indentation, accumulating the defined names.
    (let ((start t)
          accum)
      (while (or start (> (current-indentation) 0))
        (setq start nil)
        (cython-beginning-of-block)
        (end-of-line)
        (beginning-of-defun)
        (if (looking-at (rx (0+ space) (or "def" "cdef" "cpdef" "class") (1+ space)
                            (group (1+ (or word (syntax symbol))))))
            (push (match-string 1) accum)))
      (if accum (mapconcat 'identity accum ".")))))

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
  semantic-lex-default-action
  )

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

(defun semantic-cython-expand-tag (tag)
  "Expand compound declarations found in TAG into separate tags.
TAG contains compound declaration if the NAME part of the tag is a list.
In cython, this can happen with `import' and `cdef' statements."
  ;;TODO: add 'cdef' processing, 'cdef tuple item, val ='
  (let ((class (semantic-tag-class tag))
	(elts (semantic-tag-name tag))
	(expand nil))
    (cond
     ((and (eq class 'include) (listp elts))
      (dolist (E elts)
	(setq expand (cons (semantic-tag-clone tag E) expand)))
      (setq expand (nreverse expand)))
     )))

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
   semantic-tag-expand-function 'semantic-cython-expand-tag

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
				      (cdef_extern . "Extern")))
  )

(defun python-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `cython-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (cython-nav-forward-block (- arg)))

(defun cython-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (cython-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (python-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (python-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (python-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (python-syntax-context-type)))
      (setq arg (1+ arg)))
    (python-nav-beginning-of-statement)
    (if (not (looking-at (cython-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

;; Init Jedi.el package
(defun cython-check-jedi-package ()
  (when (symbol-function 'jedi:setup)
    (jedi:setup)
    ;; Unhide Semantic's 'C-c ,' key prefix,
    ;; default 'C-c ,,' reparses a buffer
    (define-key jedi-mode-map (kbd "C-c ,") nil)
    ;; Jedi uses 'C-c ,' for `jedi:goto-definition-pop-marker', redefine
    (define-key jedi-mode-map (kbd "C-c p") 'jedi:goto-definition-pop-marker)
    ))

;;;###autoload
(define-derived-mode cython-semantic-mode python-mode "Cython"
  "Major mode for Cython development.

\\{cython-semantic-mode-map}"
  (semantic-mode 1)
  (set (make-local-variable 'tab-width) 4)
  (font-lock-add-keywords nil cython-font-lock-keywords)
  (set (make-local-variable 'beginning-of-defun-function)
       #'cython-beginning-of-defun)

  (add-to-list 'semantic-new-buffer-setup-functions
	       (cons 'cython-semantic-mode 'wisent-cython-default-setup))
  
  (set (make-local-variable 'compile-command)
       (format cython-default-compile-format (shell-quote-argument buffer-file-name)))
  (add-to-list (make-local-variable 'compilation-finish-functions)
               'cython-compilation-finish)
  (set (make-local-variable 'add-log-current-defun-function)
       #'cython-semantic-current-defun)
  ;; Best 'go to definition' for python libraries
  (cython-check-jedi-package)

  ;; cython-semantic-features
  (cython-if-global-semanticdb-minor-mode))

(provide 'cython-semantic-mode)
