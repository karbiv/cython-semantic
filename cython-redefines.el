
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

(defvar python-nav-beginning-of-defun-regexp
  (cython-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

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

(defun python-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
	 ;; use cython-rx
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
    ;; use cython-rx
    (if (not (looking-at (cython-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(provide 'cython-redefines)
