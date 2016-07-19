
;; Emacs' built-in python mode
(require 'python)

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

;;******************************
;; Redefines of the python mode

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


(provide 'cython-indent)
