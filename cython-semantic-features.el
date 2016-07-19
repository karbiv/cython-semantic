
(defvar cython-enables-global-semanticdb-minor-mode t
  "If true, `cython-semantic-mode' turns on `global-semantic-minor-mode'.")

(when (require 'semantic/util-modes)
  (add-hook 'semantic-init-mode-hook #'cython-semantic-util-minor-modes))

(when (require 'semantic/decorate/mode)
  (add-hook 'semantic-init-mode-hook #'cython-semantic-decorate-mode))

(defun cython-semantic-util-minor-modes ()
  (semantic-stickyfunc-mode 1)
  (semantic-highlight-func-mode 1))

(defun cython-semantic-decorate-mode ()
  (semantic-decoration-mode 1))

(defun cython-if-global-semanticdb-minor-mode ()
  "Enable `global-semanticdb-minor-mode' if
`cython-enables-global-semanticdb-minor-mode' is t."
  (when (and (not global-semanticdb-minor-mode)
	     cython-enables-global-semanticdb-minor-mode)
    (global-semanticdb-minor-mode 1)))

;;***********************************************

(defun cython-semantic-current-defun ()
  )

(provide 'cython-semantic-features)
