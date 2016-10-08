
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

;; override
(defun cython-stickyfunc-fetch-stickyline (orig &rest args)
  "Make the function at the top of the current window sticky.
Capture its function declaration, and place it in the header line.
If there is no function, disable the header line."
  (if (eq major-mode 'cython-semantic-mode)
	  (save-excursion
		(goto-char (window-start (selected-window)))
		(let* ((noshow (bobp))
			   (str
				(progn
				  (forward-line -1)
				  (end-of-line)
				  ;; Capture this function
				  (let* ((tag (semantic-stickyfunc-tag-to-stick))
						 (beginning nil))
					;; TAG is nil if there was nothing of the appropriate type there.
					(if (not tag)
						;; Set it to be the text under the header line
						(if noshow
							""
						  (if semantic-stickyfunc-show-only-functions-p ""
							(buffer-substring (point-at-bol) (point-at-eol))))
					  ;; Go get the first line of this tag.
					  (setq beginning (goto-char (semantic-tag-start tag)))
					  (search-forward ":" nil t)
					  ;; a fix for jit-lock mode when upper text in buffer is not fontified,
					  ;; which happens when opening a buffer with the point scrolled down,
					  ;; for example after `desktop-read'
					  (jit-lock-fontify-now beginning (point))
					  (buffer-substring beginning (point))))))
			   (start 0))
		  (setq str (replace-regexp-in-string "%" "%%" str))
		  (setq str (replace-regexp-in-string "[ \n]+" " " str)) 
		  str))
	(apply orig args)))

;; override
(defun cython-highlight-func-highlight-current-tag (orig &rest args)
  "Highlight the current tag under point.
Optional argument DISABLE will turn off any active highlight.
If the current tag for this buffer is different from the last time this
function was called, move the overlay."
  (if (eq major-mode 'cython-semantic-mode)
	  (when (and (not (minibufferp))
				 (or (not semantic-highlight-func-ct-overlay)
					 (eq (semantic-overlay-buffer
						  semantic-highlight-func-ct-overlay)
						 (current-buffer))))
		(let* ((tag (semantic-stickyfunc-tag-to-stick))
			   (ol semantic-highlight-func-ct-overlay)
			   (disable (car args)))
		  (when (not ol)
			;; No overlay in this buffer.  Make one.
			(setq ol (semantic-make-overlay (point-min) (point-min)
											(current-buffer) t nil))
			(semantic-overlay-put ol 'highlight-func t)
			(semantic-overlay-put ol 'face 'semantic-highlight-func-current-tag-face)
			(semantic-overlay-put ol 'keymap semantic-highlight-func-mode-map)
			(semantic-overlay-put ol 'help-echo
								  "Current Function : mouse-3 - Context menu")
			(setq semantic-highlight-func-ct-overlay ol))

		  ;; TAG is nil if there was nothing of the appropriate type there.
		  (if (or (not tag) disable)
			  ;; No tag, make the overlay go away.
			  (progn
				(semantic-overlay-put ol 'tag nil)
				(semantic-overlay-move ol (point-min) (point-min) (current-buffer)))

			;; We have a tag, if it is the same, do nothing.
			(unless (eq (semantic-overlay-get ol 'tag) tag)
			  (save-excursion
				(goto-char (semantic-tag-start tag))
				(search-forward (semantic-tag-name tag) nil t)
				(semantic-overlay-put ol 'tag tag)
				(let ((beg (point-at-bol))
					  (end (progn
							 (search-forward ":" nil t)
							 (- (point) 1))))
				  (semantic-overlay-move ol beg end)))))))
	(apply orig args))
  nil)

(provide 'cython-semantic-features)
