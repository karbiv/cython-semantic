;;; package --- Summary:

;;; Commentary:

;;; Code:

(require 'semantic/wisent)


(defun cython-decorated (decorators tag)
  "Augment decorated item tag with decorator attribute.
DECORATORS - list of decorator names,
TAG - decorated item(class or function)."
  (when decorators
    (semantic-tag-put-attribute tag :decorators decorators)
    ;; If there is a staticmethod decorator, add a static typemodifier
    ;; for the function.
    (when (member "staticmethod" decorators)
      (semantic-tag-put-attribute
       tag :typemodifiers
       (cons "static"
	     (semantic-tag-get-attribute tag :typemodifiers))))
    (when (member "classmethod" decorators)
      (semantic-tag-put-attribute
       tag :typemodifiers
       (cons "classmethod"
	     (semantic-tag-get-attribute tag :typemodifiers))))
    tag))


(defun cython-reconstitute-function-tag (tag suite)
  "Move a docstring from TAG's members into its :documentation attribute.
Set attributes for constructors, special, private and static methods.
TAG SUITE"
  ;; Analyze first statement to see whether it is a documentation
  ;; string.
  (let ((first-statement (car suite)))
    (when (semantic-python-docstring-p first-statement)
      (semantic-tag-put-attribute
       tag :documentation
       (semantic-python-extract-docstring first-statement))))

  ;; Suite(func body) will be used in `cython-reconstitute-class-tag' to
  ;; collect instance var assignments(like 'self.var') in methods,
  ;; that could be created in any method, not only in constructor
  (semantic-tag-put-attribute tag :suite suite)
  
  ;; Identify special and private functions
  (cond
   ((semantic-python-special-p tag)
    (semantic-tag-put-attribute tag :special-flag t))
   ((semantic-python-private-p tag)
    (semantic-tag-put-attribute tag :protection "private")))

  ;; TODO
  ;; + check for operator overloading
  tag)


(defun cython-reconstitute-class-tag (tag)
  "Move a docstring from TAG's members into its :documentation attribute."
  ;; The first member of TAG may be a documentation string. If that is
  ;; the case, remove it from the members list and stick its
  ;; content into the :documentation attribute.
  (let ((first-member (car (semantic-tag-type-members tag))))
    (when (cython-docstring-p first-member)
      (semantic-tag-put-attribute
       tag :members
       (cdr (semantic-tag-type-members tag)))
      (semantic-tag-put-attribute
       tag :documentation
       (semantic-python-extract-docstring first-member))))

  ;; Augment with inner tags
  (dolist (member-tag (semantic-tag-type-members tag))
    ;; Set tag's parent name(its container tag)
    (semantic-tag-put-attribute member-tag :parent (semantic-tag-name tag))
    ;; Set flag if it's a constructor
    (when (member (semantic-tag-name member-tag) '("__init__" "__cinit__"))
      (semantic-tag-put-attribute member-tag :constructor-flag t))

    ;; Find assignments to instance variables and add
    ;; corresponding variable tags to the list of members.
    (let ((self (semantic-tag-name
                 (car (semantic-tag-function-arguments member-tag)))))
      ;; Peek assignment statements in the function's code
      (dolist (statement (semantic-tag-get-attribute member-tag :suite))
        (when (cython-instance-variable-p statement self)
          (let ((parts (split-string (semantic-tag-name statement) "\\.")))
            ;; Only 1 dot: 'self.var ='
            (when (equal 2 (length parts))
              (let ((variable (semantic-tag-clone statement (cadr parts)))
                    (members (semantic-tag-get-attribute tag :members)))
                (when (semantic-python-private-p variable)
                  (semantic-tag-put-attribute variable :protection "private"))
                ;; Append to members of the class
                (setcdr (last members) (list variable)))))))))
  tag)


(defun cython-instance-variable-p (tag self)
  "Return non-nil if TAG is an instance variable of the instance SELF."
  (when (semantic-tag-of-class-p tag 'variable)
    (let ((name (semantic-tag-name tag)))
      (and
       (semantic-tag-get-attribute tag :assign)
       (string-match (rx-to-string `(seq string-start ,self ".")) name)))))


;; Original "python-docstring-p" didn't check for single quotes docstring
(defun cython-docstring-p (tag)
  "Return non-nil, when TAG is a Python documentation string."
  ;; TAG is considered to be a documentation string if the first
  ;; member is of class 'code and its name looks like a documentation
  ;; string.
  (let ((class (semantic-tag-class tag))
	(name  (semantic-tag-name  tag)))
    (and (eq class 'code)
	 (or
          (string-match
           (rx (seq string-start (or "\"\"\"" "'''") (0+ anything) (or "\"\"\"" "'''") string-end))
           name)))))

(provide 'wisent-cython)

;;; wisent-cython.el ends here
