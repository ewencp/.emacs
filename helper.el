;#####################################################################
;Copyright 2007.
;#####################################################################
;helper.el
;#####################################################################

(provide' helper)

;#####################################################################
;My C++Style
;#####################################################################

(defconst jerry-c-style
  '((c-basic-offset . 4)
        (c-offsets-alist . ((string . -1000)
                        (c . c-lineup-C-comments)
                        (defun-open . 0)
                        (defun-close . 0)
                        (defun-block-intro . +)
                        (class-open . 0)
                        (class-close . 0)
                        (inline-open . 0)
                        (inline-close . 0)
                        (func-decl-cont . +)
                        (knr-argdecl-intro . 5)
                        (knr-argdecl . 0)
                        (topmost-intro . 0)
                        (topmost-intro-cont . 0)
                        (member-init-intro . 1)
                        (member-init-cont . 0)
                        (inher-intro . +)
                        (inher-cont . c-lineup-multi-inher)
                        (block-open . 0)
                        (block-close . 0)
                        (brace-list-open . 0)
                        (brace-list-close . 0)
                        (brace-list-intro . +)
                        (brace-list-entry . 0)
                        (statement . 0)
                        (statement-cont . +)
                        (statement-block-intro . +)
                        (statement-case-intro . *)
                        (statement-case-open . +)
                        (substatement . +)
                        (substatement-open . 0)
                        (case-label . *)
                        (access-label . -)
                        (label . *)
                        (do-while-closure . 0)
                        (else-clause . 0)
                        (comment-intro . c-lineup-comment)
                        (arglist-intro . +)
                        (arglist-cont . 0)
                        (arglist-cont-nonempty . +)
                        (arglist-close . 0)
                        (stream-op . c-lineup-streamop)
                        (inclass . +)
                        (cpp-macro . -1000)
                        (friend . 0)
                        (extern-lang-open . 0)
                        (extern-lang-close . 0)
                        (inextern-lang . +)
			(namespace-open . 0)
			(namespace-close . 0)
			(innamespace . 0)
                        (template-args-cont . +)))
    (c-hanging-braces-alist . ((class-open before after)
                               (class-close before)
                               (defun-open before after)
                               (defun-close before after)
                               (inline-open before)
                               (inline-close after)
                               (brace-list-open)
                               (brace-list-close)
                               (brace-list-intro)
                               (brace-list-entry)
                               (block-open after)
                               (block-close)
                               (substatement-open after)
                               (substatement-case-open)
			       (namespace-open after)
			       (namespace-close before after)
                               (extern-lang-open after)
                               (extern-lang-close after)))
    (c-hanging-colons-alist . ((case-label)
                               (label)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-cleanup-list         . ((defun-close-semi)
                               (scope-operator)))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks))
    (c-echo-syntactic-information-p . t))
  "Jerry's C++Style")

(c-add-style "jerry" jerry-c-style)

;#####################################################################
;Syntax Highlighting
;#####################################################################

(setq font-lock-keyword-case-fold-search nil);Need to be case sensitive
(font-lock-add-keywords 'c++-mode  '(("[^[:lower:]]\\([[:upper:]][[:upper:][:digit:]_]*\\)[<>,]" 1 font-lock-type-face t)));match class names

(make-face 'font-lock-operators-face)
(font-lock-add-keywords 'c++-mode '(("[;<>:=!]\\|->" . 'font-lock-operators-face)))

(make-face 'font-lock-preprocessor-face)
(font-lock-add-keywords 'c++-mode '(("\\#[a-zA-Z0-9]*[ ]" . 'font-lock-preprocessor-face)))

;#####################################################################
;Navigation Commands
;#####################################################################

(defun header-flip ()
  "Find the .h file for this .C file (or vice versa)."
  (interactive)
  (let ((dotc (string-match "[.]\\(cpp\\|c\\)$" (buffer-file-name)))
        (doth (string-match "[.]\\(h\\|hpp\\)$" (buffer-file-name))))
    (if dotc
        (if (file-exists-p (concat (substring (buffer-file-name) 0 dotc) ".h"))
            (find-file (concat (substring (buffer-file-name) 0 dotc) ".h"))
          (find-file (concat (substring (buffer-file-name) 0 dotc) ".hpp")))
      (if doth
          (find-file (concat (substring (buffer-file-name) 0 doth) ".cpp"))
        (message "Not a cpp or h file!!")))))

(defun open-parent ()
  "Open header of parent class"
  (interactive)
  (let ((doth (string-match "[.]h$" (buffer-file-name))))
    (if doth
        (let ((save_point (point)))
          (goto-char 0)
          (if (re-search-forward ":\\(?:public\\|protected\\|private\\) \\(\\(?:\\w\\|_\\)*\\)" nil t)
              (let ((filename (concat (match-string 1) ".h")))
                (goto-char save_point)
                (if (file-exists-p filename)
                    (find-file filename)
                  (let ((meru_filename (find-meru-file filename)))
                    (if (file-exists-p meru_filename)
                        (find-file meru_filename)
                      (message "Can't find header of parent")))))
            (goto-char save_point)
            (message "No parent class")))
      (message "Not a header file"))))

(defun shift-indent-left ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end)-4))

(defun shift-indent-right ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) 4))

;#####################################################################
;Formatting Helpers
;#####################################################################
(defun insert-function-comment ()
  "Add function comment"
  (interactive)
  (beginning-of-line)
  (insert "//#####################################################################\n")
  (insert "//\n")
  (insert "//#####################################################################\n")
  (beginning-of-line) (forward-line+1))

(defun current-year ()
  "Get year"
  (string-to-number (substring (current-time-string)-4)))

(defun get-current-line()
  "Current line string"
  (buffer-substring (save-excursion (beginning-of-line) (point))
                    (save-excursion (end-of-line) (point))))

(defun insert-copyright ()
  (interactive)
  (goto-char 0) (goto-line 7)
  (insert "//#####################################################################\n")
  (insert (concat "//Copyright " (number-to-string (current-year))  "," user-full-name ".\n"))
  (insert "//#####################################################################\n"))

(defun insert-header ()
  (interactive)
  (insert-copyright)
  (let ((classname (substring (buffer-name) 0-2)))
    (insert (format "#ifndef __%s__\n" classname))
    (insert (format "#define __%s__\n\n" classname))
    (insert (format "class %s\n" classname))
    (insert "{\n")
    (insert "public:\n\n")
    (insert "};\n")
    (insert "//#####################################################################\n")
    (insert "#endif\n")))

(defun make-cpp-from-header ()
  (interactive)
  (goto-char 0)
  (goto-line 7)
  (copy-region-as-kill (point-min) (point))
  (header-flip)
  (yank)
  (let ((classname (substring (buffer-name) 0-4)))
    (insert (format "#include \"%s.h\"\n" classname))))

;#####################################################################
;Compilation
;#####################################################################
;(setq compile-command (format (concat "make-j3-I " (getenv "CURR_PROJECT"))))
(setq compilation-read-command nil)
(global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f1>") 'stylize-cpp-file)
