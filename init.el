;#####################################################################
; .emacs
;
; Emacs configuration file
; Original by Jerry Talton (jtalton@cs.stanford.edu)
; Modified by Ewen Cheslack-Postava (ewencp@cs.stanford.edu)
;
; Sets up a nice configuration for coding and writing -- C, C++, Ruby,
; Python, LaTeX, and so on.  Note that personal information is stored
; in personality.el to isolate modifications necessary to personalize
; a copy of this setup.
;
;#####################################################################

; Don't wait for the window manager if it takes a long time
(modify-frame-parameters nil '((wait-for-wm . nil)))

; Disable the splash screen, which hides the document opened and is
; useless anyway
(setq inhibit-splash-screen t)

; Use .emacs.d for .el files
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/jade-mode")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

; Personality stuff
(require 'personality)

(require 'cursors)
(require 'cc-mode)
(require 'mouse-drag)
(require 'helper)

(require 'sws-mode)
(require 'jade-mode)

(require 'less-mode)

; There are some issues finding programs in the PATH on mac, this resolves them.
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (memq window-system '(mac ns))
  (set-exec-path-from-shell-PATH))


; Go mode, including gofmt-on-save
(require 'go-mode)
(add-hook 'go-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq-default)
    (setq tab-width 2)
    (setq standard-indent 2)
    (setq indent-tabs-mode nil)))

(require 'magit)

; Mac wants home/end to be start/end of document rather than line
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; Nukes trailing whitespace and deletes excess newlines
(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(setq whitespace-check-mode t)
(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(autoload 'glsl-mode "glsl-mode" nil t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake build scripts." t)
(autoload 'js2-mode "js2" nil t)
(autoload 'espresso-mode "espresso-mode" nil t)
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")

; Set default mode for new buffers to text
(setq default-major-mode 'text-mode)

; Put temp files somewhere they won't bother me, and delete old ones
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq delete-old-versions t)
(setq delete-auto-save-files t)

; Make executabe after saving if #! is in the first line
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

; Always show me column numbers
(setq column-number-mode t)

; Make searches case-INsensitive
(set-default 'case-fold-search t)

; Stop forcing me to type "yes"
(fset 'yes-or-no-p 'y-or-n-p)

; Highlight matching braces
(load-library "paren")
(show-paren-mode t)

; Use the Command key for meta on Macs
(defvar macosx-p (string-match "darwin" (symbol-name system-type))
(if macosx-p (progn (setq mac-command-key-is-meta nil))))

(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(global-set-key [(ctrl b)] 'cycle-buffer-backward)
(global-set-key [(ctrl f)] 'cycle-buffer)
(global-set-key [down-mouse-2] 'mouse-drag-drag)

; Global color and fonts
(setq default-frame-alist
      '(
        (font             .  "Droid Sans Mono-10")
        (width            .     156 )
        (height           .     50 )
        (mouse-color      . "White")
        (cursor-color     . "SlateBlue3")
        (foreground-color . "gray95")
        (background-color . "#212121")))

; Turn on highlighting with CTRL-X w h and CTRL-X w r
(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

; Make the modeline a little more inconspicuous...
;(set-face-background 'modeline "#202020")
;(set-face-foreground 'modeline "#C0C0C0")

; Make emacs stop bugging me about symlinks
(setq vc-follow-symlinks t)

; Make all tabs spaces by default
(setq-default indent-tabs-mode nil)

; Set up the mode-specific font locking
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

; Set the modes for various types of files
(setq auto-mode-alist
      (append
       (list
        '("\\.C$"         . c++-mode)
        '("\\.h$"         . c++-mode)
        '("\\.c\\+\\+$"   . c++-mode)
        '("\\.H$"         . c++-mode)
        '("\\.cu$"        . c++-mode)
        '("\\.rb$"        . ruby-mode)
        '("\\.el$"        . emacs-lisp-mode)
        '("emacs$"        . emacs-lisp-mode)
        '("\\.tar$"       . tar-mode)
        '("make\\.log\\~" . compilation-mode)
        '("Makefile$"     . makefile-mode)
        '("Makefile.*"    . makefile-mode)
        '("\\.vert\\'"    . glsl-mode)
        '("\\.frag\\'"    . glsl-mode)
        '("\\.py\\'"      . python-mode)
        '("SConscript"    . python-mode)
        '("\\.ml[iylp]?$" . caml-mode)
        '("\\.php$"       . php-mode)
        '("\\.cs$"        . csharp-mode)
        '("CMakeLists\\.txt\\'" . cmake-mode)
        '("\\.cmake\\'"   . cmake-mode)
        '("\\.doc\\'"     . text-mode)
        '("\\.js\\'"      . js2-mode)
        '("\\.json\\'"      . js2-mode)
        '("\\.em\\'"      . js2-mode)
        '("\\.hs\\'"      . haskell-mode)
        '("\\.styl\\'"      . sws-mode)
        '("\\.jade\\'"      . jade-mode)
        '("\\.pp\\'"      . puppet-mode)
        '("\\.less$"      . less-css-mode)
        '("\\.go$"      . go-mode)
        )
       auto-mode-alist))

(make-face            'nick-url-face)
(set-face-foreground  'nick-url-face "Blue")
(set-face-underline-p 'nick-url-face t)

; Remove those pesky scrollbars - they just take up space
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode nil)
  )

; Remove stupid toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
  )

; In case of extra buttons
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

; Quick access to files and tags
(setq speedbar-use-imenu-flag 't)

; Hooks, hooks, and more hooks
(defconst text-mode-hook
  '(lambda ()
     (defconst fill-column 80)
     (defconst tab-stop-list
       (list 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
     (auto-fill-mode 1)
     (transient-mark-mode 1)))

(defconst shell-mode-hook
  '(lambda ()
     (defconst comint-scroll-show-maximum-output 't)
     (defconst comint-scroll-to-bottom-on-input 't)
     (defconst comint-scroll-show-maximum-output 't)
     (defconst comint-output-filter-functions
       '(comint-postoutput-scroll-to-bottom comint-strip-ctrl-m))))

(defconst ediff-startup-hook
  '(lambda ()
     (ediff-toggle-split)))

(defconst makefile-mode-hook
  '(lambda ()
     (run-hooks 'text-mode-hook)
     (set-face-background 'makefile-tab-face "grey70")))

(defconst latex-mode-hook
  '(lambda ()
     (run-hooks 'text-mode-hook)
     (defconst fill-column 70) ; Every other time
     (auto-fill-mode 1)))

; tex stuff
(setq tex-dvi-view-command "xdvi")

(global-font-lock-mode t)
(setq-default font-lock-maximum-decoration t)
(setq scroll-preserve-screen-position nil)

;#####################################################################
; Style stuff
;####################################################################
;(setq font-lock-support-mode 'lazy-lock-mode)
(set-face-foreground  'font-lock-string-face "#93A07C")
(set-face-foreground  'font-lock-comment-face "#94AFE6")
(set-face-foreground  'font-lock-warning-face "#FC0B0C")
(set-face-foreground  'font-lock-function-name-face "purple1")
(set-face-foreground  'font-lock-keyword-face "pale green")
(set-face-foreground  'font-lock-constant-face "#F09E9F")
(set-face-foreground  'font-lock-type-face "#FFDC78")
(set-face-foreground  'font-lock-variable-name-face "gray95")
(set-face-foreground  'font-lock-operators-face "#F7B683")
(set-face-foreground  'font-lock-preprocessor-face "#FC0B0C")
(set-face-background  'show-paren-match-face "SlateBlue3")
(set-face-background  'region "#502020")

; Setup C++ style
(defconst c-mode-hook
  '(lambda ()
     (c-set-style "jerry")
     (run-hooks 'text-mode-hook)
     (auto-fill-mode t)
     (defconst fill-column 80)
     (setq truncate-lines nil)
     (line-number-mode 1)
     (c-toggle-hungry-state 1)
     (imenu-add-menubar-index)
     (hs-minor-mode)))

(defconst c++-mode-hook
  '(lambda ()
     (run-hooks 'c-mode-hook)))

(defconst csharp-mode-hook
  '(lambda ()
     (run-hooks 'c-mode-hook)))

(defconst asm-mode-set-comment-hook
  '(lambda ()
     (setq asm-comment-char ?\#)))

; Javascript stuff

; sourced from mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(defun e-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun e-js2-mode-hook ()
  (require 'espresso)
  (auto-fill-mode t)
  (defconst fill-column 80)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'e-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "JS2 hook"))
(add-hook 'js2-mode-hook 'e-js2-mode-hook)

; Compilation stuff
(require 'super-compile)
(setq compilation-scroll-output 'first-error)
(global-set-key (kbd "<f8>") 'super-compile)
(global-set-key (kbd "C-<f8>") 'kill-compilation)
(global-set-key (kbd "C-e") 'next-error)
; Skip warnings in compile output
(setq compilation-skip-threshold 2)


;#####################################################################
; extra binds
;#####################################################################
(global-set-key (kbd "M-o") 'header-flip)
(global-set-key (kbd "C-<") 'shift-indent-left)
(global-set-key (kbd "C->") 'shift-indent-right)
(global-set-key (kbd "M-[ d") 'backward-word)
(global-set-key (kbd "M-[ c") 'forward-word)
(global-set-key (kbd "M-<f12>") 'eval-current-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "S-<home>") 'beginning-of-buffer)
(global-set-key (kbd "S-<end>") 'end-of-buffer)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(load-home-init-file t t)
 '(show-paren-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "gray95" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono")))))

; Magit
(global-set-key (kbd "C-g") 'magit-status)

; Code folding
(global-set-key (kbd "C-f") 'hs-toggle-hiding)

; Copy-and-paste logging
;(require 'keylogger)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))
