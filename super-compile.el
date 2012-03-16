; super-compile is a much smarter compile command.  It tries to be
; smarter about which command to run and where to run it.  Currently
; it focuses on Makefiles and CMake scripts, trying to automatically
; find the right script for the project and run it.

; Command that will be run
(defconst default-super-compile-command "make -k -j2")
; Build script file to search for. If you build in a different
; directory than you have your CMakeLists.txt in then CMakeLists.txt
; isn't a good choice for this. The Makefile should be reliable since
; you probably want to run make from within that directory, but you
; don't want to accidentally hit some other makefiles. Instead look
; for a CMake-related file that shouldn't show up anywhere else.
(defconst default-super-compile-script "cmake_install.cmake")
; List of path offsets to search for build scripts in
(defconst default-super-compile-paths '("" "build/" "cmake/" "build/cmake/" "build/out/"))

(defvar super-compile-command default-super-compile-command)
(defvar super-compile-script default-super-compile-script)
(defvar super-compile-paths default-super-compile-paths)

(defun super-compile-find-upmost-file (filename path)
  (let ((dirname (expand-file-name path))
        (foundfirstp nil)
        (foundlastp nil)
        (lastfound nil))
    ;; You do not store your files in /, do you?
    (while (not (or foundlastp (string= dirname "/")))
      (if (file-exists-p (expand-file-name filename dirname))
          (progn
            (setq foundfirstp t)
            (setq lastfound dirname))
        (if foundfirstp
            (setq foundlastp t)))
      (setq dirname (expand-file-name ".." dirname)))
    (if lastfound lastfound nil)) )

(defun super-compile-cd-and-compile (command path)
  (interactive)

  (let ((buffer (find-file-noselect path)))
    (set-buffer buffer)
    (compile command)
    (kill-buffer buffer)) )

(defun super-compile-find-cd-and-compile (command filename paths)
  (interactive)

  (let ((buffer-directory (file-name-directory (buffer-file-name))))

    (let ((found nil) (pathslist paths) (path nil))
      (while (and pathslist (not found))
        (setq path (car pathslist))
        (setq pathslist (cdr pathslist))
        (let ((build-directory (super-compile-find-upmost-file (concat path filename) buffer-directory)))
          (if build-directory
              (progn
                (super-compile-cd-and-compile command (concat build-directory "/" path))
                (setq found t)
               )
            )
          )
        )
      )
    )
  )

(defun super-compile (prefix)
  (interactive "P")

  (super-compile-find-cd-and-compile super-compile-command super-compile-script super-compile-paths)
  (select-window (get-buffer-window "*compilation*"))
  )

(provide 'super-compile)