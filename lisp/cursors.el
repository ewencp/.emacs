;;; cursors.el --- Cursor depends on insert/overwrite mode.
;; 
;; Emacs Lisp Archive Entry
;; Filename: cursors.el
;; Description: Cursor depends on insert/overwrite mode.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000, 2001, Drew Adams, all rights reserved.
;; Created: Mon Nov 27 09:05:17 2000
;; Version: $Id: cursors.el,v 1.3 2001/01/08 22:33:29 dadams Exp $
;; Last-Updated: Mon Jan  8 14:33:22 2001
;;           By: dadams
;;     Update #: 36
;; Keywords: overwrite, insert, cursor
;; Compatibility: GNU Emacs 20.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Cursor depends on insert/overwrite mode.
;;
;;
;; Sets cursor to a bar in regular mode, a block in overwrite mode.
;;
;; From Joe Casadonte address@bogus.example.com, who got it from Steve Kemp.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: cursors.el,v $
;; RCS Revision 1.3  2001/01/08 22:33:29  dadams
;; RCS Adapted file header for Emacs Lisp Archive.
;; RCS
;; RCS Revision 1.2  2001/01/03 00:34:32  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/12/07 19:36:10  dadams
;; RCS Initial revision
;; RCS
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(provide 'cursors)

;;;;;;;;;;;;;;;;;;;;;


;;; Change cursor to bar (the default is box).
;; (modify-frame-parameters nil '((cursor-type . bar)))

(defun joc-cursor-type-set-hook (&optional frame)
  "Set the cursor-type according to the insertion mode."
  (interactive)
  (setq frame (or frame (selected-frame)))
  (if overwrite-mode
      (modify-frame-parameters frame (list (cons 'cursor-type 'block)))
    (modify-frame-parameters frame (list (cons 'cursor-type 'bar)))))

(add-hook 'post-command-hook 'joc-cursor-type-set-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `cursors.el' ends here
