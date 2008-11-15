;;; widen-window.el --- Widening selecting window

;; Copyright (C) 2008  Yuto Hayamizu

;; Author: Yuto Hayamizu <y.hayamizu@gmail.com>
;; Keywords: convenience
;; Version: 0.0.4

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This minor mode, widen window mode, provides a function that widen
;; selected window automatically.
;; It was tested only on Emacs 22.

;; In order to use this minor mode, put this file into
;; a directory included in load-path,
;; and add following code to your .emacs.
;; +------------------------+
;; (require 'widen-window)
;; (global-widen-window-mode t)
;; +------------------------+

;; You can change the window size ratio by customizing `ww-ratio'.
;; `ww-ratio' must be greater than 0.0 and less than 1.0 .

;; If you want to avoid widen window mode in a certain
;; major mode(say `foo-mode'), customize the variable `ww-nonwide-modes'.

;; Because this is still early release, sometimes window widening
;; might not work even if it should. If you find some functions to be
;; advised, add them to `ww-advised-functions'.

;;; Code:

(require 'easy-mmode)
(require 'cl)

(defgroup widen-window nil
  "Widen selected window"
  :group 'convenience
  :prefix "widen-window-")

(defcustom ww-ratio 0.625
  "This is a ratio which the selected window takes up in window subtree."
  :group 'widen-window
  :type 'number
  )

(defcustom ww-nonwide-modes
  '(dummy1-mode dummy2-mode)
  "Major modes `widen-current-window' cannot run."
  :type '(list symbol)
  :group 'widen-window)

(defcustom ww-height
  t
  "If `ww-height' is non-nil, widen-window for height will work."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'widen-window)

(defcustom ww-width
  t
  "If `ww-width' is non-nil, widen-window for width will work."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'widen-window)

(defcustom ww-advised-functions
  '(other-window
    split-window
    switch-to-buffer
    mouse-drag-region)
  "Functions to be advised. Window widening function `widen-current-window' is fired after advised function was called."
  :type '(list symbol)
  :group 'widen-window)

(defun widen-current-window ()
  (interactive)

  (unless (minibufferp (current-buffer))
    (cond
     ((>= 0 ww-ratio) (setq ww-ratio 0.2))
     ((<= 1 ww-ratio) (setq ww-ratio 0.8)))
  
    (let* ((current-window (selected-window))
	   (window-tree (bw-get-tree (selected-frame))))
      (when window-tree
	(ww-subtree
	 window-tree current-window
	 (- (bw-r window-tree) (bw-l window-tree))
	 (- (bw-b window-tree) (bw-t window-tree)))
	))))

(defun ww-subtree (wtree cur-win wid hei)
  (setq wtree (bw-refresh-edges wtree))
  (unless wid (setq wid (- (bw-r wtree) (bw-l wtree))))
  (unless hei (setq hei (- (bw-b wtree) (bw-t wtree))))
  (if (windowp wtree)
      (progn
	(when wid
	  (let ((dw (- wid (- (bw-r wtree) (bw-l wtree)))))
	    (when (/= 0 dw)
	      (bw-adjust-window wtree dw t))))
	(when hei
	  (let ((dh (- hei (- (bw-b wtree) (bw-t wtree)))))
	    (when (/= 0 dh)
	      (bw-adjust-window wtree dh nil))))
	)
    (let* ((children (cdr (assq 'childs wtree)))
	   (win-wid (- (bw-r wtree) (bw-l wtree)))
	   (win-hei (- (bw-b wtree) (bw-t wtree)))
	   (cwin-num (length children))
	   (cwin-bigger-wid wid)
	   (cwin-bigger-hei hei)
	   (cwin-smaller-wid wid)
	   (cwin-smaller-hei hei))
      (case (bw-dir wtree)
	((hor)
	 (when ww-width
	   (setq cwin-smaller-wid
		 (floor (/ (* win-wid (- 1 ww-ratio))
			   (- cwin-num 1))))
	   (setq cwin-bigger-wid
		 (- win-wid (* (- cwin-num 1) cwin-smaller-wid)))))
	((ver)
	 (when ww-height
	   (setq cwin-smaller-hei
		 (floor (/ (* win-hei (- 1 ww-ratio))
			   (- cwin-num 1))))
	   (setq cwin-bigger-hei
		 (- win-hei (* (- cwin-num 1) cwin-smaller-hei))))))
      (dolist (cwin children)
	(if (ww-find-window-in-subtree cwin cur-win)
	    (ww-subtree cwin cur-win
			cwin-bigger-wid
			cwin-bigger-hei)
	  (bw-balance-sub cwin cwin-smaller-wid cwin-smaller-hei)))
      )))

(defun ww-find-window-in-subtree (wt window)
  (block func
    (cond
     ((windowp wt)
      (if (equal wt window)
	  window
	nil))
     (t
      (dolist (subwt (cdr (assq 'childs wt)))
	(let ((ret (ww-find-window-in-subtree subwt window)))
	  (when ret
	    (return-from func window))))
      nil))))

(defun ww-setup-advice ()
  (dolist (func ww-advised-functions)
    (eval `(defadvice ,func (after widen-window-advice)
	     (if (and widen-window-mode (not (memq major-mode ww-nonwide-modes)))
		 (widen-current-window)))))
  (ad-activate-regexp "widen-window"))

(define-minor-mode widen-window-mode
  "Widen Window mode"
  :lighter " WW"
  :group 'widen-window
  (if widen-window-mode
      (progn
	(ww-setup-advice)
	(if (memq major-mode ww-nonwide-modes)
	    (widen-window-mode nil)))
    nil))

(defun widen-window-mode-maybe ()
  "Return t if `widen-current-window' can run on current buffer."
  (if (and (not (minibufferp (current-buffer)))
	   (not (memq major-mode ww-nonwide-modes)))
      (widen-window-mode t)))

(define-global-minor-mode global-widen-window-mode
  widen-window-mode widen-window-mode-maybe
  :group 'widen-window)

;;; for anything.el
;; (defadvice anything (around disable-ww-mode activate)
;;   (ad-deactivate-regexp "widen-window")
;;   (unwind-protect
;;       ad-do-it
;;     (ad-activate-regexp "widen-window")))

;; (if (fboundp 'anything)
;;     (ad-activate-regexp "disable-ww-mode"))

(provide 'widen-window)
;;; widen-window.el ends here
