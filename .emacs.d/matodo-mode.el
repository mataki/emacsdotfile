(easy-mmode-define-minor-mode matodo-mode
 "Mat Todo Mode."
 nil
 " Matodo"
 '(("\C-cjj" . toggle-shikaku-func)
   ("\C-cjs" . insert-shiro-shikaku)
   ("\C-cjk" . insert-kuro-shikaku)
   ))

(defun insert-shiro-shikaku ()
  (interactive)
  (insert "□ ")
  )

(defun insert-kuro-shikaku ()
  (interactive)
  (insert "■ ")
  )

(defun toggle-shikaku-func ()
  (interactive) (swap-char-at-point "□" "■")
)

(defun delete-insert-char(c)
  (interactive)
  (progn (delete-char 1 nil) (insert c) (backward-char 1)))

(defun swap-char-at-point(a b)
  (let ((c (char-to-string (char-after))))
    (cond
     ((string-equal c a) (delete-insert-char b))
     ((string-equal c b) (delete-insert-char a)))))

(provide 'matodo-mode)
;; ;-----------------------------------------------------------------------------
;; ; 初期化
;; ;-----------------------------------------------------------------------------
;; (defvar matodo-mode nil)

;; (if (not (assq 'matodo-mode minor-mode-alist))
;;     (setq minor-mode-alist
;;           (cons '(matodo-mode " matodo")
;;                 minor-mode-alist)))

;; (defun matodo-mode (&optional arg)
;;   "mat todo minor-mode"
;;   (interactive)

;;   (cond
;;    ((< (prefix-numeric-value arg) 0)
;;     (setq matodo-mode nil))
;;    (arg
;;     (setq matodo-mode t))
;;    (t
;;     (setq matodo-mode (not matodo-mode))))
;;   (if matodo-mode
;;       nil)
;;   )

;; ;; -----------------------------------------------------------------------------
;; ;; キーバインド
;; ;; -----------------------------------------------------------------------------
;; (add-hook 'matodo-mode-hook
;;           '(lambda ()
;;              (define-key matodo-mode-map "\C-cj" 'toggle-shikaku-func)))

;; ;; -----------------------------------------------------------------------------
;; ;; メソッド
;; ;; -----------------------------------------------------------------------------

;; (defun toggle-shikaku-func ()
;;   (interactive) (swap-char-at-point "□" "■")
;; )

;; (defun delete-insert-char(c)
;;   (interactive)
;;   (progn (delete-char 1 nil) (insert c) (backward-char 1)))

;; (defun swap-char-at-point(a b)
;;   (let ((c (char-to-string (char-after))))
;;     (cond
;;      ((string-equal c a) (delete-insert-char b))
;;      ((string-equal c b) (delete-insert-char a)))))



