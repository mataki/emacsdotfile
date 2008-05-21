;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; 日本語設定設定
;; http://d.hatena.ne.jp/SISY/searchdiary?word=*[emacs]
(set-language-environment "Japanese")
(auto-compression-mode t)
;; http://www.yza.jp/blog/item/422/
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-systems 'utf-8)
;; 基本設定
(set-scroll-bar-mode 'right)
(display-time)
(setq visible-bell t)
;; http://www.bookshelf.jp/soft/meadow_42.html#SEC632
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; アンチエイリアス設定
;;(set-face-font 'default "-sazanami-gothic-medium-r-normal--0-0-0-0-c-0-jisx0212.1990-0")

(cond (window-system
       (set-default-font
        "-*-fixed-medium-r-normal--12-*-*-*-*-*-*-*")
       (progn
         (set-face-font 'default
                        "-shinonome-gothic-medium-r-normal--12-*-*-*-*-*-*-*")
         (set-face-font 'bold
                        "-shinonome-gothic-bold-r-normal--12-*-*-*-*-*-*-*")
         (set-face-font 'italic
                        "-shinonome-gothic-medium-i-normal--12-*-*-*-*-*-*-*")
         (set-face-font 'bold-italic
                        "-shinonome-gothic-bold-i-normal--12-*-*-*-*-*-*-*")
       )))

;; C-hでbackspace
;(keyboard-translate ?\C-h ?\C-?)
;(global-set-key "\C-h" nil)
(global-set-key "\C-h" 'delete-backward-char)

;; anything
(require 'anything-config)
;; keybind
(global-set-key (kbd "C-;") 'anything)
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
;; source list
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-locate
                             anything-c-source-complex-command-history
                             anything-c-source-emacs-commands))

;; http://www.bookshelf.jp/soft/meadow_34.html#SEC497
(load "dabbrev-ja")
;; http://namazu.org/~tsuchiya/elisp/#dabbrev-highlight
(require 'dabbrev-highlight)

;; pabbrev-mode http://www.bookshelf.jp/soft/meadow_34.html#SEC507
(require 'pabbrev)
(global-pabbrev-mode)

;; DabbrevExpandMultiple
;; http://d.hatena.ne.jp/khiker/20070817/emacs_dabbrev
(require 'dabbrev-expand-multiple)
(global-set-key "\M-/" 'dabbrev-expand-multiple)

;; emacs-rails
;; http://rubyforge.org/projects/emacs-rails/
;; http://d.hatena.ne.jp/higepon/20061222/1166774270
(setq load-path (cons (expand-file-name "~/.emacs.d/emacs-rails") load-path))
(require 'rails)

;; git-emacs
;; http://d.hatena.ne.jp/xcezx/20080425/1209081657
;; http://tsgates.cafe24.com/git/git-emacs.html
(setq load-path (cons (expand-file-name "~/.emacs.d/git-emacs") load-path))
(require 'git-emacs)

;; moccur
(require 'color-moccur)
(eval-after-load "color-moccur"
  '(require 'moccur-edit))

;; wdiredhttp://www.bookshelf.jp/soft/meadow_25.html#SEC296
;; diredでファイル名を一括リネーム
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; find-fileでの補完
;; http://www.bookshelf.jp/soft/meadow_23.html#SEC219
(setq hc-ctrl-x-c-is-completion t)
(require 'highlight-completion)
(highlight-completion-mode 1)
(global-set-key "\C-\\" 'toggle-input-method)

;; browse-kill-ring
;; http://www.todesschaf.org/projects/bkr.html
;; http://www.bookshelf.jp/soft/meadow_32.html#SEC451
(require 'browse-kill-ring)
(global-set-key "\M-y" 'browse-kill-ring)
;; 必要に応じて browse-kill-ring のウィンドウの大きさを変更する
(setq browse-kill-ring-resize-window t)
;; 現在選択中の kill-ring のハイライトする
(setq browse-kill-ring-highlight-current-entry t)

;; 矩形
;; http://taiyaki.org/elisp/sense-region/
(autoload 'sense-region-on "sense-region"
          "System to toggle region and rectangle." t nil)
(sense-region-on)

;; ibuffer
;; http://www.bookshelf.jp/soft/meadow_28.html#SEC357
(require 'ibuffer)

;; mmm-mode
(setq load-path (cons (expand-file-name "~/.emacs.d/mmm-mode") load-path))
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
;; 色設定．これは，好みで．色をつけたくないなら nil にします．
(set-face-background 'mmm-default-submode-face "honeydew")

;; (mmm-add-classes
;;  '((mmm-html-javascript-mode
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "<script[^>]*>/\/i/\/i([^<]*/\/i/\/i)?</script>"
;;     :back "</script>"
;;     )
;;    (mmm-html-css-mode
;;     :submode css-mode
;;     :face mmm-code-submode-face
;;     :front "<style[^>]*>/\/i/\/i([^<]*/\/i/\/i)?/\/in[ /\/it]*</style>"
;;     )
;;    (mmm-ml-css-mode
;;     :submode css-mode
;;     :face mmm-code-submode-face
;;     :front "<style[^>]*>"
;;     :back "/\/in?[ /\/it]*</style>"
;;     )
;;    (mmm-ml-javascript-mode
;;     :submode javascript-mode
;;     :face mmm-code-submode-face
;;     :front "<script[^>]*>[^<]"
;;     :front-offset -1
;;     :back "/\/in?[ /\/it]*</script>"
;;     )
;;    (mmm-mxml-actionscript-mode
;;     :submode actionscript-mode
;;     :face mmm-code-submode-face
;;     :front "<mx:script><!/\/i/\/i[CDATA/\/i/\/i["
;;     :back "[ /\/it]*/\/i/\/i]/\/i/\/i]></mx:script>"
;;     )
;;    ))

;; javascript-mode js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; css-mode
;; http://www.garshol.priv.no/download/software/css-mode/doco.html
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; -----------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-basic-offset 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
