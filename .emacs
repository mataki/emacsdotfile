;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; 日本語設定設定
;; http://d.hatena.ne.jp/SISY/searchdiary?word=*[emacs]
(set-language-environment "Japanese")
(auto-compression-mode t)
;;http://www.yza.jp/blog/item/422/
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