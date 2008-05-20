;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; 日本語設定設定
(set-language-environment "Japanese")
(auto-compression-mode t)

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
                             anything-c-source-locate))

;; http://www.bookshelf.jp/soft/meadow_34.html#SEC497
(load "dabbrev-ja")
;; http://namazu.org/~tsuchiya/elisp/#dabbrev-highlight
(require 'dabbrev-highlight)

;; pabbrev-mode http://www.bookshelf.jp/soft/meadow_34.html#SEC507
(require 'pabbrev)
(global-pabbrev-mode)
