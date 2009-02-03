;; setenv PATH
(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))

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

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; バックアップしない
(setq make-backup-files nil)

;; C-x C-iでリージョンをインデント
(global-set-key "\C-x\C-i" 'indent-region)

(cond (window-system
       ;; アンチエイリアス設定
       (set-face-font 'default "-sazanami-gothic-medium-r-normal--0-0-0-0-c-0-jisx0212.1990-0")

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
(global-set-key "\C-x?" 'help)

;; grep
(require 'grep)
(require 'grep-edit)

(defun my-grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)


;; grep-find
;;(setq grep-find-command "find . -type f ! -path '*/.svn/*' -print0 | xargs grep -n ")
;;(setq grep-find-command "find . -type f ! -path '*/.svn/*' ! -path '*/tmp/*' ! -path '*/log/*' ! -name '*~' -print0 | xargs -0 grep -nH -e ")
(setq grep-find-command "find . -type f ! -path '*/.svn/*' ! -path '*/.git/*' ! -path '*/tmp/*' ! -path '*/coverage/*' ! -path '*/log/*' ! -name '#*#' ! -name '*~' -print0 | xargs -0 grep -nH -e ")
(global-set-key "\C-xgf" 'grep-find)

;; C-x p で前の画面
(define-key ctl-x-map "p"
  #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; color-theme
(setq load-path (cons (expand-file-name "~/.emacs.d/color-theme") load-path))
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;; moccur
(require 'color-moccur)
(eval-after-load "color-moccur"
  '(require 'moccur-edit))

;; http://fkmn.exblog.jp/7311776/
(setq dmoccur-exclusion-mask
      (append '("\\~$" "\\.svn\\/\*" "\\.git\\/\*") dmoccur-exclusion-mask))


;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;;; color-moccur.elの設定
;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

;; anything
(require 'anything-config)
(require 'anything)
(setq anything-idle-delay 0.3)
(setq anything-input-idle-delay 0)
(setq anything-candidate-number-limit 100)
(require 'anything-c-mx)

;; http://d.hatena.ne.jp/rubikitch/20080701/1214844444
;; (require 'anything-dabbrev-expand)
;; (setq anything-dabbrev-input-idle-delay 0.0)
;; (setq anything-dabbrev-idle-delay 1.0)

;; keybind
(global-set-key (kbd "C-;") 'anything)
(global-set-key (kbd "C-^") 'anything)
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

;; anything-c-source-kill-ring
(defvar anything-c-source-kill-ring
    '((name . "Kill Ring")
      (candidates . (lambda ()
                      (loop for kill in kill-ring
                            unless (string-match "^[\\s\\t]+$" kill)
                            collect kill)))
      (action . insert)
      (migemo)
      (multiline)))
;; source list

(setq anything-sources (list anything-c-source-buffers
;;                             anything-c-source-emacs-commands
;;                              anything-c-source-mx
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-locate
                             anything-c-source-complex-command-history
                             anything-c-source-kill-ring
                             ))


;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; WidenWindow http://d.hatena.ne.jp/rubikitch/20081113/1226575019
(require 'widen-window)
(setq ww-ratio 0.65)
(global-widen-window-mode 1)
;;(diminish 'widen-window-mode " WW")
(defadvice anything (around disable-ww-mode activate)
  (ad-deactivate-regexp "widen-window")
  (unwind-protect
      ad-do-it
    (ad-activate-regexp "widen-window")))

(require 'anything-complete)
(anything-lisp-complete-symbol-set-timer 150)

;; http://d.hatena.ne.jp/buzztaiki/20081115/1226760184
(require 'descbinds-anything)
(descbinds-anything-install)

;; http://www.bookshelf.jp/soft/meadow_34.html#SEC497
(load "dabbrev-ja")
;; http://namazu.org/~tsuchiya/elisp/#dabbrev-highlight
(require 'dabbrev-highlight)

;; pabbrev-mode http://www.bookshelf.jp/soft/meadow_34.html#SEC507
;;(require 'pabbrev)
;;(global-pabbrev-mode)

;; DabbrevExpandMultiple
;; http://d.hatena.ne.jp/khiker/20070817/emacs_dabbrev
;; (require 'dabbrev-expand-multiple)
;; (global-set-key "\M-/" 'dabbrev-expand-multiple)

;; emacs-rails
;; http://rubyforge.org/projects/emacs-rails/
;; http://d.hatena.ne.jp/higepon/20061222/1166774270
(setq load-path (cons (expand-file-name "~/.emacs.d/emacs-rails") load-path))
(require 'rails)

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
;; (require 'browse-kill-ring)
;; (global-set-key "\M-y" 'browse-kill-ring)
;; 必要に応じて browse-kill-ring のウィンドウの大きさを変更する
;; (setq browse-kill-ring-resize-window t)
;; 現在選択中の kill-ring のハイライトする
;; (setq browse-kill-ring-highlight-current-entry t)

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
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;; 色設定．これは，好みで．色をつけたくないなら nil にします．
(set-face-background 'mmm-default-submode-face "honeydew")
(load "mmm-mode-setting")

;; javascript-mode js2-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; javascript-mode
(add-to-list 'auto-mode-alist (cons  "\\.\\(js\\|as\\|json\\|jsn\\)\\'" 'javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(setq js-indent-level 4)

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
 '(js2-basic-offset 4)
 '(rails-ws:default-server-type "mongrel"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "red"))))
 '(mmm-code-submode-face ((t (:background "DarkGray"))))
 '(mmm-declaration-submode-face ((t (:background "Aquamarine" :foreground "black"))))
 '(mmm-default-submode-face ((t (:background "dark slate gray"))))
 '(mmm-output-submode-face ((t (:background "DarkGreen")))))

(put 'narrow-to-region 'disabled nil)

;; git-emacs
;; http://d.hatena.ne.jp/xcezx/20080425/1209081657
;; http://tsgates.cafe24.com/git/git-emacs.html
(setq load-path (cons (expand-file-name "~/.emacs.d/git-emacs") load-path))
(require 'vc-git)
(require 'imenu)

(require 'git-emacs)

;; ido-mode
(require 'ido)
(ido-mode t)

;; rcodetools
(require 'rcodetools)
;; (require 'anything-rcodetools)
;; ;; Command to get all RI entries.
;; (setq rct-get-all-methods-command "PAGER=cat fri -l")
;; ;; See docs
;; (define-key anything-map "\C-z" 'anything-execute-persistent-action)

;; howm
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

;; matodo
(require 'matodo-mode)

;; redo
(when (require 'redo nil t)
  (define-key ctl-x-map (if window-system "U" "r") 'redo)
  (define-key global-map [?\C-_] 'redo))

;; linum
(require 'linum)
(global-linum-mode)

;; http://openlab.dino.co.jp/2008/07/15/233005294.html
;; Show tab, zenkaku-space, white spaces at end of line
;; http://www.bookshelf.jp/soft/meadow_26.html#SEC317
(defface my-face-tab         '((t (:background "Yellow"))) nil :group 'my-faces)
(defface my-face-zenkaku-spc '((t (:background "LightBlue"))) nil :group 'my-faces)
(defface my-face-spc-at-eol  '((t (:foreground "Red" :underline t))) nil :group 'my-faces)
(defvar my-face-tab         'my-face-tab)
(defvar my-face-zenkaku-spc 'my-face-zenkaku-spc)
(defvar my-face-spc-at-eol  'my-face-spc-at-eol)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-tab append)
     ("　" 0 my-face-zenkaku-spc append)
     ("[ \t]+$" 0 my-face-spc-at-eol append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
;; settings for text file
(add-hook 'ruby-mode-hook
          '(lambda ()
             (progn
               (font-lock-mode t)
               (font-lock-fontify-buffer))))

;; ri-emacs
(setq ri-ruby-script "~/.emacs.d/ri-emacs/ri-emacs.rb")
(setq load-path (cons (expand-file-name "~/.emacs.d/ri-emacs") load-path))
;; (add-hook 'ruby-mode-hook (lambda ()
;;                               (local-set-key 'f1 'ri)
;;                               (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                               (local-set-key 'f4 'ri-ruby-show-args)
;;                               ))

;; install-elisp
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

(setq viper-mode nil)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '1)
(require 'viper)

(fset 'gettext_blkt
   "\C-s'\C-r\C-r\C-m_(\C-s\C-s\C-s\C-m)")

;; 同じファイル名のファイルを開いた際に親ディレクトリ名前を表示する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; tabをspaceに変換する
(require 'untabify-file)

;; auto-complete http://d.hatena.ne.jp/rubikitch/20081109/autocomplete
;; http://dev.ariel-networks.com/Members/matsuyama/auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;; magit http://gitorious.org/projects/magit/repos/mainline
(require 'magit)
(require 'ansi-color)
(put 'downcase-region 'disabled nil)

;; egg git http://github.com/bogolisk/egg/tree/master
;; (setq load-path (cons (expand-file-name "~/.emacs.d/egg") load-path))
;; (require 'egg)

;; rst-mode
(autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
