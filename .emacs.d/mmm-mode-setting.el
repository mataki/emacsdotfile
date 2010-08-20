(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-output-submode-face  "Blue")
(set-face-background 'mmm-code-submode-face    "Black")
(set-face-background 'mmm-comment-submode-face "Yellow")
(set-face-background 'mmm-special-submode-face "Yellow")

(mmm-add-classes
 '(
; eRubyモード
   (erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
; ヒアドキュメント用に、ruby-mode内でtext-modeを使えるようにします。
   (ruby-heredoc
    :front "<<-\\([a-zA-Z0-9_-]+\\)"
    :front-offset (end-of-line 1)
    :back "~1$"
    :save-matches 1
    :submode text-mode
    :insert ((?d ruby-heredoc "Here-document Name: " @ "<<" str _ "\n"
                 @ "\n" @ str "\n" @))
    )
; Javascript-mode js2-mode
   (html-script
    :submode js2-mode
    :delimiter-mode nil
    :front "<script\[^>\]*\\(language=[\"|\']javascript\\([0-9.]*\\)[\"|']\\|type=[\"|']text/javascript[\"|']\\)\[^>\]*>\n"
    :back"</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
   )
   ))

(add-hook 'html-mode-hook
          '(lambda ()
             (setq mmm-classes '(html-script erb-code embedded-css))
             (mmm-mode-on)))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (setq mmm-classes '(html-script embedded-css))
             (mmm-mode-on)))
