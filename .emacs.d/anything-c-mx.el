(require 'anything)

(defvar anything-c-mx-tmp-buffer-name "*anything M-x*")
(defvar anything-c-mx-tmp-buffer nil)

(defun anything-c-mx-candidates-init ()
  (with-current-buffer
      (setq anything-c-mx-tmp-buffer
            (get-buffer-create anything-c-mx-tmp-buffer-name))
    (erase-buffer)
    (mapatoms (lambda (s) (when (commandp s)
                            (insert (symbol-name s) "\n"))))))

(defun anything-c-mx-get-candidates ()
  (when (or (string-match (rx bol (+ space) eol) anything-pattern)
            (string-equal "" anything-pattern))
    (error "empty string or only one space character causes infinite loop."))
  (let ((pat (replace-regexp-in-string " " ".*" anything-pattern)))

    (sort
     (with-current-buffer anything-c-mx-tmp-buffer
       (goto-char (point-min))
       (loop with cnt = 0
             while (and (< cnt anything-candidate-number-limit)
                        (re-search-forward pat (point-max) t))
               collect (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))
               do (incf cnt)))
     'string<)))

(defvar anything-c-source-mx '((name             . "M-x")
                               (init             . anything-c-mx-candidates-init)
                               (candidates       . anything-c-mx-get-candidates)
                               (type             . command)
                               (requires-pattern . 3)
                               (match            . (identity))
                               (volatile)))

(provide 'anything-c-mx)
