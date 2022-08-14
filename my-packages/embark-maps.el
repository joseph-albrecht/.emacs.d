
(setq embark-keymap-alist '((file embark-file-map)
                            ;;; (environment-variables embark-file-map)
                            (url embark-url-map)
                            (buffer embark-buffer-map)
                            ;;; (tab embark-tab-map)
                            ;;; (identifier embark-identifier-map)
                            ;;; (defun embark-defun-map)
                            (symbol embark-symbol-map)
                            ;;; (face embark-face-map)
                            (function embark-function-map)
                            (command embark-command-map)
                            (variable embark-variable-map)
                            ;;; (minor-mode embark-command-map)
                            ;;; (package embark-package-map)
                            ;;; (bookmark embark-bookmark-map)
                            (kill-ring embark-kill-ring-map)
                            ;;; (heading embark-heading-map)
                            (t embark-general-map)))

(embark-define-keymap embark-general-map ""
  :parent nil
  ("i" embark-insert)
  ("*" embark-act-all)
  ("w" embark-copy-as-kill)
  ("o" occur))

(defun embark-file-contents-as-kill (file)
  (with-temp-buffer
    (insert-file-contents file)
    (kill-new (buffer-string))))

(embark-define-keymap embark-file-map ""
  :parent embark-general-map
  ("<" insert-file)
  ("," embark-file-contents-as-kill)
  ("j" embark-dired-jump)
  ("w" embark-copy-as-kill)
  ("W" embark-save-relative-path)
  ("f" find-file)
  ("F" find-file-other-window)
  ("c" copy-file)
  ("d" delete-file)
  ("r" rename-file)
  ("O" consult-file-externally)
  ("=" ediff-files)
  ("$" shell-command)
  ("RET" find-file))

(embark-define-keymap embark-url-map ""
  :parent embark-general-map
  ("d" embark-download-url)
  ("e" eww)
  ("RET" browse-url))

(embark-define-keymap embark-buffer-map ""
  :parent embark-general-map
  ("<" insert-buffer)
  ("=" ediff-buffers)
  ("K" embark-kill-buffer-and-window)
  ("b" switch-to-buffer)
  ("k" kill-buffer)
  ("r" embark-rename-buffer)
  ("z" embark-bury-buffer)
  ("|" embark-shell-command-on-buffer))

(embark-define-keymap embark-symbol-map ""
  :parent embark-general-map
  ("a" apropos)
  ("d" embark-find-definition)
  ("h" describe-symbol)
  ("f" xref-find-references))

(embark-define-keymap embark-variable-map ""
  :parent embark-symbol-map
  ("=" set-variable)
  ("v" embark-save-variable-value)
  ("<" embark-insert-variable-value))

(embark-define-keymap embark-function-map ""
  :parent embark-symbol-map
  ;; ("m" elp-instrument-function) ;; m=measure
  ;; ("M" 'elp-restore-function) ;; quoted, not autoloaded
  ;; ("k" debug-on-entry) ;; breaKpoint (running out of letters, really)
  ;; ("K" cancel-debug-on-entry)
  ;; ("t" trace-function)
  ;; ("T" 'untrace-function)
  )

(embark-define-keymap embark-command-map ""
  :parent embark-function-map
  ("x" execute-extended-command)
  ("I" Info-goto-emacs-command-node)
  ("b" where-is)
  ;; ("g" global-set-key)
  ;; ("l" local-set-key)
  )

(embark-define-keymap embark-kill-ring-map
  "Keymap for `kill-ring' commands."
  :parent embark-general-map
  ("d" embark-kill-ring-remove))

(provide 'embark-maps)
