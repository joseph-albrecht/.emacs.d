(setq embark-keymap-alist '((file embark-file-map)
                            ;;; (environment-variables embark-file-map)
                            (url embark-url-map)
                            (buffer embark-buffer-map)
                            (tab embark-tab-map)
                            ;;; (identifier embark-identifier-map)
                            ;;; (defun embark-defun-map)
                            (symbol embark-symbol-map)
                            ;;; (face embark-face-map)
                            (function embark-function-map)
                            (command embark-command-map)
                            (variable embark-variable-map)
                            ;;; (minor-mode embark-command-map)
                            (package embark-package-map)
                            ;;; (bookmark embark-bookmark-map)
                            (kill-ring embark-kill-ring-map)
                            ;;; (heading embark-heading-map)
                            (t embark-general-map)))

(setq embark-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'embark-insert)
    (define-key map "*" #'embark-act-all)
    (define-key map "w" #'embark-copy-as-kill)
    (define-key map "o" #'occur)
    map))

(defun embark-file-contents-as-kill (file)
  (with-temp-buffer
    (insert-file-contents file)
    (kill-new (buffer-string))))

(setq embark-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" #'insert-file)
    (define-key map "," #'embark-file-contents-as-kill)
    (define-key map "j" #'embark-dired-jump)
    (define-key map "w" #'embark-copy-as-kill)
    (define-key map "W" #'embark-save-relative-path)
    (define-key map "f" #'find-file)
    (define-key map "F" #'find-file-other-window)
    (define-key map "c" #'copy-file)
    (define-key map "d" #'delete-file)
    (define-key map "r" #'rename-file)
    (define-key map "O" #'consult-file-externally)
    (define-key map "=" #'ediff-files)
    (define-key map "$" #'shell-command)
    (define-key map "" #'find-file)
    (make-composed-keymap map embark-general-map)))

(setq embark-url-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'embark-download-url)
    (define-key map "e" #'eww)
    (define-key map "" #'browse-url)
    (make-composed-keymap map embark-general-map)))

(setq embark-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" #'insert-buffer)
    (define-key map "=" #'ediff-buffers)
    (define-key map "K" #'embark-kill-buffer-and-window)
    (define-key map "b" #'switch-to-buffer)
    (define-key map "k" #'kill-buffer)
    (define-key map "r" #'embark-rename-buffer)
    (define-key map "z" #'embark-bury-buffer)
    (define-key map "|" #'embark-shell-command-on-buffer)
    (make-composed-keymap map embark-general-map)))

(setq embark-symbol-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'apropos)
    (define-key map "d" #'embark-find-definition)
    (define-key map "h" #'describe-symbol)
    (define-key map "f" #'xref-find-references)
    (make-composed-keymap map embark-general-map)))

(setq embark-variable-map
  (let ((map (make-sparse-keymap)))
    (define-key map "=" #'set-variable)
    (define-key map "v" #'embark-save-variable-value)
    (define-key map "<" #'embark-insert-variable-value)
    (make-composed-keymap map embark-symbol-map)))

(setq embark-function-map
  (let ((map (make-sparse-keymap)))
    (make-composed-keymap map embark-symbol-map)))

(setq embark-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" #'execute-extended-command)
    (define-key map "I" #'Info-goto-emacs-command-node)
    (define-key map "b" #'where-is)
    (make-composed-keymap map embark-function-map)))

(setq embark-kill-ring-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'embark-kill-ring-remove)
    (make-composed-keymap map embark-general-map)))

(setq embark-package-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" #'embark-browse-package-url)
    (make-composed-keymap map embark-symbol-map)))

(provide 'embark-maps)
