;; inspired by the meow package: https://github.com/meow-edit/meow
(require 'evil)

;;TODO: Something about this definition for an evil-state makes C-SPC still
;;      activate visual-mode instead of staying more in an emacs-state
;;      -- 2020 May 1st
(evil-define-state leader
  "Leader state."
  :tag " <L> "
  :message "-- LEADER --"
  :input-method t
  :intercept-esc nil)

(setq evil-leader-state-cursor '("black" box))
(setq evil-emacs-state-cursor '("red" box))
;; TODO: this doesn't work with C-h to show bound keys
(defun evil-leader-call-local-map ()
  (interactive)
  (let ((prefix (cond
		 ((equal '(4) current-prefix-arg)  "\C-u")
		 ((equal '(16) current-prefix-arg) "\C-u\C-u")
		 (t ""))))
    (setq unread-command-events (listify-key-sequence (concat prefix "\C-c")))))

(listify-key-sequence  "\C-u\C-u")
(setq evil-default-state 'leader)

(setq evil-leader-state-map-extension (make-sparse-keymap))
(define-key evil-leader-state-map (kbd "SPC") evil-leader-state-map-extension)
(define-key evil-normal-state-map (kbd "SPC") evil-leader-state-map-extension)
(define-key evil-emacs-state-map (kbd "S-SPC") evil-leader-state-map-extension)
(define-key evil-insert-state-map (kbd "S-SPC") evil-leader-state-map-extension)
(define-key evil-leader-state-map-extension (kbd "x") 'execute-extended-command)
(define-key evil-leader-state-map-extension (kbd "q") 'save-buffers-kill-terminal)

;; buffers
(define-key evil-leader-state-map-extension (kbd "b b") 'switch-to-buffer)
(define-key evil-leader-state-map-extension (kbd "b k") 'kill-buffer)
(define-key evil-leader-state-map-extension (kbd "b K") 'kill-this-buffer)
(define-key evil-leader-state-map-extension (kbd "b .") 'mode-line-other-buffer)
(define-key evil-leader-state-map-extension (kbd "b r") 'rename-buffer)

;; files
(define-key evil-leader-state-map-extension (kbd "f f") 'find-file)
(define-key evil-leader-state-map-extension (kbd "f s") 'save-buffer)
(define-key evil-leader-state-map-extension (kbd "f S") 'save-some-buffers)

;; dired
(define-key evil-leader-state-map-extension (kbd "d d") 'dired-jump)
(define-key evil-leader-state-map-extension (kbd "d f") 'dired)

;; project
(define-key evil-leader-state-map-extension (kbd "C-p") 'project-switch-project)

;; eval
(define-key evil-leader-state-map-extension (kbd "e l") 'eval-last-sexp)
(define-key evil-leader-state-map-extension (kbd "e d") 'eval-defun)
(define-key evil-leader-state-map-extension (kbd "e b") 'eval-buffer)
(define-key evil-leader-state-map-extension (kbd "e r") 'eval-region)
(define-key evil-leader-state-map-extension (kbd "e c") 'compile)
(define-key evil-leader-state-map-extension (kbd "e C") 'recompile)
(define-key evil-leader-state-map-extension (kbd "e s") 'shell-command)
(define-key evil-leader-state-map-extension (kbd "e S") 'async-shell-command)
(define-key evil-leader-state-map-extension (kbd "e $") 'shell)

;; edit
(define-key evil-leader-state-map-extension (kbd "a r") 'align-regexp)

;; windows
(define-key evil-leader-state-map-extension (kbd "t w") 'ace-window)
(define-key evil-leader-state-map-extension (kbd "t o") 'delete-other-windows)
(define-key evil-leader-state-map-extension (kbd "t k") 'delete-window)
(define-key evil-leader-state-map-extension (kbd "t h") 'windmove-left)
(define-key evil-leader-state-map-extension (kbd "t l") 'windmove-right)
(define-key evil-leader-state-map-extension (kbd "t p") 'windmove-up)
(define-key evil-leader-state-map-extension (kbd "t n") 'windmove-down)
(define-key evil-leader-state-map-extension (kbd "t |") 'split-window-right)
(define-key evil-leader-state-map-extension (kbd "t -") 'split-window-below)
(define-key evil-leader-state-map-extension (kbd "t v") 'scroll-up-command)
(define-key evil-leader-state-map-extension (kbd "t ^") 'scroll-down-command)
(define-key evil-leader-state-map-extension (kbd "t b") 'balance-windows)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "TAB TAB") 'tab-bar-select-tab-by-name)
(define-key evil-leader-state-map-extension (kbd "TAB n") 'tab-bar-new-tab)
(define-key evil-leader-state-map-extension (kbd "TAB r") 'tab-bar-rename-tab)
(define-key evil-leader-state-map-extension (kbd "TAB R") 'tab-bar-rename-tab-by-name)
(define-key evil-leader-state-map-extension (kbd "TAB d") 'tab-bar-close-tab-by-name)
(define-key evil-leader-state-map-extension (kbd "TAB D") 'tab-bar-close-tab)
(define-key evil-leader-state-map-extension (kbd "TAB o") 'tab-bar-close-other-tabs)

;; search
(define-key evil-leader-state-map-extension (kbd "s s") 'isearch-forward)
(define-key evil-leader-state-map-extension (kbd "s r") 'isearch-backward)
(define-key evil-leader-state-map-extension (kbd "s q") 'query-replace)
(define-key evil-leader-state-map-extension (kbd "s C-q") 'query-replace-regexp)

;; help
(define-key evil-leader-state-map-extension (kbd "h f") 'describe-function)
(define-key evil-leader-state-map-extension (kbd "h F") 'describe-face)
(define-key evil-leader-state-map-extension (kbd "h k") 'describe-key)
(define-key evil-leader-state-map-extension (kbd "h K") 'describe-keymap)
(define-key evil-leader-state-map-extension (kbd "h v") 'describe-variable)
(define-key evil-leader-state-map-extension (kbd "h m") 'describe-mode)
(define-key evil-leader-state-map-extension (kbd "h s") 'describe-symbol)
(define-key evil-leader-state-map-extension (kbd "h l") 'view-lossage)
(define-key evil-leader-state-map-extension (kbd "h e") 'view-echo-area-messages)

;; open
(define-key evil-leader-state-map-extension (kbd "o s") 'shell)
(define-key evil-leader-state-map-extension (kbd "o b") 'bookmark-jump)
(define-key evil-leader-state-map-extension (kbd "o +") 'bookmark-set)
(define-key evil-leader-state-map-extension (kbd "o p") 'list-processes)
(defun open-init () (interactive) (find-file (format "%s/init.el" user-emacs-directory)))
(define-key evil-leader-state-map-extension (kbd "o i") 'open-init)

;; local
(define-key evil-leader-state-map-extension (kbd "SPC") 'evil-leader-call-local-map)

(provide 'evil-leader)
