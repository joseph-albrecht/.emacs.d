;; inspired by the meow package: https://github.com/meow-edit/meow
(require 'evil)

;;TODO: Something about this definition for an evil-state makes C-SPC still
;;      activate visual-mode instead of staying more in an emacs-state
;;      It is identical to (evil-define-state emacs ...) though.
;;      -- 2022 May 1st
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

(setq evil-default-state 'leader)

(setq evil-leader-state-map-extension (make-sparse-keymap))
(define-key evil-leader-state-map (kbd "<escape>") 'evil-normal-state)
(global-set-key (kbd "M-SPC") evil-leader-state-map-extension)
(define-key evil-leader-state-map (kbd "SPC") evil-leader-state-map-extension)
(define-key evil-normal-state-map (kbd "SPC") evil-leader-state-map-extension)
(define-key evil-leader-state-map-extension (kbd "x") 'execute-extended-command)
(define-key evil-leader-state-map-extension (kbd "q") 'save-buffers-kill-terminal)

;; states
(define-key evil-leader-state-map-extension (kbd ". n") 'evil-normal-state)
(define-key evil-leader-state-map-extension (kbd ". e") 'evil-emacs-state)
(define-key evil-leader-state-map-extension (kbd ". l") 'evil-leader-state)

;; buffers
(define-key evil-leader-state-map-extension (kbd "b b") 'switch-to-buffer)
(define-key evil-leader-state-map-extension (kbd "b D") 'dired-jump)
(define-key evil-leader-state-map-extension (kbd "b k") 'kill-buffer)
(define-key evil-leader-state-map-extension (kbd "b K") 'kill-this-buffer)
(define-key evil-leader-state-map-extension (kbd "b .") 'mode-line-other-buffer)
(define-key evil-leader-state-map-extension (kbd "b r") 'rename-buffer)

;; files
(define-key evil-leader-state-map-extension (kbd "f f") 'find-file)
(define-key evil-leader-state-map-extension (kbd "f s") 'save-buffer)
(define-key evil-leader-state-map-extension (kbd "f S") 'save-some-buffers)
(define-key evil-leader-state-map-extension (kbd "f w") 'write-file)
(define-key evil-leader-state-map-extension (kbd "f r") 'revert-buffer)

(defun dired-default-directory+ ()
  (interactive)
  (find-file default-directory))

;; dired
(define-key evil-leader-state-map-extension (kbd "d d") 'dired)
(define-key evil-leader-state-map-extension (kbd "d b") 'dired-jump)
(define-key evil-leader-state-map-extension (kbd "d D") 'dired-default-directory+)

;; project
(define-key evil-leader-state-map-extension (kbd "C-p") 'project-switch-project)

;; insert
(define-key evil-leader-state-map-extension (kbd "i c") 'insert-char)

;; eval
(define-key evil-leader-state-map-extension (kbd "e l") 'eval-last-sexp)
(define-key evil-leader-state-map-extension (kbd "e d") 'eval-defun)
(define-key evil-leader-state-map-extension (kbd "e b") 'eval-buffer)
(define-key evil-leader-state-map-extension (kbd "e r") 'eval-region)
(define-key evil-leader-state-map-extension (kbd "e c") 'compile)
(define-key evil-leader-state-map-extension (kbd "e C") 'recompile)
(define-key evil-leader-state-map-extension (kbd "e s") 'shell-command)
(define-key evil-leader-state-map-extension (kbd "e S") 'async-shell-command)
(define-key evil-leader-state-map-extension (kbd "e $") 'shell-command-on-region)
(define-key evil-leader-state-map-extension (kbd "e e") 'eval-expression)

;; edit
(define-key evil-leader-state-map-extension (kbd "c r") 'align-regexp)
(define-key evil-leader-state-map-extension (kbd "c k") 'keep-lines)
(define-key evil-leader-state-map-extension (kbd "c f") 'flush-lines)
(define-key evil-leader-state-map-extension (kbd "c q") 'query-replace)
(define-key evil-leader-state-map-extension (kbd "c Q") 'query-replace-regexp)
(define-key evil-leader-state-map-extension (kbd "c m") 'apply-macro-to-region-lines)
(define-key evil-leader-state-map-extension (kbd "c SPC") 'just-one-space)

;; windows
(define-key evil-leader-state-map-extension (kbd "t w") 'ace-window)
(define-key evil-leader-state-map-extension (kbd "t o") 'delete-other-windows)
(define-key evil-leader-state-map-extension (kbd "t k") 'delete-window)
(define-key evil-leader-state-map-extension (kbd "t h") 'windmove-left)
(define-key evil-leader-state-map-extension (kbd "t l") 'windmove-right)
(define-key evil-leader-state-map-extension (kbd "t p") 'windmove-up)
(define-key evil-leader-state-map-extension (kbd "t n") 'windmove-down)
(define-key evil-leader-state-map-extension (kbd "t |") 'split-window-right)
(define-key evil-leader-state-map-extension (kbd "t 2") 'split-window-right)
(defun split-window-thrice ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))
(define-key evil-leader-state-map-extension (kbd "t 3") 'split-window-thrice)
(define-key evil-leader-state-map-extension (kbd "t -") 'split-window-below)
(define-key evil-leader-state-map-extension (kbd "t v") 'scroll-up-command)
(define-key evil-leader-state-map-extension (kbd "t ^") 'scroll-down-command)
(define-key evil-leader-state-map-extension (kbd "t =") 'balance-windows)
(define-key evil-leader-state-map-extension (kbd "t m") 'switch-to-minibuffer)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "t f k") 'delete-frame)
(define-key evil-leader-state-map-extension (kbd "TAB TAB") 'tab-bar-select-tab-by-name)
(defun tab-bar-new-tab+ ()
  (interactive)
  (let ((name (read-string "Tab name: ")))
    (call-interactively #'tab-bar-duplicate-tab)
    (tab-bar-rename-tab name)
    (message "Created tab [%s]" name)))
(define-key evil-leader-state-map-extension (kbd "TAB n") 'tab-bar-new-tab+)
(define-key evil-leader-state-map-extension (kbd "TAB r") 'tab-bar-rename-tab)
(define-key evil-leader-state-map-extension (kbd "TAB R") 'tab-bar-rename-tab-by-name)
(define-key evil-leader-state-map-extension (kbd "TAB k") 'tab-bar-close-tab-by-name)
(define-key evil-leader-state-map-extension (kbd "TAB K") 'tab-bar-close-tab)
(define-key evil-leader-state-map-extension (kbd "TAB o") 'tab-bar-close-other-tabs)
(define-key evil-leader-state-map-extension (kbd "TAB l") 'tab-bar-switch-to-next-tab)
(define-key evil-leader-state-map-extension (kbd "TAB h") 'tab-bar-switch-to-prev-tab)

;; search
(define-key evil-leader-state-map-extension (kbd "s s") 'isearch-forward)
(define-key evil-leader-state-map-extension (kbd "s r") 'isearch-backward)

;; help
(define-key evil-leader-state-map-extension (kbd "h f") 'describe-function)
(define-key evil-leader-state-map-extension (kbd "h F") 'describe-face)
(define-key evil-leader-state-map-extension (kbd "h k") 'describe-key)
(define-key evil-leader-state-map-extension (kbd "h K") 'describe-keymap)
(define-key evil-leader-state-map-extension (kbd "h v") 'describe-variable)
(define-key evil-leader-state-map-extension (kbd "h m") 'describe-mode)
(define-key evil-leader-state-map-extension (kbd "h s") 'describe-symbol)
(define-key evil-leader-state-map-extension (kbd "h p") 'describe-package)
(define-key evil-leader-state-map-extension (kbd "h l") 'view-lossage)
(define-key evil-leader-state-map-extension (kbd "h e") 'view-echo-area-messages)

;; open
(define-key evil-leader-state-map-extension (kbd "o s") 'shell)
(define-key evil-leader-state-map-extension (kbd "o e") 'ielm)
(define-key evil-leader-state-map-extension (kbd "o b") 'bookmark-jump)
(define-key evil-leader-state-map-extension (kbd "o +") 'bookmark-set)
(define-key evil-leader-state-map-extension (kbd "o p") 'list-processes)
(defun open-init () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))
(defun open-guide () (interactive) (find-file (expand-file-name "guide.org" user-emacs-directory)))
(define-key evil-leader-state-map-extension (kbd "o i") 'open-init)
(define-key evil-leader-state-map-extension (kbd "o g") 'open-guide)

;; local
(define-key evil-leader-state-map-extension (kbd "SPC") 'evil-leader-call-local-map)

(provide 'evil-leader)
