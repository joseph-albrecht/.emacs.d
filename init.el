;;; -*- lexical-binding: t -*-
;; packages to try
;; https://github.com/emacs-citar/citar
;; debug something something for jira ticket number clicking
;; try to fix vterm window sizing issue
;; vertico float for ultra tall mode
;; some things that are going into evil-leader should be on the local map
;;   to have consistency I should create a "programming" map that I can apply
;;   whenever I need access and then i'll still have consistent bindings
;;     flymake
;;     java
;;     lsp
;;     dap

;; install brew from: https://brew.sh
;; `brew tap d12frosted/emacs-plus`
;; `brew install emacs-plus@29 --with-native-comp`

(setq debug-on-error t)
(setq debugger-stack-frame-as-list t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(setq my-package-dir (concat (expand-file-name user-emacs-directory) "my-packages/"))

(setq use-package-enable-imenu-support t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :custom ((use-package-hook-name-suffix "")
           (use-package-debug t)
           (use-package-verbose t)))

(use-package emacs
  :after (evil-leader embark)
  :demand t
  :hook (text-mode-hook . toggle-show-trailing-whitespace)
  :commands (insert-time-id
             eval-region+
             eval-buffer+
             eval-region-and-replace
	     toggle-show-trailing-whitespace
	     select-from-history
             mirror-window
             conform-frame-to-monitor
             shell-command-on-region+
             keep-lines+
             flush-lines+
             isearch-abort+)
  :bind (("M-n" . open-next-file-in-directory+)
         ("M-p" . open-previous-file-in-directory+)
         ("C-x f" . find-file)
         ("C-x >" . scroll-left)
         ("C-x <" . scroll-right)
         ("s-l" . windmove-right)
         ("s-h" . windmove-left)
         ("s-p" . windmove-up)
         ("s-n" . windmove-down)
         ("s-f" . find-file)
         ("s-," . mode-line-other-buffer)
         ("s-." . delete-other-windows)
         ("s--" . delete-window)
         ("s-+" . copy-window)

         :map isearch-mode-map
         ("C-g" . isearch-abort+)
         :map minibuffer-mode-map
	 ("M-p" . nil)
	 ("M-n" . nil)
         ("C-c e" . edit-minibuffer)
	 ("C-c h" . select-from-history)
	 ("C-c D" . select-subdirectory+)
	 :map evil-leader-state-map-extension
	 ("C-c" . server-edit)
	 ("e r" . eval-region+)
	 ("e b" . eval-buffer+)
	 ("v +" . text-scale-increase)
	 ("v -" . text-scale-decrease)
	 ("e L" . eval-expression-and-replace)
         ("e $" . shell-command-on-region+)
         ("c $" . shell-command-on-region+)
	 ("c k" . keep-lines)
         ("c f" . flush-lines)
         ("c w" . delete-trailing-whitespace)
	 ("f w" . kill-filepath)
	 ("f e" . echo-filepath)
	 ("v w" . toggle-show-trailing-whitespace)
	 ("v h" . global-hl-line-mode)
	 ("v m" . conform-frame-to-monitor)
	 ("v e" . setenv)
	 ("v l" . display-line-numbers-mode)
         ("t C" . 'copy-window)
         ("h g" . 'open-guide)
         ("r"   . 'random-line-jump)
         ("R"   . 'random-line)
         ("b h ." . 'highlight-symbol-at-point)
         ("b h r" . 'highlight-lines-matching-regexp)
         ("b h u" . 'unhighlight-regexp)
         ("b h U" . 'unhighlight-regexp-all+)
         ("b h p" . 'highlight-phrase)
         ("i r c" . 'insert-regexp-char-class)
         ("C-d" . run-command-with-default-dir))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun unhighlight-regexp-all+ ()
    (interactive)
    (unhighlight-regexp t))

  (defun server-edit-back-to-terminal+ ()
    (interactive)
    (let ((frame (selected-frame)))
      (when (not server-buffer-clients)
        (with-selected-frame frame
          (when (< 1 (length (frame-list)))
            (delete-frame frame)))
        (shell-command "open -a iTerm"))))

  (setq warning-minimum-level :error)
  (setq read-minibuffer-restore-windows nil)
  (advice-remove 'server-edit #'server-edit-back-to-terminal+)

  (defun print-buffer-to-stdout ()
    (print (buffer-substring-no-properties (point-min) (point-max))))

  (when (file-exists-p "/opt/homebrew/bin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/bin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/bin"))))
  (when (file-exists-p "/opt/homebrew/sbin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/sbin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/sbin"))))

  (setq split-width-threshold 120
	split-height-threshold 9999)
  (setq display-buffer-alist
	'(("\\*Help\\*"
	   (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-window)
	   (side . right))
	  ("\\*Embark Collect"
	   (display-buffer-reuse-window))
	  ;; ("\\*Embark Export"
	  ;;  (display-buffer-reuse-window display-buffer-pop-up-window)
	  ;;  (inhibit-same-window . t))
	  ("\\*Messages\\*"
	   (display-buffer-reuse-window)
	   (inhibit-same-window . t))))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (fringe-mode nil)

  (setq large-file-warning-threshold nil)

  ;; brew search font-
  ;; brew install font-
  ;; Terminus (TTF)
  ;; Agave
  ;; Iosevka
  ;; Iosevka Comfy
  ;; Input Mono
  ;; Cascadia Mono
  ;; Hack
  (condition-case nil
      (set-face-attribute 'default nil :font "Iosevka comfy" :height 160)
      ;; (set-face-attribute 'default nil :font "Iosevka" :height 160)
    (error (set-face-attribute 'default nil :height 120)))

  (set-face-attribute 'region nil :background "#A0F5F4")
  (set-cursor-color "#007F00")

  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (blink-cursor-mode 0)

  (setq-default fill-column 85)

  (indent-tabs-mode -1)
  (setq-default indent-tabs-mode nil)

  (defun toggle-show-trailing-whitespace ()
    (interactive)
    (setq show-trailing-whitespace (not show-trailing-whitespace))
    (font-lock-update))

  (defun copy-window ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1))

  (defun pred-width (height) (* height iosevka-height-to-width))

  (setq ultrawide-34-ratio (/ 3440.0 1400))
  (setq iosevka-rows-to-cols (/ 64.0 188))
  (setq ultrawide-38-ratio (/ 3440.0 1400))
  (setq mac-retina-ratio (/ 3440.0 1400))

  (setq monitor-attributes '(("ultrawide" . ((font-height . 170)
					     (frame-width . 190)))
                             ("ultrawide demo" . ((font-height . 220)
					          (frame-width . 165)))
			     ("mac" . ((font-height . 140)
				       (frame-width . 215)))
                             ("mac demo" . ((font-height . 180)
					    (frame-width . 180)))))

  (defun conform-frame-to-monitor ()
    (interactive)
    (let* ((monitor-settings (cdr (assoc (completing-read "select monitor: "
						          monitor-attributes)
				         monitor-attributes)))
	   (font-height (cdr (assoc 'font-height monitor-settings)))
	   (frame-width (cdr (assoc 'frame-width monitor-settings))))
      (message "monitor-settings: %s font-height: %s frame-width: %s" monitor-settings font-height frame-width)
      (set-face-attribute 'default nil
			  :height font-height)
      (set-frame-size (selected-frame) frame-width 80)))

  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-right-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'meta)
  ;; (define-key input-decode-map [?\C-m] [C-m]) ;;; allow C-m in GUI emacs

  (setq completion-cycle-threshold nil)
  (setq tab-always-indent 'complete) ;;; TODO: maybe use a different keybinding for completion

  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers nil)
  (setq enable-recursive-minibuffers t)

  (setq backup-directory-alist
 	`((".*" . "~/.emacs.d/backups-and-autosaves/")))
  (setq auto-save-file-name-transforms
 	`((".*" "~/.emacs.d/backups-and-autosaves/" t)))
  (setq create-lockfiles nil)

  (setq confirm-kill-processes nil)
  (setq kill-buffer-query-functions nil)

  (defun clear-line ()
    (interactive)
    (let ((start (progn (move-beginning-of-line 1)
                        (point)))
          (end   (progn (move-end-of-line 1)
                        (point))))
      (delete-region start end)))

  (defun clear-input ()
    (interactive)
    (let ((start (progn (move-beginning-of-line 1)
                        (point)))
          (end   (point-max)))
      (delete-region start end)))

  (defun select-from-history ()
    (interactive)
    (let ((history (append minibuffer-default
                           (seq-uniq (minibuffer-history-value))))
	  (vertico-sort-override-function #'identity))
      (if (> (length history) 0)
	  (let ((chosen-history (completing-read "input history: " history)))
	    (clear-input)
	    (insert chosen-history))
	(message "no history items"))))

  (defun kill-filepath ()
    (interactive)
    (let ((path (or (buffer-file-name) (expand-file-name default-directory))))
      (kill-new path)
      (message "added to kill ring: %s" path)))

  (defun echo-filepath ()
    (interactive)
    (message (or (buffer-file-name) default-directory)))

  (defun eval-region+ ()
    (interactive)
    (let ((start (region-beginning))
          (end   (region-end)))
      (eval-region start end)
      (message "eval-region done.")))

  (defun eval-buffer+ ()
    (interactive)
    (eval-buffer)
    (message "eval-buffer done."))

  ;;; TODO: make this work on regions as well
  (defun eval-expression-and-replace ()
    (interactive)
    (let* ((start (progn (thing-at-point--beginning-of-sexp) (point)))
           (end   (progn (thing-at-point--end-of-sexp)       (point)))
           (exp   (read (buffer-substring start end)))
           (value (eval exp)))
      (delete-region start end)
      (insert (format "%s" value))))

  (setq confirm-kill-processes nil)

  (defun switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active")))

  ;; use current-minibuffer-command to change mode
  (defun edit-minibuffer ()
    (interactive)
    (ignore-errors (kill-buffer "*minibuffer contents*"))
    (display-buffer-in-side-window (get-buffer-create "*minibuffer contents*")
				   '((side . bottom)))
    (let ((minibuffer (minibuffer-contents))
	  (window (get-buffer-window (get-buffer "*minibuffer contents*"))))
      (select-window window)
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key (kbd "C-c C-c") 'edit-minibuffer-save)
      (setq header-line-format "Press C-c C-c to save changes")
      (insert minibuffer)
      (evil-insert 0)))

  (defun edit-minibuffer-save ()
    (interactive)
    (select-window (get-buffer-window (get-buffer "*minibuffer contents*")))
    (let ((minibuffer-contents (buffer-string)))
      (kill-current-buffer)
      (switch-to-minibuffer)
      (beginning-of-line)
      (ignore-errors (kill-line))
      (insert minibuffer-contents)))

  (defun isearch-abort+ ()
    (interactive)
    (if (not defining-kbd-macro)
        (isearch-abort)
      (isearch-printing-char ?a) ;;; required to not enter weird "search" state
      (isearch-exit)
      (if evil-mode
          (evil-jump-backward)
        (pop-to-mark-command))))

  (defun undefined ()
    "Beep to tell the user this binding is undefined."
    (declare (completion ignore))
    (interactive)
    (ding t)
    (message "%s is undefined" (key-description (this-single-command-keys)))
    (force-mode-line-update)
    ;; If this is a down-mouse event, don't reset prefix-arg;
    ;; pass it to the command run by the up event.
    (setq prefix-arg
          (when (memq 'down (event-modifiers last-command-event))
            current-prefix-arg)))

  (defun shell-command-on-region+ (start end command &optional output)
    (interactive (let ((start (if (region-active-p) (region-beginning) (point)))
                       (end (if (region-active-p) (region-end)       (point)))
                       (output (if current-prefix-arg
                                   (intern (completing-read "output: " '(buffer echo replace)))
                                 'replace))
                       (command (read-shell-command "Shell command on region: "))
                       )
                   (list start end command output)))
    (let ((buffer (when (equal output 'buffer)
                    (get-buffer-create (format "*shell-command* (%s) %s"
                                               (buffer-name)
                                               command)))))
      (shell-command-on-region start end command
                               buffer
                               (equal output 'replace))
      (when (equal output 'buffer) (pop-to-buffer buffer))))

  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it))

  (defun open-guide ()
    (interactive)
    (find-file "~/.emacs.d/guide.org"))

  (defun run-command-with-default-dir (dir)
    (interactive (list (read-directory-name "in directory: ")))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t)
          (map (make-composed-keymap evil-leader-state-map (current-global-map))))
      (message "waiting for command to run in dir: %s" dir)
      (call-interactively (funcall embark-prompter map #'indentity))))

  (setq regexp-char-classes
        '("[:ascii:]"
          "[:alnum:]"
          "[:alpha:]"
          "[:blank:]"
          "[:cntrl:]"
          "[:digit:]"
          "[:graph:]"
          "[:lower:]"
          "[:multibyte:]"
          "[:nonascii:]"
          "[:print:]"
          "[:punct:]"
          "[:space:]"
          "[:unibyte:]"
          "[:upper:]"
          "[:word:]"
          "[:xdigit:]"))

  (defun insert-regexp-char-class ()
    (interactive)
    (insert (completing-read "regexp class: " regexp-char-classes)))

  (defun toggle-window-dedicated-p (&optional window)
    "Toggle WINDOW's dedicated flag.
Also set its `no-delete-other-windows' parameter to match."
    (interactive)
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (set-window-parameter window 'no-delete-other-windows
                          (window-dedicated-p window))
    (message "Dedicated: %s" (window-dedicated-p window)))
  (defun select-subdirectory+ ()
    "Select a subdirectory under default-directory."
    (interactive)
    (let* ((all-dirs (ignore-errors
                       (directory-files-recursively default-directory "" t)))
           (dir (and all-dirs (completing-read "Select directory: " all-dirs))))
      (if (and dir (file-directory-p dir))
          (insert dir)
        (message "Not a valid directory!"))))
  (global-hl-line-mode t)
  (setq hl-line-sticky-flag nil)

  (defun open-next-file-in-directory+ ()
    "Open the next file in the current directory."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (all-files (directory-files (file-name-directory current-file) t))
           (sorted-files (cl-sort (cl-remove-if-not #'file-regular-p all-files) 'string<))
           (current-index (cl-position current-file sorted-files :test 'string=)))
      (if current-index
          (let* ((next-index (mod (1+ current-index) (length sorted-files)))
                 (next-file (nth next-index sorted-files)))
            (find-file next-file))
        (message "Current file is not in the directory listing."))))

  (defun open-previous-file-in-directory+ ()
    "Open the next file in the current directory."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (all-files (directory-files (file-name-directory current-file) t))
           (sorted-files (cl-sort (cl-remove-if-not #'file-regular-p all-files) 'string<))
           (current-index (cl-position current-file sorted-files :test 'string=)))
      (if current-index
          (let* ((next-index (mod (1- current-index) (length sorted-files)))
                 (next-file (nth next-index sorted-files)))
            (find-file next-file))
        (message "Current file is not in the directory listing."))))

  (defun remove-all-properties (str)
    "Remove all text properties from the given string STR."
    (set-text-properties 0 (length str) nil str)
    str)




  )

(use-package solarized-theme
  :after (org orderless)
  :ensure t
  :config
  (load-theme 'solarized-light t)

  (set-face-attribute 'org-headline-done nil :strike-through t)
  (set-face-attribute 'org-level-1 nil :height 1.0 :inherit 'default)
  (set-face-attribute 'org-level-2 nil :height 1.0 :inherit 'default)
  (set-face-attribute 'org-level-3 nil :height 1.0 :inherit 'default)
  (set-face-attribute 'org-level-4 nil :height 1.0 :inherit 'default)

  (set-face-attribute 'orderless-match-face-0 nil :foreground "black" :overline nil :underline nil :bold t)
  (set-face-attribute 'orderless-match-face-1 nil :foreground "black" :overline nil :underline nil :bold t)
  (set-face-attribute 'orderless-match-face-2 nil :foreground "black" :overline nil :underline nil :bold t)
  (set-face-attribute 'orderless-match-face-3 nil :foreground "black" :overline nil :underline nil :bold t))

(use-package grep
  :after (compile)
  :bind (:map grep-mode-map
	      ("n" . next-line)
	      ("p" . previous-line)
	      ("M-n" . next-error+)
	      ("M-p" . previous-error+)
              ("M-<return>" . compile-goto-error-no-select)
              ("d" . compile-delete-line+)
              ("D" . compile-delete-line-no-select+))
  :config
  (setq grep-command "grep --color=auto -nr"))

(use-package replace
  :after (compile)
  :demand t
  :commands (occur-mode-goto-occurrence-no-select compile-delete-line+)
  :bind (:map occur-mode-map
              ("n" . next-line)
	      ("p" . previous-line)
	      ("M-n" . next-error+)
	      ("M-p" . previous-error+)
              ("M-<return>" . occur-mode-goto-occurrence-no-select)
              ("<return>" . occur-mode-goto-occurrence)
              ("d" . compile-delete-line+))
  :config
  (defun occur-mode-goto-occurrence-no-select ()
    (interactive)
    (let ((window (selected-window)))
      (occur-mode-goto-occurrence)
      (select-window window))))

(use-package compile
  :after (f evil)
  :demand t
  :commands (next-error+ previous-error+ compile-goto-error-no-select compile-delete-line+)
  :bind (("C-M-n" . next-error)
         ("C-M-p" . previous-error)
         :map evil-leader-state-map-extension
	 ("p C" . project-compile-menu)
         ("e c" . compile)
         ("e C" . recompile)
         ("e M-c" . recompile+)
         :map compilation-mode-map
	 ("n" . next-line)
	 ("p" . previous-line)
	 ("M-n" . next-error+)
	 ("M-p" . previous-error+)
         ("M-<return>" . compile-goto-error-no-select)
	 ("d" . compile-delete-line+)
         ("D" . compile-delete-line-no-select+))
  :config
  (setq compilation-environment '("TERM=tmux-256color"))
  (setq compilation-scroll-output t)

  (defun compile-goto-error-no-select ()
    (interactive)
    (let ((window (selected-window)))
      (compile-goto-error)
      (select-window window)))

  (defun next-error+ ()
    (interactive)
    (next-error-select-buffer (current-buffer))
    (ignore-errors (compile-goto-error-no-select))
    (next-error-no-select))

  (defun previous-error+ ()
    (interactive)
    (next-error-select-buffer (current-buffer))
    (ignore-errors (compile-goto-error-no-select))
    (previous-error-no-select))

  (defun compilation--buffer-name (mode)
    (concat "*compilation* (" (f-short default-directory) "): " (or (car compilation-arguments) compile-command)))
  (setq compilation-buffer-name-function 'compilation--buffer-name)

  (evil-add-command-properties 'compile-goto-error :jump t)
  (evil-add-command-properties 'next-error :jump t)
  (evil-add-command-properties 'previous-error :jump t)

  (defun compile-delete-line+ ()
    (interactive)
    (compile-delete-line-no-select+)
    (previous-line)
    (next-error+))

  (defun compile-delete-line-no-select+ ()
    (interactive)
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char 1)
      (when (equal (line-number-at-pos (point-max))
                   (line-number-at-pos (point)))
        (previous-line))
      (previous-line)
      (next-line)))

  ;; TODO: would a compile transient command be nice?
  ;; default to current directory
  ;; allow adding a command
  ;; executing

  (defun recompile+ ()
    "Edit the last compile command than run it in a compile buffer"
    (interactive)
    (let* ((dir (or compilation-directory default-directory)))
      (let ((default-directory dir))
        (call-interactively #'compile))))

  (defun compile+ (current-dir)
    "Select a directory, enter a command, then run it in compile buffer."
    (interactive "P")
    (let ((default-directory (if current-dir default-directory (read-directory-name "directory: ")))
          (current-prefix-arg nil))
      (call-interactively #'compile)))

  (put 'project-compile-commands 'safe-local-variable #'listp)
  (defun make-command-assoc-list (command-list)
    (let* ((dir-text (propertize "DIR" 'face 'bold))
           (command-text (propertize "COMMAND" 'face 'bold))
           (max-dir-length (apply 'max (mapcar (lambda (pair)
                                                 (length (format "%s%s" (project-root (project-current)) (car pair))))
                                               command-list))))
      (mapcar (lambda (pair)
                (let* ((dir (format "%s%s" (project-root (project-current)) (car pair)))
                       (cmd (cdr pair))
                       (combined-key (format (concat "%s: %-" (number-to-string max-dir-length) "s %s: %s")
                                             dir-text
                                             dir
                                             command-text
                                             cmd)))
                  (cons combined-key (cons dir cmd))))
              command-list)))

  (defun get-project-compile-commands ()
    (interactive)
    (let* ((dir (project-root (project-current)))
           (path (format "%s%s" dir ".compile-commands.el"))
           (buffer (find-buffer-visiting path)))
      (unless buffer
        (setq buffer (find-file-noselect path)))
      (with-current-buffer buffer
        (let* ((buffer-contents (buffer-string)))
          (read buffer-contents)))))

  (defun project-compile-menu ()
    "given a selection of commands, choose one and run it."
    (interactive)
    (let* ((command-assoc-list (make-command-assoc-list (get-project-compile-commands)))
           (command-name (completing-read "select command: " command-assoc-list))
           (dir-and-command (cdr (assoc command-name command-assoc-list)))
           (default-directory (car dir-and-command)))
      (compile (cdr dir-and-command))))

  (defun compilation-read-command (command)
    (let* ((dir-text (propertize "DIR" 'face 'bold))
           (command-text (propertize "COMMAND" 'face 'bold))
           (current-dir default-directory)
           (prompt (format "%s: (%s)\n%s: " dir-text current-dir command-text)))
      ;; Execute the shell command with `shell-command`
      (read-shell-command prompt command)))

  (defun append-to-zsh-history (&rest r)
    (shell-command (format "echo '%s' >> ~/.zsh_history" (car r))))

  (advice-add 'compile :after #'append-to-zsh-history))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter-hook . (lambda () (ansi-color-apply-on-region compilation-filter-start (point)))))

;;; TODO: i'd prefer to use a normal key-map here. can i write something?
(use-package project
  :after (evil-leader embark consult)
  :demand t
  :commands (project-dir-.emacs.d
	     project-switch-current)
  :bind (:map evil-leader-state-map-extension
              ("P" . project-current-run-command+)
              ("C-p" . project-run-command+)
              ("f a" . find-file-no-ignores)
              ("f p" . project-find-file)
              ("b p" . consult-project-buffer)
              ("d p" . project-find-dir)
              ("c C-q" . project-query-replace-regexp))
  :config
  (defun lines-in-file-matching-re (file regexp)
    (with-temp-buffer
      (insert-file-contents file)
      (keep-lines regexp)
      (->> (split-string (buffer-string) "\n")
           (seq-remove #'string-empty-p))))

  (defun makefile-from-dir (dir)
    (cond
     ((file-exists-p (format "%s/GNUmakefile" dir)) (format "%s/GNUmakefile" dir))
     ((file-exists-p (format "%s/makefile" dir)) (format "%s/makefile" dir))
     ((file-exists-p (format "%s/Makefile" dir)) (format "%s/Makefile" dir))))

  (defun project-make+ ()
    "Select a command from the project makefile and make."
    (interactive)
    (let* ((project-dir (project-root (project-current nil)))
           (default-dir project-dir)
           (makefile (makefile-from-dir project-dir))
           (target-regexp "^[[:alnum:]_-]+:.*$"))
      (if (not makefile)
          (message (format "There is no makefile in %s" project-dir))
        (->> (lines-in-file-matching-re makefile target-regexp)
             (seq-remove #'string-empty-p)
             (seq-map (lambda (target)
                        (replace-regexp-in-string ":.*$" "" target)))
             (completing-read-multiple "run make command:")
             (s-join " ")
             (format "make %s")
             compile))))

  (defun project-override (dir)
    (let ((root (locate-dominating-file default-directory ".project.el"))
          (backend  (ignore-errors (vc-responsible-backend dir))))
      (when root (if (version<= emacs-version "28")
                     (cons 'vc root)
                   (list 'vc backend root)))))

  ;; get current file name {path}/name{.extension}
  ;; get all file paths in project
  ;; look for a file with "test" in the path and the name in the filename
  ;; if there is only 1, then jump to it
  ;; if there are 2 then provide a choice

  (defun project-jump-to-test ()
    (let* ((target-base (file-name-base buffer-file-name))
           (target-extension (file-name-extension buffer-file-name))
           (all-files (project--files-in-directory (project-root (project-current)) nil))
           (matching-files (seq-filter (lambda (file)
                                         (let ((base (file-name-base file))
                                               (extension (file-name-extension file)))
                                           (and (s-contains-p target-base base)
                                                (s-contains-p "test" base)
                                                (s-equals-p target-extension extension)
                                                (s-contains-p "test" (file-name-directory file)))))
                                       all-files)))
      (cond
       ((= 1 (length matching-files)) (find-file (nth 0 matching-files)))
       (t (find-file (completing-read "Pick test: " matching-files))))))

  (defun project-jump-to-src ()
    (defun project-jump-to-test ()
      (let* ((target-base (file-name-base buffer-file-name))
             (target-extension (file-name-extension buffer-file-name))
             (all-files (project--files-in-directory (project-root (project-current)) nil))
             (matching-files (seq-filter (lambda (file)
                                           (let ((base (file-name-base file))
                                                 (extension (file-name-extension file)))
                                             (and (s-contains-p target-base base)
                                                  (s-contains-p "test" base)
                                                  (s-equals-p target-extension extension)
                                                  (s-contains-p "test" (file-name-directory file)))))
                                         all-files)))
        (cond
         ((= 1 (length matching-files)) (find-file (nth 0 matching-files)))
         (t (find-file (completing-read "Pick test: " matching-files)))))))

  (defun project-find-test-pair (file-path)
    (interactive)
    (let ((file-path (or file-path buffer-file-name))))
    (cond
     ((not (s-contains-p "test" (file-name-base file-path))) (project-jump-to-test))))

  (defun project-find-file-no-ignores (project)
    (interactive (list nil))
    (let ((project (or project (project-current))))
      (find-file-no-ignores (project-root project))))

  (defun find-file-no-ignores (&optional dir)
    (interactive (list nil))
    (find-file (funcall project-read-file-name-function
                        "Find file" (project--files-in-directory default-directory nil) nil 'file-name-history "default")))

  (defun project-run-command+ (dir)
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-directory-override nil)
          (map (make-composed-keymap evil-leader-state-map (current-global-map))))
      (message "waiting for command to run in project: %s" dir)
      (call-interactively (funcall embark-prompter map #'indentity))))

  (defun project-current-run-command+ ()
    (interactive)
    (project-run-command+ (project-root (project-current))))
  )

(use-package minibuffer
  :ensure nil
  :demand t
  :after (evil-leader)
  :bind (:map minibuffer-mode-map
              ("C-c $" . select-shell-history)
              ("C-c s" . select-shell-history)
              ("C-c SPC" . minibuffer-clear+)
              :map evil-leader-state-map-extension
              ("i $" . insert-shell-history))
  :config
  (defun insert-shell-history ()
    (interactive)
    (let* ((zsh-hist     (shell-command-to-string "cat ~/.zsh_history  2> /dev/null"))
           (history (->> zsh-hist
	  		 (s-split "\n")
                         (seq-remove #'string-empty-p)
	  		 seq-uniq
	  		 seq-reverse))
	   (vertico-sort-override-function #'identity))
      (if (> (length history) 0)
	  (progn
	    (let ((chosen-history (completing-read "shell history: " history)))
	      (insert chosen-history))))))

  (defun select-shell-history ()
    (interactive)
    (minibuffer-clear+)
    (insert-shell-history))

  (defun minibuffer-clear+ ()
    (interactive)
    (let ((start (progn (goto-char (field-beginning))
                        (point)))
          (end    (progn (end-of-buffer)
                         (move-end-of-line 1)
                         (point))))
      (delete-region start end))))


(use-package dabbrev
  :ensure nil
  :config
  (global-set-key (kbd "M-/") nil))

(use-package vertico
  :after (evil-leader)
  :ensure t
  :demand t
  :bind (("C-M-x" . vertico-repeat)
	 :map vertico-map
	 ("s-t" . vertico-quick-insert)
	 ("C-p" . vertico-C-p-or-reverse)
	 ("C-M-n" . vertico-next-group)
	 ("C-M-p" . vertico-previous-group)
	 ("M-n" . vertico-next+)
	 ("M-p" . vertico-previous+)
	 ("C-M-p" . vertico-previous-group)
	 ("C-M-n" . vertico-next-group)
	 ("C-<return>" . vertico-exit-input)
	 ("S-RET" . vertico-quick-exit)
	 ("S-<return>" . vertico-quick-exit)
	 ("s-<return>" . vertico-quick-exit)
	 ("C-^" . vertico-directory-up)
         ("M-h" . vertico-directory-up)
         ("C-c +" . vertico-show-more)
         ("C-c -" . vertico-show-less)
         ("C-c s a" . vertico-alphabetic-sort+)
         ("C-c s u" . vertico-default-sort+)
         ("C-c s h" . vertico-history-sort+)
         ("C-c s l" . vertico-length-sort+)
         ("C-c s r" . vertico-reverse-alpha-sort+)
         :map evil-leader-state-map-extension
         ("X" . vertico-repeat)
         ("M-x" . vertico-repeat))
  :config
  (setq vertico-default-count 10)
  (setq vertico-count vertico-default-count)
  (setq vertico-resize t)
  (setq vertico-cycle nil)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  (defun vertico-settings ()
    (setq vertico-count vertico-default-count))

  (add-hook 'minibuffer-exit-hook #'vertico-settings)

  (defun vertico-show-more ()
    (interactive)
    (if vertico-unobtrusive-mode
        (vertico-multiform-reverse)
      (setq vertico-count (+ vertico-count 10))
      (vertico--exhibit)))

  (defun vertico-show-less ()
    (interactive)
    (if (= vertico-count 10)
        (vertico-multiform-unobtrusive)
      (setq vertico-count (- vertico-count 10))
      (setq vertico-resize t)
      (vertico--exhibit)
      (setq vertico-resize nil)))

  (setq vertico-multiline '(#("⤶" 0 1 (face vertico-multiline)) . #("…" 0 1 (face vertico-multiline))))

  (vertico-mode)
  (set-face-attribute 'vertico-group-title nil :foreground "blue")
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; (defun vertico--exhibit ()
  ;;   "Exhibit completion UI."
  ;;   (let* ((buffer-undo-list t) ;; Overlays affect point position and undo list!
  ;;          (pt (max 0 (- (point) (minibuffer-prompt-end))))
  ;;          (content (minibuffer-contents-no-properties)))
  ;;     (unless (or (input-pending-p) (equal vertico--input (cons content pt)))
  ;;       (vertico--update-candidates pt content))
  ;;     (vertico--prompt-selection)
  ;;     (vertico--display-count)
  ;;     (vertico--display-candidates (vertico--arrange-candidates))
  ;;     (when (and (bound-and-true-p last-vertico--index)
  ;;                (> last-vertico--index 0))
  ;;       (dotimes (1- last-vertico--index)
  ;;         (message "...")
  ;;         (vertico-next 1))
  ;;       (setq last-vertico--index -1)
  ;;       (vertico--update-scroll)
  ;;       (vertico--exhibit))))

  (defun vertico-next+ ()
    (interactive)
    (cond
     (vertico-unobtrusive-mode (vertico-previous))
     ;; (vertico-reverse-mode     (vertico-next))
     (t                        (vertico-next))))

  (defun vertico-previous+ ()
    (interactive)
    (cond
     (vertico-unobtrusive-mode (vertico-next))
     ;; (vertico-reverse-mode     (vertico-previous))
     (t                        (vertico-previous))))

  (defun vertico-C-p-or-reverse ()
    (interactive)
    (cond
     (vertico-unobtrusive-mode (call-interactively #'vertico-multiform-reverse))
     (vertico-reverse-mode     (call-interactively #'vertico-next))
     (t                        (call-interactively #'vertico-previous))))

  (defun vertico-alphabetic-sort+ ()
    (interactive)
    (setq-local vertico-sort-override-function #'vertico-sort-alpha)
    (consult--vertico-refresh))

  (defun vertico-history-sort+ ()
    (interactive)
    (setq-local vertico-sort-override-function #'vertico-sort-history-alpha)
    (consult--vertico-refresh))

  (defun vertico-length-sort+ ()
    (interactive)
    (setq-local vertico-sort-override-function #'vertico-sort-length-alpha)
    (consult--vertico-refresh))

  (defun vertico-default-sort+ ()
    (interactive)
    (setq-local vertico-sort-override-function nil)
    (consult--vertico-refresh))

  ;;(vertico--define-sort (reversed-alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string> string>)

  (defun vertico-sort-reversed-alpha (candidates)
    (sort candidates #'string>))

  ;; if this is so easy... why do they provide the vertico--define-sort function?
  (defun vertico-sort-alpha (candidates)
    (sort candidates #'string<))

  (defun vertico-reverse-alpha-sort+ ()
    (interactive)
    (setq-local vertico-sort-override-function #'vertico-sort-reversed-alpha)
    (consult--vertico-refresh)))

;; (use-package vertico-reverse
;;   :config
;;   (vertico-reverse-mode))

(use-package vertico-reverse
  :bind (:map vertico-reverse-map
	 ("M-p" . vertico-next+)
	 ("M-n" . vertico-previous+)
	 ("C-M-n" . vertico-previous-group)
	 ("C-M-p" . vertico-next-group)))

(use-package vertico-multiform
  :ensure nil
  :demand t
  :bind (:map vertico-map
              ("C-l" . my/vertico-multiform-unobtrusive))
  :config

  (defun my/vertico-multiform-unobtrusive ()
    "Toggle between vertico-unobtrusive and vertico-reverse."
    (interactive)
    (vertico-multiform-vertical 'vertico-reverse-mode))

  (setq vertico-multiform-categories '((imenu reverse)
                                       (buffer flat)
                                       (consult-grep buffer)
                                       (t reverse)))

  (setq vertico-multiform-commands '((consult-imenu buffer)
                                     (notes-find-file reverse)
                                     (embark-prefix-help-command reverse)
                                     (markdown-open-some-buffer-link+ reverse)
                                     (insert-shell-history reverse)
                                     (consult-line buffer)
                                     (consult-buffer-terminal reverse)
                                     (execute-extended-command reverse)
                                     (consult-buffer-ein reverse)
                                     (consult-buffer-compilation reverse)
                                     (select-from-history reverse (vertico-resize . t))
                                     (select-shell-history reverse (vertico-resize . t))))

  (vertico-multiform-mode 1))


(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :commands (corfu-move-to-minibuffer)
  :demand t
  :bind (("C-," . completion-at-point)
         :map corfu-map
         ("M-h" . corfu-reset)
         ("C-c c" . corfu-move-to-minibuffer))
  :custom
  (corfu-quit-at-boundary nil)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (;;(completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)
  (global-corfu-mode 1)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer))

(use-package cape
  :ensure t
  :demand t
  :bind (("C-," . nil)
         ("C-, C-," . completion-at-point)
         ("C-, d" . cape-dabbr)
         ;; ("C-, h" . cape-history)
         ("C-, f" . cape-file)
         ("C-, s" . cape-elisp-symbol)
         ("C-, l" . cape-line))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (setq completion-at-point-functions
        (list #'cape-dabbrev)))

(use-package elisp-mode
  :after (cape)
  :demand t
  :ensure nil
  :hook (emacs-lisp-mode-hook . (lambda ()
                                  (add-to-list 'completion-at-point-functions
                                               #'cape-dabbrev))))

(use-package consult
  :after (evil-leader)
  :ensure t
  :demand t
  :commands (consult-grep-dir
   	     consult-buffer-terminal
             consult-buffer-ein
             consult-buffer-compilation
             consult-grep-dir-case-sensitive)
  :bind (("C-M-y" . consult-yank-from-kill-ring)
         ("s-b" . consult-buffer)
         ("s-s" . consult-line)
         ("s-i" . consult-imenu)
         ("s-y" . consult-yank-from-kill-ring)
         :map evil-leader-state-map-extension
   	 ("b b"   . consult-buffer)
   	 ("b B"   . switch-to-buffer)
   	 ("b t"   . consult-buffer-terminal)
   	 ("b c"   . consult-buffer-compilation)
   	 ("b C-B"   . ibuffer)
   	 ("b L"   . consult-outline)
   	 ("b l"   . consult-line)
   	 ("d g"   . consult-grep+)
   	 ("d p"   . consult-git-grep+)
	 ("b i"   . consult-imenu)
	 ("b o"   . occur))
  :config
  (setq consult-preview-key (list "M-<return>" "M-n" "M-p"))
  (setq consult-async-min-input 0)
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  (defun consult-grep+ (query &optional case-sensitive)
    (interactive (list "" prefix-arg))
    (if case-sensitive
        (let ((consult-grep-args "grep --null --line-buffered --color=never --line-number -I -r ."))
          (consult-grep default-directory))
      (consult-grep default-directory)))

  (defun consult-git-grep+ (query &optional case-sensitive)
    (interactive (list "" prefix-arg))
    (if case-sensitive
        (let ((consult-git-grep-args "git --no-pager grep --null --color=never --extended-regexp --line-number -I"))
          (consult-git-grep default-directory))
      (consult-git-grep default-directory)))

  ;; add initial option
  (defun consult-buffer (&optional sources initial)
    (interactive)
    (let ((selected (consult--multi (or sources consult-buffer-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Switch to: "
                                    :initial initial
                                    :history 'consult--buffer-history
                                    :sort nil)))
      ;; For non-matching candidates, fall back to buffer creation.
      (unless (plist-get (cdr selected) :match)
        (consult--buffer-action (car selected)))))

  (defun consult-buffer-terminal (&optional arg)
    (interactive "P")
    (let ((consult-preview-key (if arg 'any consult-preview-key)))
      (funcall-interactively #'consult-buffer nil "*vterm* ")))

  (defun consult-buffer-compilation (&optional arg)
    (interactive "P")
    (let ((consult-preview-key (if arg 'any consult-preview-key)))
      (funcall-interactively #'consult-buffer nil "*compilation* ")))

  (evil-add-command-properties 'consult-line :jump t)
  (evil-add-command-properties 'consult-imenu :jump t)

  (defun project--file-preview (dir)
    "Buffer preview function."
    (let ((orig-buf (current-buffer))
          (orig-prev (copy-sequence (window-prev-buffers)))
          (orig-next (copy-sequence (window-next-buffers)))
          files-to-close)
      (lambda (action cand)
        (let ((path (expand-file-name (concat dir cand))))
          (pcase action
            ('exit
             (seq-map #'kill-buffer (seq-uniq files-to-close))
             (set-window-prev-buffers (selected-window) orig-prev)
             (set-window-next-buffers (selected-window) orig-next)
             (switch-to-buffer orig-buf))
            ('preview
             (let ((buf-open? (get-file-buffer path))
                   (buf (or (get-file-buffer path) (find-file-noselect path))))
               (with-selected-window (selected-window)
                 (unless (or orig-prev orig-next)
                   (setq orig-prev (copy-sequence (window-prev-buffers))
                         orig-next (copy-sequence (window-next-buffers))))
                 (switch-to-buffer buf 'norecord)
                 (when (not buf-open?)
                   (setq files-to-close (append (list buf) files-to-close)))))))))))

  (defun read-file-name-default (prompt &optional dir default-filename mustmatch initial predicate)
    "Default method for reading file names.
See `read-file-name' for the meaning of the arguments."
    (unless dir (setq dir (or default-directory "~/")))
    (unless (file-name-absolute-p dir) (setq dir (expand-file-name dir)))
    (unless default-filename
      (setq default-filename
            (cond
             ((null initial) buffer-file-name)
             ;; Special-case "" because (expand-file-name "" "/tmp/") returns
             ;; "/tmp" rather than "/tmp/" (bug#39057).
             ((equal "" initial) dir)
             (t (expand-file-name initial dir)))))
    ;; If dir starts with user's homedir, change that to ~.
    (setq dir (abbreviate-file-name dir))
    ;; Likewise for default-filename.
    (if default-filename
        (setq default-filename
	      (if (consp default-filename)
		  (mapcar 'abbreviate-file-name default-filename)
	        (abbreviate-file-name default-filename))))
    (let ((insdef (cond
                   ((and insert-default-directory (stringp dir))
                    (if initial
                        (cons (minibuffer-maybe-quote-filename (concat dir initial))
                              (length (minibuffer-maybe-quote-filename dir)))
                      (minibuffer-maybe-quote-filename dir)))
                   (initial (cons (minibuffer-maybe-quote-filename initial) 0)))))

      (let ((ignore-case read-file-name-completion-ignore-case)
            (minibuffer-completing-file-name t)
            (pred (or predicate 'file-exists-p))
            (add-to-history nil))

        (let* ((val
                (if (or (not (next-read-file-uses-dialog-p))
                        ;; Graphical file dialogs can't handle remote
                        ;; files (Bug#99).
                        (file-remote-p dir))
                    ;; We used to pass `dir' to `read-file-name-internal' by
                    ;; abusing the `predicate' argument.  It's better to
                    ;; just use `default-directory', but in order to avoid
                    ;; changing `default-directory' in the current buffer,
                    ;; we don't let-bind it.
                    (let ((dir (file-name-as-directory
                                (expand-file-name dir))))
                      (minibuffer-with-setup-hook
                          (lambda ()
                            (setq default-directory dir)
                            ;; When the first default in `minibuffer-default'
                            ;; duplicates initial input `insdef',
                            ;; reset `minibuffer-default' to nil.
                            (when (equal (or (car-safe insdef) insdef)
                                         (or (car-safe minibuffer-default)
                                             minibuffer-default))
                              (setq minibuffer-default
                                    (cdr-safe minibuffer-default)))
                            (setq-local completion-ignore-case ignore-case)
                            ;; On the first request on `M-n' fill
                            ;; `minibuffer-default' with a list of defaults
                            ;; relevant for file-name reading.
                            (setq-local minibuffer-default-add-function
                                        (lambda ()
                                          (with-current-buffer
                                              (window-buffer (minibuffer-selected-window))
				            (read-file-name--defaults dir initial))))
			    (set-syntax-table minibuffer-local-filename-syntax))
                        (consult--read 'read-file-name-internal
                                       :prompt prompt
                                       :predicate pred
                                       :state (project--file-preview "")
                                       :require-match mustmatch
                                       :initial insdef
                                       :history 'file-name-history
                                       :default default-filename)))
                  ;; If DEFAULT-FILENAME not supplied and DIR contains
                  ;; a file name, split it.
                  (let ((file (file-name-nondirectory dir))
                        ;; When using a dialog, revert to nil and non-nil
                        ;; interpretation of mustmatch. confirm options
                        ;; need to be interpreted as nil, otherwise
                        ;; it is impossible to create new files using
                        ;; dialogs with the default settings.
                        (dialog-mustmatch
                         (not (memq mustmatch
                                    '(nil confirm confirm-after-completion)))))
                    (when (and (not default-filename)
                               (not (zerop (length file))))
                      (setq default-filename file)
                      (setq dir (file-name-directory dir)))
                    (when default-filename
                      (setq default-filename
                            (expand-file-name (if (consp default-filename)
                                                  (car default-filename)
                                                default-filename)
                                              dir)))
                    (setq add-to-history t)
                    (x-file-dialog prompt dir default-filename
                                   dialog-mustmatch
                                   (eq predicate 'file-directory-p)))))

               (replace-in-history (eq (car-safe file-name-history) val)))
          ;; If completing-read returned the inserted default string itself
          ;; (rather than a new string with the same contents),
          ;; it has to mean that the user typed RET with the minibuffer empty.
          ;; In that case, we really want to return ""
          ;; so that commands such as set-visited-file-name can distinguish.
          (when (consp default-filename)
            (setq default-filename (car default-filename)))
          (when (eq val default-filename)
            ;; In this case, completing-read has not added an element
            ;; to the history.  Maybe we should.
            (if (not replace-in-history)
                (setq add-to-history t))
            (setq val ""))
          (unless val (error "No file name specified"))

          (if (and default-filename
                   (string-equal val (if (consp insdef) (car insdef) insdef)))
              (setq val default-filename))
          (setq val (substitute-in-file-name val))

          (if replace-in-history
              ;; Replace what Fcompleting_read added to the history
              ;; with what we will actually return.  As an exception,
              ;; if that's the same as the second item in
              ;; file-name-history, it's really a repeat (Bug#4657).
              (let ((val1 (minibuffer-maybe-quote-filename val)))
                (if history-delete-duplicates
                    (setcdr file-name-history
                            (delete val1 (cdr file-name-history))))
                (if (string= val1 (cadr file-name-history))
                    (pop file-name-history)
                  (setcar file-name-history val1)))
            (when add-to-history
              (add-to-history 'file-name-history
                              (minibuffer-maybe-quote-filename val))))
	  val))))
  (setq completion-in-region-function #'completion--in-region)
  )

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult))

(use-package consult-dir
  :ensure t
  :demand t
  :after (consult evil-leader)
  :bind (:map minibuffer-mode-map
              ("C-c d" . consult-dir)
              :map evil-leader-state-map-extension
              ("M-d" . consult-dir)))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; TODO: remove confirmation from kill-buffer
(use-package embark
  :after (vertico)
  :ensure t
  :demand t
  :commands (embark-act-quit forward-button-click+ backward-button-click+ embark-collect-delete+)
  :bind (("C-z" . embark-act)
   	 :map minibuffer-mode-map
   	 ("TAB" . minibuffer-force-complete)
   	 ("SPC" . nil)
   	 ("C-z" . embark-act)
   	 ("M-z" . embark-select-next-line)
   	 ("C-c a" . embark-select-all-vertico)
   	 ("C-c u" . embark-unselect-all-vertico)
   	 ("C-c t" . embark-select-toggle-vertico)
   	 ("C-M-z" . embark-act-all)
   	 ("C-c b" . embark-become+)
   	 ("C-c x" . embark-export)
   	 ("C-c c" . embark-collect)
   	 :map embark-meta-map
   	 ("C-h" . nil)
   	 ("C-z" . embark-act)
	 :map embark-collect-mode-map
	 ("m" . embark-select-forward-button)
	 ("d" . embark-collect-delete+)
	 ("D" . embark-collect-delete-no-select+)
	 ("M-n" . forward-button-click+)
	 ("M-p" . backward-button-click+))
  :config

  (setq embark-confirm-act-all nil)

  (defun embark-select-forward-button ()
    "Select the candidate and move to the next candidate."
    (interactive)
    (embark-select)
    (forward-button 1))

  (defun embark-select-next-line ()
    "Select the candidate and move to the next candidate."
    (interactive)
    (embark-select)
    (vertico-next))

  (defun embark-select-toggle-vertico ()
    "Select all of the filtered candidates"
    (interactive)
    (let ((next-command (if (equal major-mode 'vertico)
                            #'embark-select-next-line
                          #'embark-select-forward-button))
          (last-index -1))
      (vertico-first)
      (when (= last-index -1)
        (vertico-next))
      (while (> vertico--index last-index)
        (setq last-index vertico--index)
        (embark-select-next-line))
      (vertico-first)))

  (defun embark-select-all-vertico ()
    "Select all of the filtered candidates"
    (interactive)
    (let ((next-command (if (equal major-mode 'vertico)
                            #'embark-select-next-line
                          #'embark-select-forward-button))
          (last-index -1)
          (last-count (length embark--selection)))
      (vertico-first)
      (when (= vertico--index -1)
        (vertico-next))
      (while (> vertico--index last-index)
        (setq last-index vertico--index)
        (setq last-count (length embark--selection))
        (embark-select)
        (when (<= (length embark--selection) last-count)
          (embark-select))
        (vertico-next))
      (vertico-first)))

  (defun embark-unselect-all-vertico ()
    "Select all of the filtered candidates"
    (interactive)
    (let ((next-command (if (equal major-mode 'vertico)
                            #'embark-select-next-line
                          #'embark-select-forward-button))
          (last-index -1)
          (last-count (length embark--selection)))
      (vertico-first)
      (when (= vertico--index -1)
        (vertico-next))
      (while (> vertico--index last-index)
        (setq last-index vertico--index)
        (setq last-count (length embark--selection))
        (embark-select)
        (when (> (length embark--selection) last-count)
          (embark-select))
        (vertico-next))
      (vertico-first)))

  (defun embark-become+ ()
    (interactive)
    (evil-normal-state)
    (embark-become))


  (defun forward-button-click+ ()
    (interactive)
    (funcall-interactively #'forward-button 1)
    (funcall-interactively #'push-button))

  (defun backward-button-click+ ()
    (interactive)
    (funcall-interactively #'backward-button 1)
    (funcall-interactively #'push-button))

  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
   	 (interactive)
   	 (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  (defun embark-act-quit ()
    (interactive)
    (let ((embark-quit-after-action t))
      (funcall-interactively #'embark-act)))

  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action nil)
  (setq embark-indicators '(embark--vertico-indicator embark-mixed-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

  (defun embark-collect-delete+ ()
    (interactive)
    (embark-collect-delete-no-select+)
    (call-interactively #'push-button))

  (defun embark-collect-delete-no-select+ ()
    (interactive)
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char 1)
      (when (equal (line-number-at-pos (point-max))
                   (line-number-at-pos (point)))
        (previous-line))))

  (cl-defun embark--confirm (&key action target &allow-other-keys)
    "Ask for confirmation before running the ACTION on the TARGET."
    nil)

  (defun record-vertico-details ()
    (interactive)
    (setq last-vertico--index vertico--index)
    (setq was-vertico-multiform-unobtrusive? vertico-unobtrusive-mode)
    (setq was-vertico-multiform-reverse? vertico-reverse-mode))

  (add-hook 'minibuffer-setup-hook #'reset-vertico-details)

  ;; how can i get this to reset to the correct candidate index?
  (defun reset-vertico-details ()
    (interactive)
    (when (bound-and-true-p was-vertico-multiform-unobtrusive?)
      (call-interactively #'vertico-multiform-unobtrusive)
      (setq was-vertico-multiform-unobtrusive? nil))
    (when (bound-and-true-p was-vertico-multiform-reverse?)
      (call-interactively #'vertico-multiform-reverse)
      (setq was-vertico-multiform-reverse? nil)))

  (defun embark--restart (&rest _)
    "Restart current command with current input.
Use this to refresh the list of candidates for commands that do
not handle that themselves."
    (when (minibufferp)
      (record-vertico-details)
      (embark--become-command embark--command (minibuffer-contents))))

  (set-face-attribute 'embark-target nil :bold t)

  (defun embark--collect (buffer-name)
    "Create an Embark Collect buffer named BUFFER-NAME.

The function `generate-new-buffer-name' is used to ensure the
buffer has a unique name."
    (let ((buffer (generate-new-buffer buffer-name))
          (rerun (embark--rerun-function #'embark-collect)))
      (with-current-buffer buffer
        ;; we'll run the mode hooks once the buffer is displayed, so
        ;; the hooks can make use of the window
        (delay-mode-hooks (embark-collect-mode)))

      (embark--cache-info buffer)
      (unless (embark-collect--update-candidates buffer)
        (user-error "No candidates to collect"))

      (with-current-buffer buffer
        (setq tabulated-list-use-header-line nil ; default to no header
              header-line-format nil
              tabulated-list--header-string nil)
        (setq embark--rerun-function rerun))

      (let ((window (display-buffer buffer)))
        (with-selected-window window
          (run-mode-hooks)
          (tabulated-list-revert))
        buffer))))

(use-package embark-maps ;; my package!
  :after (embark evil-leader)
  :demand t
  :load-path my-package-dir
  :config
  (define-key embark-general-map (kbd "SPC") evil-leader-state-map-extension))

(use-package orderless
  :ensure t
  :demand t
  :commands (orderless-help+)
  :bind (:map minibuffer-mode-map
              ("C-c ?" . orderless-help+))
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq orderless-component-separator 'orderless-escapable-split-on-space)

  (defun orderless-flex-if-twiddle-dispatcher+ (pattern _index _total)
    "match characters in order but not necessarily consecutively"
    (cond
     ((equal "~" pattern)
      '(orderless-literal . "~"))
     ((string-prefix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 1)))))

  (defun orderless-literal-dispatcher+ (pattern _index _total)
    "match literally"
    (cond
     ((equal "=" pattern)
      '(orderless-literal . "="))
     ((string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1)))))

  (defun orderless-initialism-dispatcher+ (pattern _index _total)
    "match characters as initials of words"
    (cond
     ((equal "," pattern)
      '(orderless-literal . ","))
     ((string-prefix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 1)))))

  (defun orderless-without-dispatcher+ (pattern _index _total)
    "remove matches"
    (cond
     ((equal "!" pattern)
      '(orderless-literal . "!"))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-literal orderless-regexp)
   	orderless-style-dispatchers '(orderless-flex-if-twiddle-dispatcher+
   				      orderless-initialism-dispatcher+
   				      orderless-literal-dispatcher+
   				      orderless-without-dispatcher+))

  (defvar my-orderless-prefix-faces
    '((?= . error)
      (?, . error)
      (?! . error)
      (?~ . error)))

  (defun my-highlight-prefix ()
    (save-excursion
      (goto-char (point-min))
      (dolist (prefix-face my-orderless-prefix-faces)
        (let* ((prefix (car prefix-face))
               (face (cdr prefix-face))
               (search (format "\\(^\\|[^\\\\] \\)%s[^ ]" (char-to-string prefix))))
          (while (search-forward-regexp search nil t)
            (let ((prefix-pos (- (match-end 0) 2)))
              (put-text-property prefix-pos (1+ prefix-pos) 'face face)
              (remove-list-of-text-properties (1+ prefix-pos)
                                              (min (1+ (1+ prefix-pos)) (point-max))
                                              '(face)))))))
    (set-buffer-modified-p nil))

  (setq orderless-smart-case t)

  (defun my-setup-highlight-hook ()
    (when (eq (current-local-map) vertico-map)
      (add-hook 'post-command-hook #'my-highlight-prefix nil t)))

  (add-hook 'minibuffer-setup-hook #'my-setup-highlight-hook))

(use-package undo-tree
  :ensure t
  :demand t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :demand t
  :config
  (setq evil-respect-visual-line-mode t)
  (setq evil-echo-state nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-mode-line-format nil)
  (evil-set-undo-system 'undo-tree)
  (setq evil-default-state 'emacs)
  (setq evil-want-minibuffer t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)

  (setq evil-emacs-state-modes nil)
  (setq evil-normal-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-operator-state-modes nil)
  (setq evil-replace-state-modes nil)

  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'org-mode 'normal)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'javascript-mode 'normal)
  (evil-set-initial-state 'python-mode 'normal)
  (evil-set-initial-state 'emacs-lisp-mode 'normal)
  (evil-set-initial-state 'clojure-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'snippet-mode 'normal)
  (evil-set-initial-state 'javascript-mode 'normal)
  (evil-set-initial-state 'sh-mode 'normal)
  (evil-set-initial-state 'js-mode 'normal)
  (evil-set-initial-state 'racket-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'scheme-mode 'normal)
  (evil-set-initial-state 'css-mode 'normal)
  (evil-set-initial-state 'markdown-mode 'normal)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'wdired-mode 'normal)
  (evil-set-initial-state 'nxml-mode 'normal)
  (evil-set-initial-state 'conf-colon-mode 'normal)
  (evil-set-initial-state 'inferior-python-mode 'normal)
  (evil-set-initial-state 'inferior-emacs-lisp-mode 'normal)
  (evil-set-initial-state 'ein-notebook-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'cider-repl-mode 'normal)

  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (setq evil-emacs-state-cursor    '("black" box))
  (setq evil-insert-state-cursor   '("Royal Blue" (bar . 2)))
  (setq evil-normal-state-cursor   '("Royal Blue" box))
  (setq evil-operator-state-cursor '("Royal Blue" (hbar . 2)))
  (evil-mode 1)

  ;;; for some reason this is necessary to not start in emacs-state
  (advice-add 'evil-show-registers
              :after (lambda (&rest r) (evil-change-state evil-default-state))))

(use-package evil-escape
  :ensure t
  :demand t
  :config
  (evil-escape-mode)
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  (setq evil-escape-delay .1))

(use-package evil-baptism
  :after (evil)
  :demand t
  :load-path my-package-dir)

(use-package evil-leader
  :after (evil)
  :demand t
  :load-path my-package-dir
  :config
  (define-key evil-leader-state-map-extension (kbd "i t") 'insert-time-id))

(use-package evil-surround
  :after evil
  :demand t
  :ensure t
  :config
  (defun evil-surround-read-string ()
    (let ((delimiter (if (evil-operator-state-p)
                         (save-restriction (widen) (read-string "delimiter: "))
                       (read-string "delimiter: "))))
      (cons delimiter delimiter)))

  (setq-default evil-surround-pairs-alist
                '((?\) . ("(" . ")"))
                  (?\] . ("[" . "]"))
                  (?\} . ("{" . "}"))

                  (?\( . ("( " . " )"))
                  (?\[ . ("[ " . " ]"))
                  (?\{ . ("{ " . " }"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))
                  (?s . evil-surround-read-string)
                  (?t . evil-surround-read-tag)
                  (?f . evil-surround-function)))

  (evil-define-command evil-sandwich (char)
    (interactive (evil-surround-input-char))
    (call-interactively
     (pcase char
       (?c #'evil-surround-change)
       (?k #'evil-surround-delete))))

  (define-key evil-normal-state-map (kbd "'") 'evil-sandwich)
  (define-key evil-visual-state-map (kbd "'") 'evil-surround-region))

;; (use-package lispyville
;;   :ensure t
;;   :demand t
;;   :bind (("C-k" . lispy-kill))
;;   :hook ((prog-mode-hook   . lispyville-mode)))

;; (use-package elec-pair
;;   :hook ((text-mode-hook   . electric-pair-local-mode)
;;          (prog-mode-hook   . electric-pair-local-mode)
;;          (python-mode-hook . electric-pair-local-mode)
;;          (java-mode-hook   . electric-pair-local-mode)
;;          (scala-mode-hook  . electric-pair-local-mode)
;;          (emacs-lisp-mode  . electric-pair-local-mode)))

(use-package magit
  :ensure t
  :demand t
  :bind (("s-g" . magit-status)
         :map magit-status-mode-map
         ("<" . magit-section-up)
         :map evil-leader-state-map-extension
	 ("g g" . magit-status)
	 ("g b" . magit-blame)
         ("g c" . magit-clone)
         ("g d" . magit-file-dispatch)
         ("f g" . magit-file-dispatch))
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package dash
  :ensure t)

(use-package magit-delta
  :ensure t
  :demand t
  :init
  (shell-command "which delta || brew install git-delta" nil)
  :hook (magit-mode-hook . (lambda () (magit-delta-mode +1))))

(use-package avy
  :ensure t
  :demand t
  :bind (("M-g c" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line))
  :config
  (setq avy-single-candidate-jump nil)
  (setq avy-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setq avy-dispatch-alist
        '((?k . avy-action-kill-whole-line)
          (?y . avy-action-yank-whole-line)
          (?w . avy-action-copy-whole-line)
          (?m . avy-action-teleport-whole-line)))

  )

(use-package dired
  :commands (find-grep-dired-default-dir)
  :hook (dired-mode-hook . dired-hide-details-mode)
  :demand t
  :bind (("s-d" . dired+)
         :map evil-leader-state-map-extension
	 ("d o" . open-in-finder)
	 ("s F" . find-grep-dired)
 	 ("s f" . find-grep-dired-default-dir)
         ("d D" . dired+)
         ("D" . dired+)
  	 :map dired-mode-map
 	 ("C-M-n" . nil)
 	 ("C-M-p" . nil)
 	 ("C-t" . nil)
 	 ("^" . dired-up-directory)
 	 ("<" . dired-goto-first-item)
 	 (">" . end-of-buffer)
 	 ("M-s f C-s" . nil)
  	 ("M-s f ESC" . nil)
 	 ("M-s f" . nil)
 	 ("e" . wdired-change-to-wdired-mode)
  	 ("M-n" . dired-preview-next)
  	 ("M-p" . dired-preview-previous)
	 ("M-<return>" . dired-preview))
  :config
  (setq dired-listing-switches "-Al")

  (defun dired+ ()
    (interactive)
    (let* ((filepath (buffer-file-name))
           (filename (when filepath (concat " " (file-name-base filepath) "\\." (file-name-extension filepath))))
           (prefix "[0-9]\\{2\\}[-:][0-9]\\{2\\}"))
      (dired-default-directory+)
      (goto-char (point-min))
      (dired-next-line 1)
      (when (and filepath (search-forward-regexp (concat prefix filename) nil t))
        (dired-next-line 1)
        (dired-previous-line 1))))

  (defun dired-goto-first-item ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 1))

  (defun find-grep-dired-default-dir ()
    (interactive)
    (let ((search-string (read-string "Search for: ")))
      (find-grep-dired default-directory search-string)
      (rename-buffer (format "find: %s %s" (file-name-nondirectory (directory-file-name default-directory)) search-string) t)))

  (defun dired-preview ()
    (interactive)
    (save-selected-window
      (cond
       ((equal major-mode 'dired-sidebar-mode) (dired-sidebar-find-file))
       ((equal major-mode 'dired-mode)         (dired-find-file-other-window)))))

  (defun dired-preview-previous ()
    (interactive)
    (dired-previous-line 1)
    (dired-preview))

  (defun dired-preview-next ()
    (interactive)
    (dired-next-line 1)
    (dired-preview))

  (defun open-in-finder ()
    "Open the current buffer's directory in Finder."
    (interactive)
    (shell-command (concat "open " (shell-quote-argument (expand-file-name default-directory)))))
  )


(use-package dired-narrow
  :ensure t
  :demand t
  :commands (dired-narrow-archive)
  :bind (:map dired-mode-map
  	      ("/" . dired-narrow)))

(use-package dired-sidebar
  :after (dired)
  :ensure t
  :demand t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (:map evil-leader-state-map-extension
              ("d s" . dired-sidebar-toggle-sidebar)
              ("^" . nil)
              :map dired-sidebar-mode-map
              ("^" . nil))
  :config
  (setq dired-sidebar-width 45)

  )

(use-package dired-subtree
  :after (dired dired-sidebar)
  :ensure t
  :demand t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle)
              ("^" . dired-up-directory)
              :map dired-sidebar-mode-map
              ("^" . dired-subtree-up))
  :config
  (copy-face 'default 'dired-subtree-depth-1-face)
  (copy-face 'default 'dired-subtree-depth-2-face)
  (copy-face 'default 'dired-subtree-depth-3-face)
  (copy-face 'default 'dired-subtree-depth-4-face)
  (copy-face 'default 'dired-subtree-depth-5-face)
  (copy-face 'default 'dired-subtree-depth-6-face)

  (set-face-attribute 'dired-subtree-depth-1-face nil :background "#e495e5f4e1df")
  (set-face-attribute 'dired-subtree-depth-2-face nil :background "#e495e5f4e1df")
  (set-face-attribute 'dired-subtree-depth-3-face nil :background "#e495e5f4e1df")
  (set-face-attribute 'dired-subtree-depth-4-face nil :background "#e495e5f4e1df")
  (set-face-attribute 'dired-subtree-depth-5-face nil :background "#e495e5f4e1df")
  (set-face-attribute 'dired-subtree-depth-6-face nil :background "#e495e5f4e1df"))

(use-package ls-lisp
  :after (dired)
  :config
  (setq dired-use-ls-dired t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t))

(use-package ace-window
  :ensure t
  :after (evil-leader)
  :commands (ace-copy-window ace-move-window ace-switch-buffer-other-window)
  :bind (("s-w" . ace-window)
         :map evil-leader-state-map-extension
	 ("t s" . ace-swap-window)
	 ("t t" . ace-window)
	 ("t k" . ace-delete-window)
	 ("t K" . delete-window)
	 ("t c" . ace-copy-window)
	 ("t m" . ace-move-window)
         ("t ." . aw-flip-window))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red"     ;; You can change the color
                      :height 400           ;; Adjust this value to change the size
                      :weight bold)))))
  (defun ace-copy-window ()
    (interactive)
    (aw-select #'aw-copy-window))

  (defun ace-move-window ()
    (interactive)
    (aw-select #'aw-move-window))

  (defun ace-switch-buffer-other-window ()
    (interactive)
    (aw-select #'aw-switch-buffer-other-window))
  (defun toggle-window-dedicated (&optional window)
    "Toggle WINDOW's dedicated flag.
Also set its `no-delete-other-windows' parameter to match."
    (interactive)
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (set-window-parameter window 'no-delete-other-windows
                          (window-dedicated-p window))
    (message "Dedicated: %s" (window-dedicated-p window)))

  (defun ace-window-dedicate ()
    (interactive)
    (aw-select " Ace - Delete Window"
               #'toggle-window-dedicated))


  (setq aw-keys '(?u ?h ?e ?t))
  (setq aw-dispatch-always t))

(use-package ace-link
  :ensure t
  :demand t
  :config
  (ace-link-setup-default))

(use-package yasnippet
  :ensure t
  :demand t
  :bind (("C-, y" . yas-insert-snippet)
         :map evil-leader-state-map-extension
         ("i y" . yas-insert-snippet))
  :config
  (yas-global-mode 1))

;; (use-package visual-fill-column
;;   :ensure t
;;   (visual-line-mode-hook . visual-fill-column-mode))

(use-package smerge-mode
  :ensure nil
  :after (hydra evil-leader)
  :bind (:map evil-leader-state-map-extension
	      ("g s" . hydra/smerge/body))
  ;; :hook (magit-diff-visit-file . (lambda ()
  ;;       			   (when smerge-mode
  ;;       			     (hydra/smerge/body))))
  :init
  (setq smerge-command-prefix "")
  :config
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (defhydra hydra/smerge
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
	    (interactive)
	    (save-buffer)
	    (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue)))

(use-package winner
  :after (evil-leader)
  :config
  (winner-mode 1)
  (define-key evil-leader-state-map-extension (kbd "t u") 'winner-undo)
  (define-key evil-leader-state-map-extension (kbd "t r") 'winner-redo)
  (define-key winner-mode-map [(control c) left] nil)
  (define-key winner-mode-map [(control c) right] nil))

(use-package tab-bar
  :ensure nil
  :commands (tab-bar-goto-misc+)
  :bind (("C-{" . tab-bar-switch-to-next-tab)
         ("C-}" . tab-bar-switch-to-prev-tab)
         ("C-M-{" . tab-bar-move-tab)
         ("C-M-}" . tab-bar-move-tab-backward)
         ("C-s-l" . tab-bar-switch-to-next-tab)
         ("C-s-h" . tab-bar-switch-to-prev-tab)
         :map evil-leader-state-map-extension
         ("TAB m" . tab-bar-goto-misc+)
         ("TAB 1" . tab-bar-switch-to-tab-1+)
         ("TAB 2" . tab-bar-switch-to-tab-2+)
         ("TAB 3" . tab-bar-switch-to-tab-3+)
         ("TAB 4" . tab-bar-switch-to-tab-4+)
         ("TAB 5" . tab-bar-switch-to-tab-5+)
         ("TAB 6" . tab-bar-switch-to-tab-6+)
         ("TAB 7" . tab-bar-switch-to-tab-7+)
         ("TAB 8" . tab-bar-switch-to-tab-8+)
         ("TAB 9" . tab-bar-switch-to-tab-9+))
  :config
  (setq tab-bar-show t)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-separator "  ")
  (setq tab-bar-new-button nil)
  (setq tab-bar-tab-hints t)
  (set-face-attribute 'tab-bar-tab nil :background "#273532" :foreground "#268bd2" :overline nil :underline nil :bold t)
  (set-face-attribute 'tab-bar-tab-inactive nil :bold t)
  (set-face-attribute 'tab-bar nil :underline '("gray50" . (line . 2)))
  (defun tab-bar-switch-to-tab (name)
    "Switch to the tab by NAME.
Default values are tab names sorted by recency, so you can use \
\\<minibuffer-local-map>\\[next-history-element]
to get the name of the most recently visited tab, the second
most recent, and so on."
    (interactive
     (let* ((recent-tabs (mapcar (lambda (tab)
                                   (alist-get 'name tab))
                                 (funcall tab-bar-tabs-function nil))))
       (list (completing-read (format-prompt "Switch to tab by name"
                                             (car recent-tabs))
                              recent-tabs nil nil nil nil recent-tabs))))
    (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0))))


  (defun tab-bar-goto-misc+ ()
    (interactive)
    (tab-bar-switch-to-tab "misc."))

  (defun tab-bar-switch-to-tab-1+  () (interactive) (tab-bar-select-tab 0))
  (defun tab-bar-switch-to-tab-2+  () (interactive) (tab-bar-select-tab 2))
  (defun tab-bar-switch-to-tab-3+  () (interactive) (tab-bar-select-tab 3))
  (defun tab-bar-switch-to-tab-4+  () (interactive) (tab-bar-select-tab 4))
  (defun tab-bar-switch-to-tab-5+  () (interactive) (tab-bar-select-tab 5))
  (defun tab-bar-switch-to-tab-6+  () (interactive) (tab-bar-select-tab 6))
  (defun tab-bar-switch-to-tab-7+  () (interactive) (tab-bar-select-tab 7))
  (defun tab-bar-switch-to-tab-8+  () (interactive) (tab-bar-select-tab 8))
  (defun tab-bar-switch-to-tab-9+  () (interactive) (tab-bar-select-tab 9))

  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defun tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 10))
                       (alist-get i circle-numbers-alist) "")))
      (propertize
       (concat tab-num
               " "
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))
  (defvar circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")
  (setq tab-bar-tab-name-format-function #'tab-bar-tab-name-format-default)
  (tab-bar-rename-tab "misc.")
  (tab-bar-mode -1))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
	      :map wgrep-mode-map
	      ("C-c C-e" . nil)
	      ("C-x C-s" . nil)
	      ("C-c C-c" . wgrep-finish-edit)))

(use-package python
  :ensure nil
  :commands (ipython)
  :bind (:map evil-leader-state-map-extension
              ("o p" . run-python)
              ("o P" . ipython))
  :config
  (evil-define-key '(normal) python-mode-map
    (kbd "<tab>") 'python-indent-shift-right)
  (evil-define-key '(normal) python-mode-map
    (kbd "<S-tab>") 'python-indent-shift-left)

  (defun ipython ()
    (interactive)
    (let ((python-shell-interpreter "python")
          (python-shell-interpreter-args "-m IPython --simple-prompt"))
      (call-interactively #'run-python))))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package f
  :ensure t
  :demand t)

(use-package org
  :ensure nil
  :demand t
  :commands (open-log-file+ org-insert-timestamp+ org-open-some-buffer-link+)
  :hook ((org-mode-hook . visual-line-mode)
         (org-mode-hook . auto-revert-mode)
         (org-mode-hook . (lambda ()
                            (setq paragraph-start "[ \t]*$")
                            (setq paragraph-separate "[ \t]*$")))
         (org-mode-hook . (lambda ()
                            (setq-local help-at-pt-display-when-idle t)
                            (setq-local help-at-pt-timer-delay .3)
                            (help-at-pt-set-timer)))
         (org-mode-hook . org-show-all))
  :bind (("C-, c" . insert-archive-id+)
         :map org-mode-map
	 ("C-c o"  . org-open-at-point)
	 ("M-n"  . org-next-visible-heading)
	 ("M-p"  . org-previous-visible-heading)
	 ("M-n"  . nil)
	 ("M-p"  . nil)
	 ("C-c ."  . org-insert-timestamp+)
	 ("C-c c"  . org-cycle)
	 ("C-c l"  . org-open-some-buffer-link+)
	 ("C-c r"  . org-refile)
	 ("C-c t"  . org-todo)
	 ("C-c s t"  . org-show-tree)
	 ("C-c i g"  . org-set-tags-command)
	 ("C-c i t"  . org-insert-todo-heading)
	 ("C-c i h"  . org-insert-heading)
	 ("TAB"  . org-cycle)
	 ("S-TAB"  . org-shifttab)
         ("C-c RET" . org-meta-return)
         ("C-c S-RET" . org-insert-todo-heading)
         :map evil-leader-state-map-extension
         ("i c" . insert-archive-id+)
         ("i i" . org-insert-time-id+)
         ("i t" . org-insert-time+)
         ("s l" . org-jump+)
         ("c +" . org-increase-number-at-point)
         ("c -" . org-decrease-number-at-point)
         ("o l" . open-log-file+)
         ("n p" . project-dir-notes)
         ("n t" . org-open-todos )
         ("n f" . find-file-notebox)
         ("n d" . open-log-file+))
  :config
  (defun org-open-todos () (interactive) (find-file org-todo-file))
  (setq org-startup-folded 'showeverything)
  (setq org-property-format  "%-12s %s")
  (setq org-image-actual-width nil)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?Z)
  (setq org-hide-leading-stars nil)
  (setq org-log-into-drawer t)
  (setq org-fontify-whole-heading-line t)

  (defun org-set-notebox+ (path)
    (setq notebox (expand-file-name path))
    (setq org-directory notebox))

  (org-set-notebox+ "~/notes")

  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (python . t)))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  (set-face-attribute 'org-level-1 nil :bold t)
  (set-face-attribute 'org-level-2 nil :bold t)
  (set-face-attribute 'org-level-3 nil :bold t)
  (set-face-attribute 'org-level-4 nil :bold t)
  (set-face-attribute 'org-level-5 nil :bold t)
  (set-face-attribute 'org-level-6 nil :bold t)
  (set-face-attribute 'org-level-7 nil :bold t)

  (setq org-log-file (concat notebox "daily-log.org" ))
  (evil-add-command-properties 'org-open-at-point :jump t)

  (defun project-dir-notes ()
    (interactive)
    (project-switch-project notebox))

  (defun open-log-file+ ()
    (interactive)
    (find-file org-log-file))

  (defun org-insert-timestamp+ ()
    (interactive)
    (org-time-stamp '(16) t))

  (defun org-open-some-buffer-link+ ()
    (interactive)
    (let* ((links (org-element-map (org-element-parse-buffer) 'link
                    (lambda (link)
                      (let ((type (org-element-property :type link))
                            (path (org-element-property :path link)))
                        (when (member type '("http" "https"))
                          (format "%s:%s" type path)))))))

      (browse-url (completing-read "links: " links))))

  (defun find-file-notebox ()
    (interactive)
    (let ((default-directory notebox))
      (call-interactively #'project-find-file)))

  (defun org-jump+ ()
    (interactive)
    (let ((outline (org-collect-outline+))
          (vertico-sort-override-function #'identity))
      (goto-char (cdr (assoc (completing-read "jump to:" outline) outline))))
    (recenter-top-bottom 0)
    (org-show-entry))

  (defun org-collect-outline+ ()
    (interactive)
    (save-excursion
      (let ((ast (cddr (org-element-parse-buffer 'headline))))
        (org-get-headline-details+ ast))))

  (defun format-headline+ (headline parents)
    (concat (when parents (concat parents "/"))
            (propertize (--> (plist-get (cadr headline) :title)
                             (replace-regexp-in-string "\\[\\[.*?\\]\\[" "" it t t)
                             (replace-regexp-in-string "\\]\\]" "" it t t))
                        'face
                        (list :foreground (face-attribute (nth (plist-get (cadr headline) :level) org-level-faces) :foreground)))))

  (defun org-get-headline-details+ (ast &optional parents)
    (cond
     ((null ast) '())
     (t (let* ((current-headline (car ast))
               (formatted-headline (format-headline+ current-headline parents)))
          (append (list (cons formatted-headline
                              (plist-get (cadr current-headline)
                                         :begin)))
                  (org-get-headline-details+ (cddr current-headline)
                                             formatted-headline)
                  (org-get-headline-details+ (cdr ast) parents))))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-tags-exclude-from-inheritance (list "project"))
  (setq org-capture-templates
        '(("i" "inbox" item (file org-log-file)
           "")
          ("t" "todo" entry (file org-todo-file)
           "* TODO %?")
          ("m" "meeting" entry (file org-log-file)
           "* DONE %? :meeting:")))
  (setq org-refile-targets '((nil :maxlevel . 10)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)

  (defun org-time-id+ ()
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
 	   (time     (format-time-string "%y.%m%d.%H%M%S"))
 	   (seconds  (string-to-number (substring time 12 14)))
 	   (index    (floor (* (/ seconds 60.0) 26)))
 	   (letter   (substring alphabet index (1+ index) ))
 	   (time-id  (format "%s%s" (substring time 0 12) letter)))
      time-id))

  (defun org-time-id-full+ ()
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
 	   (time     (format-time-string "%y.%m%d.%H:%M:%S")))
      time))

  (defun archive-id+ ()
    (format-time-string "%y%m%d%H%M"))

  (defun insert-archive-id+ ()
    (interactive)
    (insert (org-time-id+)))

  (defun org-insert-time+ ()
    (interactive)
    (insert (format-time-string "%H:%M")))

  (defun org-insert-time-id+ ()
    (interactive)
    (insert (format-time-string "%y.%m%d.%H%M")))

  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame)))

  (defun org-show-tree ()
    (interactive)
    (org-content 10)))

(use-package org-agenda
  :ensure nil
  :after (org)
  :bind (:map evil-leader-state-map-extension
              ("n a" . org-agenda)))

(use-package org-id
  :config
  (setq org-agenda-files `(,notebox))
  (setq org-id-ts-format "%y.%m.%d.%H.%M.%S")
  (setq org-id-method 'ts)
  (org-id-update-id-locations))

(use-package org-ql
  :ensure t
  :commands (org-ql-search-buffer+)
  :bind (:map evil-leader-state-map-extension
              ("n q b" . org-ql-search-buffer+)
              ("n q a" . org-ql-search-all+)
              ("n q s" . org-ql-search)
              ("n q v" . org-ql-view)
              ("n g" . org-ql-find-guide))
  :config
  (defun org-ql-search-all+ ()
    (interactive)
    (let ((query (read-string "Query: ")))
      (org-ql-search (org-ql-view--expand-buffers-files "all") query)))

  (defun org-ql-search-buffer+ ()
    (interactive)
    (let ((query (read-string "Query: ")))
      (org-ql-search (org-ql-view--expand-buffers-files "buffer") query)))

  ;; todo: this doesn't seem to filter on headline?
  (defun org-ql-find-guide ()
    (interactive)
    (let ((guides (list (expand-file-name "guide.org" user-emacs-directory))))
      (org-ql-find guides :query-prefix "tags:#help "))))

;; (use-package org-roam
;;   :ensure t
;;   :custom (org-roam-directory (file-truename org-directory))
;;   :bind (:map evil-leader-state-map-extension
;;               ("n f" . org-roam-node-find+)
;;               ("n i" . org-roam-node-insert+)
;;               ("n d" . org-roam-goto-today+))
;;   :hook (org-roam-mode . org-show-all)
;;   :config
;;   (org-roam-db-autosync-mode)

;;   (advice-add #'org-roam-node-find :after (lambda (&rest args) (org-show-all)))

;;   (defun org-roam-node-create+ (node-name &optional hide)
;;     (let* ((id        (org-time-id+))
;;            (template  (format ":PROPERTIES:\n:ID:  %s\n:TAGGED:\n:END:\n\n#+title: %s\n\n%%?" id node-name))
;;            (filename  (format "%s %s.org" id node-name))
;;            (org-capture-templates (list (list "i" "i" 'plain
;;                                               (list 'file filename)
;;                                               template
;;                                               :immediate-finish t
;;                                               :jump-to-captured t))))
;;       (if hide
;;           (save-window-excursion (org-capture nil "i")
;;                                  (org-show-all))
;;         (org-capture nil "i")
;;         (org-show-all))
;;       id))

;;   (defun org-roam-node-find+ ()
;;     (interactive)
;;     (let* ((node (org-roam-node-read)))
;;       (if (org-roam-node-file node)
;;           (progn (org-roam-node-open node)
;;                  (org-show-all))
;;         (org-roam-node-create+ (org-roam-node-title node)))))

;;   (defun org-roam-node-insert+ (with-title &optional title)
;;     (interactive (list current-prefix-arg
;;                        (org-roam-node-title (org-roam-node-read))))
;;     (when (not (org-roam-node-titled title))
;;       (org-roam-node-create+ title t))
;;     (let* ((node (org-roam-node-titled title))
;;            (id (org-roam-node-id node)))
;;       (insert (format "id:%s%s"
;;                       id
;;                       (if with-title (concat " " title) "")))))

;;   (defun org-roam-node-titled (node-title)
;;     (->> (org-roam-node-list)
;;          (seq-find (lambda (node)
;;                      (equal (org-roam-node-title node)
;;                             node-title)))))

;;   (cl-defmethod org-roam-node-tagged ((node org-roam-node))
;;     "Return the currently set category for the NODE."
;;     (cdr (assoc-string "TAGGED" (org-roam-node-properties node))))

;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tagged:50}" 'face 'org-tag)))

;;   (defun org-roam-goto-today+ ()
;;     (interactive)
;;     (let* ((today (format-time-string "%y-%m-%m" (current-time)))
;;            (node  (org-roam-node-titled today)))
;;       (if node
;;           (org-roam-node-open node)
;;         (org-roam-node-create+ today)))))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode-hook . (lambda ()
                                (setq paragraph-start "[ \t]*$")
                                (setq paragraph-separate "[ \t]*$")
                                (markdown-toggle-url-hiding)
                                (my-markdown-add-highlighting)))
  :bind (:map markdown-mode-map
              ("C-c l" . markdown-open-some-buffer-link+ )
              ("C-c -" . mark-task-complete)
              ("C-c r" . random-line-jump)
              ("C-c a" . add-task-to-list)
              ("C-c d" . duplicate-task-at-point)
              ("C-c o" . markdown-follow-thing-at-point))
  :config
  (defface my-markdown-highlight-face
    '((t (:background "yellow")))
    "Face for highlighted text in markdown mode."
    :group 'markdown-faces)

  (defvar my-markdown-highlight-keyword
    '(("\\(==\\)\\([^=\n]+\\)\\(==\\)"
       (1 'default t) ; don't highlight the == delimiters
       (2 'my-markdown-highlight-face t)
       (3 'default t)))
    "Keyword for highlighting text in markdown mode.")

  (defun my-markdown-add-highlighting ()
    (interactive)
    "Add my custom markdown highlighting."
    (font-lock-add-keywords nil my-markdown-highlight-keyword))

  (setq markdown-enable-wiki-links t)

  (setq markdown-translate-filename-function (lambda (url) (string-replace "%20" " " url)))

  (defun markdown-next-link-show+ ()
    (interactive)
    (when (call-interactively #'markdown-next-link)
      (markdown-follow-thing-at-point t)))

  (defun collect-markdown-links ()
    "Collect all Markdown links from the current buffer."
    (interactive)
    (let ((brace-regex "\\[.*?\\](\\(.*?\\))")
          (http-regex "[^(]\\(https?://[^ \n]*\\)")
          links)
      (seq-doseq (regex `(,brace-regex ,http-regex))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regex nil t)
            (push (match-string-no-properties 1) links))))
      (reverse links)))

  (defun markdown-open-some-buffer-link+ ()
    (interactive)
    (let* ((links (collect-markdown-links))
           (link (consult--read (seq-map (lambda (link) (string-replace "%20" " " link)) links) :prompt "links: " :sort nil)))
      (markdown--browse-url (string-replace " " "%20" link))))

  (defun random-line (start end)
    (interactive (if (region-active-p)
                     (list (line-number-at-pos (caar (region-bounds)))
                           (line-number-at-pos (cadr (region-bounds))))
                   (list (line-number-at-pos (point-min))
                         (line-number-at-pos (point-max)))))
    (save-excursion
      (when (region-active-p)
        (deactivate-mark))
      (forward-line (- (+ start (random (+ 1 (- end start))))
                         (line-number-at-pos)))
      (message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
  
  (defun random-line-jump (start end)
    (interactive (if (region-active-p)
                     (list (line-number-at-pos (caar (region-bounds)))
                           (line-number-at-pos (cadr (region-bounds))))
                   (list (line-number-at-pos (point-min))
                         (line-number-at-pos (point-max)))))
    (when (region-active-p)
      (deactivate-mark))
    (forward-line (- (+ start (random (+ 1 (- end start))))
                     (line-number-at-pos))))

  (defun mark-task-complete ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp "[[:alpha:]]")
      (backward-char 1)
      (insert "~~")
      ;; (end-of-line)
      (if (search-forward-regexp "\\( \\+[[:alpha:]]\\| \\[?http\\| \\[.+\\](.+)\\)" (save-excursion (end-of-line) (point)) t)
          (search-backward-regexp "\\( \\+[[:alpha:]]\\| \\[?http\\| \\[.+\\](.+)\\)" (save-excursion (beginning-of-line) (point)) t)
        (end-of-line))
      (insert "~~")
      ))

  (defun duplicate-task-at-point ()
    (interactive)
    (append-task-to-list (buffer-substring (save-excursion (beginning-of-line) (point))
                                           (save-excursion (end-of-line) (point)))))

  (defun add-task-to-list (task)
    (interactive (list (format "+ %s" (read-string "Task to add: "))))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^\\+")
      (backward-char 1)
      (insert (format "%s\n" task))))

  (defun markdown-font-lock-gtd-project ()
    "Add custom font-lock keywords for markdown files with 'gtd' in the title."
    (font-lock-add-keywords nil
                            '(("\\b\\(P\\)[[:digit:]]+\\b" 1 'gnus-emphasis-underline-bold)
                              ("\\bP\\([[:digit:]]+\\b\\)" 1 'success))))

  (defun markdown-font-lock-tags ()
    "Add custom font-lock keywords for markdown files with 'gtd' in the title."
    (font-lock-add-keywords nil
                            '(("\\(?:^\\|\\s-\\)\\(##?\\)[[:alnum:]-/]+\\b" 1 'gnus-emphasis-underline-bold)
                              ("\\(?:^\\|\\s-\\)##?\\([[:alnum:]-/]+\\b\\)" 1 'success))))

  (defun markdown-font-lock-gtd-contexts ()
    "Add custom font-lock keywords for markdown files with 'gtd' in the title."
    (font-lock-add-keywords nil
                            '(("\\(?:^\\|\\s-\\)\\(@\\)[[:alnum:]/\.]+\\b" 1 'gnus-emphasis-underline-bold)
                              ("\\(?:^\\|\\s-\\)@\\([[:alnum:]/\.]+\\b\\)" 1 'success))))

  (add-hook 'markdown-mode-hook #'markdown-font-lock-gtd-project)
  (add-hook 'markdown-mode-hook #'markdown-font-lock-tags)
  (add-hook 'markdown-mode-hook #'markdown-font-lock-gtd-contexts)
  (defun markdown-display-inline-images ()
    "Add inline image overlays to image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."
    (interactive)
    (unless (display-images-p)
      (error "Cannot show images"))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward markdown-regex-link-inline nil t)
          (let* ((start (match-beginning 0))
                 (imagep (match-beginning 1))
                 (end (match-end 0))
                 (file (match-string-no-properties 6))
                 (resize (ignore-error t
                           (thread-last (match-string 3)
                                        (s-split "|")
                                        cadr
                                        (s-split "x")
                                        (seq-map #'string-to-number)
                                        ((lambda (x) (cons (car x) (cadr x))))))))
            (message "%S %S" (match-string 3) resize)
            (when (and imagep
                       (not (zerop (length file))))
              (unless (file-exists-p file)
                (let* ((download-file (funcall markdown-translate-filename-function file))
                       (valid-url (ignore-errors
                                    (member (downcase (url-type (url-generic-parse-url download-file)))
                                            markdown-remote-image-protocols))))
                  (if (and markdown-display-remote-images valid-url)
                      (setq file (markdown--get-remote-image download-file))
                    (when (not valid-url)
                      ;; strip query parameter
                      (setq file (replace-regexp-in-string "?.+\\'" "" file))
                      (unless (file-exists-p file)
                        (setq file (url-unhex-string file)))))))
              (when (file-exists-p file)
                (let* ((abspath (if (file-name-absolute-p file)
                                    file
                                  (concat default-directory file)))
                       (image
                        (cond ((and markdown-max-image-size
                                    (image-type-available-p 'imagemagick))
                               (create-image
                                abspath 'imagemagick nil
                                :max-width (car markdown-max-image-size)
                                :max-height (cdr markdown-max-image-size)))
                              (markdown-max-image-size
                               (create-image abspath nil nil
                                             :max-width (car markdown-max-image-size)
                                             :max-height (cdr markdown-max-image-size)))
                              (resize
                               (create-image abspath nil nil
                                             :max-width (car resize)
                                             :max-height (cdr resize)))
                              (t (create-image abspath)))))
                  (when image
                    (let ((ov (make-overlay start end)))
                      (overlay-put ov 'display image)
                      (overlay-put ov 'face 'default)
                      (push ov markdown-inline-image-overlays)))))))))))
  )

(defun custom-set-icons (&rest args) (ignore)) ;;; needed for bug when loading custom.el
                                               ;;; custom-set-icons isn't defined for some reason
(setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
(when (not (file-exists-p custom-file))
  (shell-command (concat "touch " custom-file)))
(load-file custom-file)

(use-package shell
  :after (minibuffer)
  :commands (select-shell-history)
  :bind (:map shell-mode-map
              ("C-p" . comint-previous-input)
              ("C-n" . comint-next-input)
              ("M-p" . comint-previous-prompt)
              ("M-n" . comint-next-prompt)
              ("C-c r" . select-shell-history)
              ("C-c s" . shell-rename+))
  :config
  (setq shell-file-name "/bin/zsh")
  (setq comint-process-echoes t)

  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (defun clear-line ()
    (move-beginning-of-line 1)
    (ignore-errors (kill-line)
		   (pop kill-ring)))



  (defun shell-rename+()
    (interactive)
    (rename-buffer (concat "*shell* " (read-string "shell name: ")))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-base-face nil :bold t))

;; TODO: change xref--insert-xrefs to use a more grep-like output format
;;       then try to use wgrep
;; TODO: can I get this to hook into compile modes next/previous-error?
(use-package xref
  :after (evil-leader)
  :ensure nil
  :bind (:map xref--xref-buffer-mode-map
              ("n"   . xref-next-line-no-show)
              ("p"   . xref-prev-line-no-show)
              ("M-n" . xref-next-line)
              ("M-p" . xref-prev-line)))


(use-package flymake
  :after (evil-leader)
  :ensure nil
  :hook (flymake-mode-hook . (lambda () (cond
                                         (flymake-mode (help-at-pt-set-timer))
                                         (t            (help-at-pt-cancel-timer)))))
  :bind (:map evil-leader-state-map-extension
              ("m a" . flymake-show-project-diagnostics)
              ("m n" . flymake-goto-next-error)
              ("m p" . flymake-goto-prev-error)
              ("m c" . consult-flymake))
  :config
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 1.0))

(use-package pulsar
  :ensure t
  :bind (:map evil-leader-state-map-extension
              ("RET" . pulsar-pulse-line)
              ("v p" . pulsar-global-mode))
  :custom
  (pulsar-pulse-functions '(recenter-top-bottom move-to-window-line-top-bottom reposition-window bookmark-jump other-window delete-window delete-other-windows forward-page backward-page scroll-up-command scroll-down-command windmove-right windmove-left windmove-up windmove-down windmove-swap-states-right windmove-swap-states-left windmove-swap-states-up windmove-swap-states-down tab-new tab-close tab-next org-next-visible-heading org-previous-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-backward-same-level outline-forward-same-level outline-next-visible-heading outline-previous-visible-heading outline-up-heading copy-window imenu switch-to-buffer magit-status open-init consult-buffer evil-goto-first-line evil-goto-line evil-jump-backward evil-jump-forward last-buffer elfeed))
  :hook (focus-in-hook . pulsar-pulse-line)
  :config

  (advice-add 'consult-buffer :after #'pulsar-recenter-middle)
  (advice-add 'consult-imenu :after #'pulsar-recenter-middle)

  (setq pulsar-pulse t)
  (setq pulsar-delay 0.02)
  (setq pulsar-iterations 20)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(use-package devdocs
  :after (embark)
  :ensure t
  :bind (:map evil-normal-state-map
              ("g D" . devdocs-lookup))
  :config
  (defun devdocs-lookup+ ()
    (interactive)
    (let ((symbol (thing-at-point 'symbol)))
      (devdocs-lookup nil symbol))))

(use-package highlight-indentation
  :ensure t
  :hook ((python-mode-hook . highlight-indentation-mode))
  :config
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package cider
  :ensure t)

(save-window-excursion (switch-to-buffer "*Messages*") (evil-normal-state))

(let ((local-init-file (concat (expand-file-name user-emacs-directory)
                               "local-init.el")))
  (when (file-exists-p local-init-file)
    (load local-init-file)))

(server-start)

(use-package gnu-apl-mode
  :ensure t
  :config
  (defun em-gnu-apl-init ()
    (setq buffer-face-mode-face 'gnu-apl-default)
    (buffer-face-mode))
  (add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
  (add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init))

(use-package ediff
  :config
  (defun ediff-setup-windows-tab (buffer-A buffer-B buffer-C control-buffer)
    (call-interactively #'tab-bar-duplicate-tab)
    (tab-bar-rename-tab "*ediff*")
    (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))

  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-tab)

  (defun ediff-quit+ (reverse-default-keep-variants)
    (interactive "P")
    (ediff-quit reverse-default-keep-variants)
    (tab-close))

  (defun my-setup-ediff-keybindings ()
    (define-key ediff-mode-map "q" 'ediff-quit+))

  (add-hook 'ediff-keymap-setup-hook 'my-setup-ediff-keybindings))

(use-package archive-search
  :load-path my-package-dir
  :after (consult)
  :bind (("C-, t" . archive-insert-tag)
         :map evil-leader-state-map-extension
	 ("n s" . archive-interactive-search)
         ("n S" . archive-search)))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-mode-line t)
  (spacious-padding-mode t)
  (spacious-padding-mode nil)
  (spacious-padding-mode t)
  (set-face-attribute 'mode-line nil :height 1.2 :underline nil :bold nil)
  (set-face-attribute 'mode-line-active nil :height 1.2 :underline nil :bold nil)
  (set-face-attribute 'mode-line-inactive nil :height 1.2 :underline nil))

(use-package drag-stuff
  :ensure t
  :config
    (define-key evil-normal-state-map (kbd "P") 'drag-stuff-up)
    (define-key evil-normal-state-map (kbd "N") 'drag-stuff-down)
    (define-key evil-motion-state-map (kbd "P") 'drag-stuff-up)
    (define-key evil-motion-state-map (kbd "N") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "P") 'drag-stuff-up)
    (define-key evil-visual-state-map (kbd "N") 'drag-stuff-down)
)

;; (use-package gptel
;;   :ensure t
;;   :config)

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0)
  (setq which-key-use-C-h-commands nil)
  (setq which-key-show-early-on-C-h nil)
  :bind (:map evil-leader-state-map-extension
              ("v k" . which-key-mode)))

(kill-buffer "*scratch*")
(setq debug-on-error nil)

(debug-on-entry 'set-window-dedicated-p)
(cancel-debug-on-entry 'set-window-dedicated-p)

(remove-hook 'embark-collect-mode-hook #'make-non-dedicated-window)
(put 'scroll-left 'disabled nil)
