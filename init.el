(setq debug-on-error t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(setq my-package-dir (concat (expand-file-name user-emacs-directory) "my-packages/"))

(setq use-package-enable-imenu-support t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :custom ((use-package-hook-name-suffix "")))

(use-package emacs
  :after (evil-leader)
  :commands (insert-time-id
             eval-region+
             eval-buffer+
             eval-region-and-replace
	     toggle-show-trailing-whitespace
	     select-from-history
             mirror-window
             isearch-abort+)
  :bind (:map isearch-mode-map
              ("C-g" . isearch-abort+)
         :map minibuffer-mode-map
	      ("M-p" . nil)
	      ("M-n" . nil)
              ("C-c l" . edit-minibuffer)
	      ("A-r" . select-from-history)
	      ("A-p" . previous-history-element)
	      ("A-n" . next-history-element)
	 :map evil-leader-state-map-extension
	      ("t x" . switch-to-minibuffer)
	      ("e r" . eval-region+)
	      ("e b" . eval-buffer+)
	      ("e L" . eval-expression-and-replace)
	      ("f k" . kill-filepath)
	      ("f e" . echo-filepath)
	      ("v w" . toggle-show-trailing-whitespace)
	      ("v h" . global-hl-line-mode)
	      ("v e" . setenv)
              ("t C" . 'copy-window))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (when (file-exists-p "/opt/homebrew/bin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/bin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/bin"))))
  (when (file-exists-p "/opt/homebrew/sbin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/sbin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/sbin"))))
  (when (file-exists-p "/Users/joey/Library/Python/3.8/bin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/Users/joey/Library/Python/3.8/bin"))
    (setq exec-path (append exec-path (list "/Users/joey/Library/Python/3.8/bin"))))

  (setq split-width-threshold 100
	split-height-threshold 9999)
  (setq display-buffer-alist
	'(("\\*Help\\*"
	   (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-window)
	   (side . right))
	  ("\\*Embark Collect"
	   (display-buffer-reuse-window)
	   (side . bottom))
	  ;; ("\\*Embark Export"
	  ;;  (display-buffer-reuse-window display-buffer-pop-up-window)
	  ;;  (inhibit-same-window . t))
	  ("\\*Messages\\*"
	   (display-buffer-reuse-window)
	   (inhibit-same-window . t))))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (fringe-mode (cons 1 1))
  (tab-bar-mode -1)

  (condition-case nil
      (set-face-attribute 'default nil :font "iosevka" :height 150)
    (error (set-face-attribute 'default nil :height 150)))

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

  (setq monitor-attributes '(("ultrawide" . ((font-height . 170)
					     (frame-width . 190)))
			     ("mac-retina" . ((font-height . 150)
					      (frame-width . 179)))))

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

  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'alt)

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

  (defun clear-line ()
    (move-beginning-of-line 1)
    (ignore-errors (kill-line)
		   (pop kill-ring)))

  (defun select-from-history ()
    (interactive)
    (let ((history (seq-uniq (minibuffer-history-value)))
	  (vertico-sort-override-function #'identity))
      (if (> (length history) 0)
	  (let ((chosen-history (completing-read "input history: " history)))
	    (clear-line)
	    (insert chosen-history))
	(message "no history items"))))

  (defun kill-filepath ()
    (interactive)
    (kill-new (buffer-file-name)))

  (defun echo-filepath ()
    (interactive)
    (message (buffer-file-name)))

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
      (emacs-lisp-mode)
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

  )

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
         :map compilation-mode-map
	      ("n" . next-line)
	      ("p" . previous-line)
	      ("M-n" . next-error+)
	      ("M-p" . previous-error+)
              ("M-<return>" . compile-goto-error-no-select)
	      ("d" . compile-delete-line+)
              ("D" . compile-delete-line-no-select+))
  :config
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

  (defun append-to-zsh-history (&rest r)
    (shell-command (format "echo '%s' >> ~/.zsh_history" (car r))))

  (advice-add 'compile :after #'append-to-zsh-history))

(use-package project
  :after (evil-leader)
  :demand t
  :commands (project-dir-.emacs.d
	     project-switch-current)
  :bind (:map evil-leader-state-map-extension
	      ("p ." . project-switch-current)
	      ("p r" . consult-grep)
	      ("p R" . consult-grep-case-sensitive)
	      ("p F" . find-grep-dired-default-dir)
	      ("C-p n" . project-dir-notes)
	      ("C-p e" . project-dir-.emacs.d))
  :config
  (setq project-switch-commands
	'((?f "File"          project-find-file)
	  (?F "CLI Find"      find-grep-dired-default-dir)
	  (?n "New file"      find-file)
	  (?r "Grep"          consult-grep)
	  (?R "Grep"          consult-grep-case-sensitive)
	  (?d "Project Dir"   project-dired)
	  (?D "Dired"         dired)
	  (?b "Buffer"        project-switch-to-buffer)
	  (?q "Query replace" project-query-replace-regexp)
	  (?g "Magit"         magit-project-status)
	  (?c "Compile"       compile)
	  (?C "Recompile"     recompile)
	  (?e "Eshell"        project-eshell)
	  (?s "Shell"         shell)
          (?m "Make"          project-make+)))

  (defun project-switch-current ()
    (interactive)
    (project-switch-project (project-root (project-current nil))))

  (defun project-dir-.emacs.d ()
    (interactive)
    (project-switch-project user-emacs-directory))

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

  (add-hook 'project-find-functions #'project-override)

  )

(use-package vertico
  :after (evil-leader)
  :ensure t
  :demand t
  :bind (("C-M-x" . vertico-repeat)
	 :map vertico-map
	      ("C-p" . vertico-C-p-or-reverse)
	      ("C-M-n" . vertico-next-group)
	      ("C-M-p" . vertico-previous-group)
	      ("M-n" . vertico-next)
	      ("M-p" . vertico-previous)
	      ("C-<return>" . vertico-exit-input)
	      ("C-^" . vertico-directory-up)
              ("M-h" . vertico-directory-up)
         :map evil-leader-state-map-extension
              ("X" . vertico-repeat))
  :config
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle nil)

  (vertico-mode)
  (set-face-attribute 'vertico-group-title nil :foreground "blue")
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (defun vertico-C-p-or-reverse ()
    (interactive)
    (cond
     (vertico-unobtrusive-mode (call-interactively #'vertico-multiform-reverse))
     (vertico-reverse-mode     (call-interactively #'vertico-next))
     (t                        (call-interactively #'vertico-previous)))))

(use-package vertico-multiform
  :ensure nil
  :demand t
  :config
  (vertico-multiform-mode 1)
  (vertico-reverse-mode 1)
  (vertico-reverse-mode -1)
  (define-key vertico-reverse-map (kbd "M-n") 'vertico-previous)
  (define-key vertico-reverse-map (kbd "M-p") 'vertico-next) 

  (setq vertico-multiform-categories '((imenu buffer)
                                       (consult-grep buffer)
                                       (t unobtrusive)))

  (setq vertico-multiform-commands '((consult-line buffer)
                                     (consult-imenu buffer)
                                     (select-from-history reverse (vertico-resize . t))
                                     (select-shell-history reverse (vertico-resize . t)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package corfu
  :ensure t
  :bind (("C--" . completion-at-point))
  :config
  (global-corfu-mode))

(use-package consult
  :after (evil-leader)
  :commands (consult-grep-dir consult-grep-dir-case-sensitive)
  :ensure t
  :bind (("C-M-y" . consult-yank-from-kill-ring)
         :map evil-leader-state-map-extension
   	      ("b b"   . consult-buffer)
   	      ("b B"   . switch-to-buffer)
   	      ("s l"   . consult-outline)
   	      ("s s"   . consult-line)
   	      ("s r"   . consult-grep-dir)
   	      ("s R"   . consult-grep-dir-case-sensitive)
	      ("s i"   . consult-imenu)
	      ("s o"   . occur))
  :config
  (setq consult-preview-key (list (kbd "M-<return>") (kbd "M-n") (kbd "M-p")))
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  (defun consult-grep-case-sensitive (&optional dir)
    (interactive "P")
    (let ((consult-grep-args "grep --null --line-buffered --color=never --line-number -I -r ."))
      (consult-grep)))

  (defun consult-grep-dir (&optional dir)
    (interactive "P")
    (if dir
	(consult-grep default-directory)
      (consult-grep t)))

  (defun consult-grep-dir-case-sensitive (&optional dir)
    (interactive "P")
    (let ((consult-grep-args "grep --null --line-buffered --color=never --line-number -I -r ."
                             ))
      (if dir
	(consult-grep default-directory)
      (consult-grep t))))
  
  (evil-add-command-properties 'consult-line :jump t)
  (evil-add-command-properties 'consult-imenu :jump t))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package consult-dir
  :ensure t
  :demand t
  :after (consult)
  :bind (:map minibuffer-mode-map
         ("C-c d" . consult-dir)))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; TODO: remove confirmation from kill-buffer
(use-package embark
  :after (vertico)
  :ensure t
  :demand t
  :commands (embark-act-quit forward-button-click+ backward-button-click+ embark-collect-delete+)
  :bind (("C-." . embark-act)
   	 :map minibuffer-mode-map
   	      ("TAB" . minibuffer-force-complete)
   	      ("SPC" . self-insert-command)
   	      ("M-." . embark-act-quit)
   	      ("C-." . embark-act)
   	      ("C-," . embark-become)
   	      ("C-c e" . embark-export)
   	      ("C-c c" . embark-collect)
	 :map vertico-map
   	      ("C-c e" . embark-export)
   	 :map embark-meta-map
   	      ("C-h" . nil)
   	      ("C-." . embark-keymap-help)
	 :map embark-collect-mode-map
	      ("d" . embark-collect-delete+)
	      ("D" . embark-collect-delete-no-select+)
	      ("M-n" . forward-button-click+)
	      ("M-p" . backward-button-click+))
  :config
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
      (embark-act)))

  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action nil)
  (setq embark-action-indicator (lambda (map)
   				  (which-key--show-keymap "Embark" map nil nil 'no-paging)
   				  #'which-key--hide-popup-ignore-command))
  (setq embark-become-indicator embark-action-indicator)

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
        (previous-line)))))

(use-package embark-maps
  :after (embark)
  :demand t
  :load-path my-package-dir)

(use-package orderless
  :ensure t
  :demand t
  :commands (orderless-help+)
  :bind (:map minibuffer-mode-map
              ("C-c ?" . orderless-help+))
  :config
  (setq completion-styles '(orderless))
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

  (setq orderless-matching-styles '(orderless-regexp)
   	orderless-style-dispatchers '(orderless-flex-if-twiddle-dispatcher+
   				      orderless-initialism-dispatcher+
   				      orderless-literal-dispatcher+
   				      orderless-without-dispatcher+)))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :config
  (setq evil-respect-visual-line-mode t)
  (setq-default evil-symbol-word-search t)
  (setq evil-mode-line-format nil)
  (evil-set-undo-system 'undo-tree)
  (setq evil-default-state 'emacs)
  (setq evil-want-minibuffer t)

  (setq evil-emacs-state-modes nil)
  (setq evil-normal-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-operator-state-modes nil)
  (setq evil-replace-state-modes nil)

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
  :config
  (evil-escape-mode)
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay .1))

(use-package evil-baptism
  :after evil
  :load-path my-package-dir)

(use-package evil-leader
  :after (evil)
  :load-path my-package-dir
  :config
  (define-key evil-leader-state-map-extension (kbd "i t") 'insert-time-id)
  (global-set-key (kbd "C-M-SPC") evil-leader-state-map-extension))

(use-package magit
  :ensure t
  :bind (:map magit-status-mode-map
              ("<" . magit-section-up)
         :map evil-leader-state-map-extension
	      ("g g" . magit-status) 
              ("g c" . magit-clone))
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package avy
  :ensure t
  :config
  (setq avy-keys (list ?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package dired
  :commands (find-grep-dired-default-dir)
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind (:map evil-leader-state-map-extension
	      ("s F" . find-grep-dired)
 	      ("s f" . find-grep-dired-default-dir)
  	 :map dired-mode-map
 	      ("C-M-n" . nil)
 	      ("C-M-p" . nil)
 	      ("<" . dired-up-directory)
 	      ("M-s f C-s" . nil)
  	      ("M-s f ESC" . nil)
 	      ("M-s f" . nil)
 	      ("e" . wdired-change-to-wdired-mode)
  	      ("M-n" . dired-preview-next)
  	      ("M-p" . dired-preview-previous)
	      ("M-<return>" . dired-preview))
  :config
  (setq dired-listing-switches "-Al")

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
    (dired-preview)))

(use-package ls-lisp
  :after (dired)
  :config
  (setq dired-use-ls-dired t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t))

(use-package dired-narrow
  :ensure t
  :commands (dired-narrow-archive)
  :bind (:map dired-mode-map
  	      ("/" . dired-narrow)))

(use-package ace-window
  :ensure t
  :after (evil-leader)
  :commands (ace-copy-window ace-move-window ace-switch-buffer-other-window)
  :bind (:map evil-leader-state-map-extension
	      ("t s" . ace-swap-window)
	      ("t k" . ace-delete-window)
	      ("t K" . delete-window)
	      ("t c" . ace-copy-window)
	      ("t m" . ace-move-window)
              ("t ." . aw-flip-window))
  :config
  (defun ace-copy-window ()
    (interactive)
    (aw-select #'aw-copy-window))

  (defun ace-move-window ()
    (interactive)
    (aw-select #'aw-move-window))

  (defun ace-switch-buffer-other-window ()
    (interactive)
    (aw-select #'aw-switch-buffer-other-window))
  
  (setq aw-keys '(?u ?h ?e ?t))
  (setq aw-dispatch-always nil))

(use-package yasnippet
  :ensure t
  :demand t
  :bind (:map evil-leader-state-map-extension
              ("i y" . yas-insert-snippet))
  :config
  (yas-global-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode-hook . visual-fill-column-mode))

(use-package smerge-mode
  :ensure nil
  :after (hydra)
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
    ("\C-m" smerge-keep-current)
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
    ("q" nil "cancel" :color blue))

  :bind (:map evil-leader-state-map-extension
	      ("g s" . hydra/smerge/body))
  :hook (magit-diff-visit-file . (lambda ()
				   (when smerge-mode
				     (hydra/smerge/body)))))

(use-package winner
  :after (evil-leader)
  :config
  (winner-mode 1)
  (define-key evil-leader-state-map-extension (kbd "t u") 'winner-undo)
  (define-key evil-leader-state-map-extension (kbd "t r") 'winner-redo))

(use-package tab-bar
  :ensure nil
  :config
  (setq tab-bar-mode t)
  (setq tab-bar-show nil))

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
    (let ((python-shell-interpreter "python3")
          (python-shell-interpreter-args "-m IPython --simple-prompt"))
      (call-interactively #'run-python))))

(use-package pyvenv
  :ensure t)

(use-package f
  :ensure t
  :demand t)

(use-package org
  :ensure nil
  :commands (open-log-file+ org-insert-timestamp+ org-open-some-buffer-link+)
  :hook (org-mode-hook . visual-line-mode)
  :bind (:map org-mode-map
	      ("M-n"  . org-next-visible-heading)
	      ("M-p"  . org-previous-visible-heading)
	      ("C-c ."  . org-insert-timestamp+)
	      ("C-c c"  . org-cycle)
	      ("C-c l"  . org-open-some-buffer-link+)
         :map evil-leader-state-map-extension
              ("o l" . open-log-file+)
              ("C-p n" . project-dir-notes)
              ("n p" . project-dir-notes)
              ("n f" . find-file-notebox)
              ("n d" . open-log-file+))
  :config
  (setq org-startup-folded 'showeverything)
  (setq org-property-format  "%-12s %s")
  (setq org-image-actual-width nil)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?Z)
  (setq org-hide-leading-stars nil)
  (setq org-log-into-drawer t)
  (setq org-fontify-whole-heading-line t)
  (setq notebox (expand-file-name "~/notes/"))

  (setq org-babel-load-languages '((emacs-lisp . t)))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  (set-face-attribute 'org-level-1 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-2 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-3 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-4 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-5 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-6 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-7 nil :background "#F0F0F0" :overline t :underline t :bold t)

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
                      (when (member (org-element-property :type link) '("http" "https"))
                        link)))))
      (browse-url (completing-read "links: " links))))

  (defun find-file-notebox ()
    (interactive)
    (let ((default-directory notebox))
      (call-interactively #'project-find-file))))

(use-package org-id
  :config
  (setq org-agenda-files `(,notebox))
  (org-id-update-id-locations))

(use-package markdown-mode
  :ensure t)

;; TODO: read through this package
;; (use-package ob-http
;;   :ensure t
;;   :config
;;   (add-to-list 'org-babel-load-languages '(http . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(defun custom-set-icons (&rest args) (ignore)) ;;; needed for bug when loading custom.el
                                               ;;; custom-set-icons isn't defined for some reason
  (setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
  (when (not (file-exists-p custom-file))
    (shell-command (concat "touch " custom-file)))
  (load-file custom-file)

(use-package shell
  :commands (select-shell-history)
  :bind (:map minibuffer-mode-map
              ("A-h" . select-shell-history)
         :map shell-mode-map
              ("C-p" . comint-previous-input)
              ("C-n" . comint-next-input)
              ("M-p" . comint-previous-prompt)
              ("M-n" . comint-next-prompt)
              ("A-h" . select-shell-history)
              ("C-c r" . shell-rename+))
  :config
  (setq shell-file-name "/bin/zsh")
  (setq comint-process-echoes t)
  
  (defun clear-line ()
    (move-beginning-of-line 1)
    (ignore-errors (kill-line)
		   (pop kill-ring)))
  
  (defun select-shell-history ()
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
	    (let ((chosen-history (completing-read "history: " history)))
	      (clear-line)
	      (insert chosen-history))))))

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
              ("M-p" . xref-previous-line)))

(use-package lsp-mode
  :ensure t
  :hook ((lsp-managed-mode-hook . (lambda () (when flymake-mode (flymake-mode -1)))))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (evil-define-key '(normal motion) lsp-mode-map (kbd "g r") 'lsp-find-references))

(use-package lsp-java
  :after (lsp)
  :ensure t)

;; python https://github.com/python-lsp/python-lsp-server
;; java   https://github.com/eclipse/eclipse.jdt.ls
;;        https://github.com/joaotavora/eglot/pull/864
;; (use-package eglot
;;   :ensure t
;;   :demand t
;;   :hook (eglot-managed-mode-hook . (lambda () (when eldoc-mode   (eldoc-mode -1))
;;                                               (when flymake-mode (flymake-mode -1))))
;;   :config
;;   (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)))

;; ;; TODO: how can i make the `eldoc` function echo instead of pulling up a buffer?
;; (use-package eldoc
;;   :after (evil-leader)
;;   :ensure nil
;;   :hook (emacs-lisp-mode-hook . eldoc-mode)
;;   :bind (:map evil-leader-state-map-extension
;;               ("s e" . eldoc))
;;   :config
;;   (global-eldoc-mode -1))

;; (use-package flymake
;;   :after (evil-leader)
;;   :ensure nil
;;   :hook (flymake-mode-hook . (lambda () (cond
;;                                          (flymake-mode (help-at-pt-set-timer))
;;                                          (t            (help-at-pt-cancel-timer)))))
;;   :bind (:map evil-leader-state-map-extension
;;               ("m a" . flymake-show-project-diagnostics)
;;               ("m n" . flymake-goto-next-error)
;;               ("m p" . flymake-goto-prev-error)
;;               ("m c" . consult-flymake))
;;   :config
;;   (setq help-at-pt-display-when-idle t)
;;   (setq help-at-pt-timer-delay 1.0))

(use-package pulsar
  :ensure t
  :custom
  (pulsar-pulse-functions '(recenter-top-bottom move-to-window-line-top-bottom reposition-window bookmark-jump other-window delete-window delete-other-windows forward-page backward-page scroll-up-command scroll-down-command windmove-right windmove-left windmove-up windmove-down windmove-swap-states-right windmove-swap-states-left windmove-swap-states-up windmove-swap-states-down tab-new tab-close tab-next org-next-visible-heading org-previous-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-backward-same-level outline-forward-same-level outline-next-visible-heading outline-previous-visible-heading outline-up-heading copy-window imenu switch-to-buffer magit-status open-init consult-buffer evil-goto-first-line evil-goto-line))
  :config

  (advice-add 'consult-buffer :after #'pulsar-recenter-middle)
  (advice-add 'consult-imenu :after #'pulsar-recenter-middle)
  
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.04)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(save-window-excursion (switch-to-buffer "*Messages*") (evil-normal-state))

(let ((local-init-file (concat (expand-file-name user-emacs-directory)
                               "local-init.el")))
  (when (file-exists-p local-init-file)
    (load local-init-file)))

(defun display-startup-echo-area-message ()
  (let ((seconds (progn (string-match "[[:digit:]]+\\.[[:digit:]]\\{2\\}" (emacs-init-time)) (match-string 0 (emacs-init-time)))))
    (message (format "Emacs started in %s seconds." seconds))))

(setq debug-on-error nil)
