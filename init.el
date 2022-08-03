(setq debug-on-error t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(setq my-package-dir (concat (expand-file-name user-emacs-directory) "my-packages/"))

(setq use-package-enable-imenu-support t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package emacs
  :after (evil-leader)
  :commands (insert-time-id
	     toggle-show-trailing-whitespace
	     select-from-history
             mirror-window)
  :bind (:map minibuffer-mode-map
	      ("M-p" . nil)
	      ("M-n" . nil)
	      ("A-r" . select-from-history)
	      ("A-p" . previous-history-element)
	      ("A-n" . next-history-element)
	 :map evil-leader-state-map-extension
	      ("v w" . toggle-show-trailing-whitespace)
	      ("v h" . global-hl-line-mode)
              ("t m" . 'mirror-window))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (when (file-exists-p "/opt/homebrew/bin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/bin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/bin"))))
  (when (file-exists-p "/opt/homebrew/sbin")
    (setenv "PATH" (concat (getenv "PATH") ":" "/opt/homebrew/sbin"))
    (setq exec-path (append exec-path (list "/opt/homebrew/sbin"))))

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
  (set-face-attribute 'default nil :font "iosevka" :height 150)
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

  (defun mirror-window ()
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
	(message "no history items")))))

(use-package grep
  :after (compile)
  :bind (:map grep-mode-map
	      ("n" . next-line)
	      ("p" . previous-line)
	      ("M-n" . next-error+)
	      ("M-p" . previous-error+)
              ("d" . compile-delete-line+))
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
  :bind (:map compilation-mode-map
	      ("n" . next-line)
	      ("p" . previous-line)
	      ("M-n" . next-error+)
	      ("M-p" . previous-error+)
              ("M-<return>" . compile-goto-error-no-select)
	      ("d" . compile-delete-line+))
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
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char 1)
      (when (equal (line-number-at-pos (point-max))
                   (line-number-at-pos (point)))
        (previous-line))
      (previous-line)
      (next-line)))

  (define-key compilation-mode-map (kbd "d") 'compile-delete-line+))

(use-package project
  :after (evil-leader)
  :demand t
  :commands (project-dir-.emacs.d
	     project-switch-current)
  :bind (:map evil-leader-state-map-extension
	      ("p ." . project-switch-current)
	      ("p F" . find-grep-dired-default-dir)
	      ("C-p e" . project-dir-.emacs.d))
  :config
  (setq project-switch-commands
	'((?f "File" project-find-file)
	  (?F "CLI Find" find-grep-dired-default-dir)
	  (?n "New file" find-file)
	  (?g "Grep" consult-grep)
	  (?d "Project Dir" project-dired)
	  (?D "Dired" dired)
	  (?b "Buffer" project-switch-to-buffer)
	  (?q "Query replace" project-query-replace-regexp)
	  (?G "Magit" magit-project-status)
	  (?c "Compile" compile)
	  (?C "Recompile" recompile)
	  (?e "Eshell" project-eshell)
	  (?s "Shell" shell)))

  (defun project-switch-current ()
    (interactive)
    (project-switch-project (project-root (project-current nil))))

  (defun project-dir-.emacs.d ()
    (interactive)
    (project-switch-project user-emacs-directory)))

(use-package vertico
  :ensure t
  :demand t
  :bind (("C-M-x" . vertico-repeat)
	 :map vertico-map
	 ("M-n" . vertico-next)
	 ("M-p" . vertico-previous)
	 ("C-<return>" . vertico-exit-input)
	 ("C-^" . vertico-directory-up)
         ("M-h" . vertico-directory-up))
  :config
  (vertico-mode 1)
  
  (setq vertico-count 10)
  (set-face-attribute 'vertico-group-title nil :foreground "blue"))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode))

(use-package consult
  :after (evil-leader)
  :ensure t
  :bind (:map evil-leader-state-map-extension
   	      ("b b"   . consult-buffer)
   	      ("b B"   . switch-to-buffer)
   	      ("s l"   . consult-outline)
   	      ("s s"   . consult-line)
   	      ("s g"   . consult-grep-dir)
   	      ("s G"   . lgrep)
	      ("s i"   . consult-imenu)
	      ("s o"   . occur))
  :config
  (setq consult-preview-key (list (kbd "M-<return>") (kbd "M-n") (kbd "M-p")))
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  (defun consult-grep-dir (&optional dir)
    (interactive "P")
    (if dir
	(consult-grep default-directory)
      (consult-grep t)))
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

(use-package embark
  :after (vertico)
  :ensure t
  :demand t
  :commands (embark-act-quit forward-button-click+ backward-button-click+ embark-collect-delete+)
  :bind (("C-." . embark-act)
   	 :map minibuffer-mode-map
   	      ("TAB" . minibuffer-force-complete)
   	      ("SPC" . self-insert-command)
   	      ("C-." . embark-act-quit)
   	      ("M-." . embark-act)
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
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char 1)
      (when (equal (line-number-at-pos (point-max))
                   (line-number-at-pos (point)))
        (previous-line))
      (previous-line)
      (next-line))))


(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless))
  (setq orderless-component-separator 'orderless-escapable-split-on-space)

  (defun orderless-flex-if-twiddle-dispatcher+ (pattern _index _total)
    "match characters in order but not necessarily consecutively"
    (cond
     ((equal "~" pattern)
      '(orderless-literal . "~"))
     ((string-prefix-p "$~" pattern)
      `(orderless-flex . ,(substring pattern 2)))))

  (defun orderless-literal-dispatcher+ (pattern _index _total)
    "match literally"
    (cond
     ((equal "=" pattern)
      '(orderless-literal . "="))
     ((string-prefix-p "$=" pattern)
      `(orderless-literal . ,(substring pattern 2)))))

  (defun orderless-initialism-dispatcher+ (pattern _index _total)
    "match characters as initials of words"
    (cond
     ((equal "," pattern)
      '(orderless-literal . ","))
     ((string-prefix-p "$," pattern)
      `(orderless-strict-initialism . ,(substring pattern 2)))))

  (defun orderless-without-dispatcher+ (pattern _index _total)
    "remove matches"
    (cond
     ((equal "!" pattern)
      '(orderless-literal . "!"))
     ((string-prefix-p "$!" pattern)
      `(orderless-without-literal . ,(substring pattern 2)))))

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

;; TODO: evil-show-registers defaults to motion-state
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

  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (setq evil-emacs-state-cursor    '("black" box))
  (setq evil-insert-state-cursor   '("Royal Blue" (bar . 2)))
  (setq evil-normal-state-cursor   '("Royal Blue" box))
  (setq evil-operator-state-cursor '("Royal Blue" (hbar . 2)))
  (evil-mode 1))

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
              ("g c" . magit-clone)))

(use-package avy
  :ensure t
  :config
  (setq avy-keys (list ?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package dired
  :commands (find-grep-dired-default-dir)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map evil-leader-state-map-extension
	      ("s F" . find-grep-dired)
 	      ("s f" . find-grep-dired-default-dir)
  	 :map dired-mode-map
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
  :bind (:map evil-leader-state-map-extension
	 ("t t" . ace-window))
  :config
  (setq aw-dispatch-alist
   	'((?k aw-delete-window "Delete Window")
   	  (?m aw-move-window "Move Window")
   	  (?M aw-swap-window "Swap Windows")
   	  (?c aw-copy-window "Copy Window")
   	  (?b aw-switch-buffer-in-window "Select Buffer")
   	  (?l aw-flip-window)
   	  (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
   	  (?= aw-split-window-fair "Split Fair Window")
   	  (?- aw-split-window-vert "Split Vert Window")
   	  (?| aw-split-window-horz "Split Horz Window")
   	  (?o delete-other-windows "Delete Other Windows")
   	  (?? aw-show-dispatch-help)))
  (setq aw-keys '(?u ?h ?e ?t))
  (setq aw-dispatch-always t))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode))

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
  :config
  (evil-define-key '(normal) python-mode-map
    (kbd "<tab>") 'python-indent-shift-right)
  (evil-define-key '(normal) python-mode-map
    (kbd "<S-tab>") 'python-indent-shift-left))

(use-package pyvenv
  :ensure t)

(use-package f
  :ensure t
  :demand t)

(use-package org
  :ensure nil
  :commands (open-log-file+)
  :bind (:map org-mode-map
	      ("M-n"  . org-next-visible-heading)
	      ("M-p"  . org-previous-visible-heading)
         :map evil-leader-state-map-extension
              ("o l" . open-log-file+))
  :config
  (setq org-startup-folded 'showeverything)
  (setq org-property-format  "%-12s %s")
  (setq org-image-actual-width nil)
  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?Z)
  (setq org-hide-leading-stars nil)
  (setq org-log-into-drawer t)
  (setq org-fontify-whole-heading-line t)
  
  (set-face-attribute 'org-level-1 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-2 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-3 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-4 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-5 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-6 nil :background "#F0F0F0" :overline t :underline t :bold t)
  (set-face-attribute 'org-level-7 nil :background "#F0F0F0" :overline t :underline t :bold t)

  (setq org-log-file (format "%s/daily-log.org" (expand-file-name "~")))

  (defun open-log-file+ ()
    (interactive)
    (find-file org-log-file)))

  (defun custom-set-icons (&rest args) (ignore)) ;;; needed for bug when loading custom.el
                                                 ;;; custom-set-icons isn't defined for some reason
  (setq custom-file (concat (expand-file-name user-emacs-directory) "custom.el"))
  (when (not (file-exists-p custom-file))
    (shell-command (concat "touch " custom-file)))
  (load-file custom-file)

(setq debug-on-error nil)

(defun display-startup-echo-area-message ()
  (let ((seconds (progn (string-match "[[:digit:]]+\\.[[:digit:]]\\{2\\}" (emacs-init-time)) (match-string 0 (emacs-init-time)))))
    (message (format "Emacs started in %s seconds." seconds))))

