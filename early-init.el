(defface mode-line-indicator-blue-bg
  '((default :inherit (bold prot-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'mode-line-faces)

(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	(and (minibuffer-window-active-p (minibuffer-window))
	     (with-selected-window (minibuffer-window)
	       (eq window (minibuffer-selected-window)))))))

(defvar-local mode-line-kbd-macro+
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'mode-line-indicator-blue-bg)))
    "Mode line construct displaying 'defining-kbd-macro'.")

(put 'mode-line-kbd-macro+ 'risky-local-variable t)

(defconst mode-line-evil-state-tags+
  '((normal     :short "<N>"  :long "NORMAL")
    (insert     :short "<I>"  :long "INSERT")
    (visual     :short "<V>"  :long "VISUAL")
    (vblock     :short "<Vb>" :long "VBLOCK")
    (vline      :short "<Vl>" :long "VLINE")
    (vsline     :short "<Vsl>":long "VSLINE")
    (motion     :short "<M>"  :long "MOTION")
    (emacs      :short "<E>"  :long "EMACS")
    (operator   :short "<O>"  :long "OPERATE")
    (replace    :short "<R>"  :long "REPLACE")
    (leader     :short "<L>"  :long "LEADER"))
  "Short and long tags for Evil states.")

(defvar-local mode-line-evil-mode+
    '(:eval
      (format " %s"(plist-get (alist-get evil-state mode-line-evil-state-tags+) :short)))
    "Mode line construct to display the buffer local evil-state.")

(put 'mode-line-evil-mode+ 'risky-local-variable t)

(defvar-local mode-line-buffer-name+
    '(:eval
      (format " %s" (buffer-name)))
    "Mode line construct to display the buffer name.")

(put 'mode-line-buffer-name+ 'risky-local-variable t)

(defvar-local mode-line-buffer-modified+
    '(:eval 
      (when (and (buffer-modified-p)
                 (buffer-file-name)) " ●"))
    "Mode line construct to display if the buffer has been modified.") 

(put 'mode-line-buffer-modified+ 'risky-local-variable t)

(defvar-local mode-line-major-mode+
    '(:eval
      (format "  𝌭 %s" major-mode))
    "Mode line construct to display the buffer's major mode.")

(put 'mode-line-major-mode+ 'risky-local-variable t)

;; TODO: what kinds of information might I want to know?
;;       modified?
;;       not tracked?
(defvar-local mode-line-git-branch+
    '(:eval
      (when (and (magit-get-current-branch)
                 (buffer-file-name))
        (format "  %s %s" (char-to-string #xE0A0) (magit-get-current-branch))))
    "Mode line construct to display the buffer's git branch.")

(put 'mode-line-git-branch+ 'risky-local-variable t)

(defun mode-line-sesman-system+ ()
  (condition-case nil
    (sesman--system)
  (error nil)))

(defun mode-line-sesman-repls ()
  (cond
   ((equal major-mode 'clojurescript-mode) "CIDER")
   ((equal major-mode 'python-mode) "PYTHON")))

(defvar-local mode-line-sesman+
    '(:eval
      (when (and (mode-line-sesman-system+)
                 (mode-line-sesman-repls)
                 (sesman--linked-sessions (sesman--system)))
        (format "  ⎶ %s" (mode-line-sesman-repls))
        ))
    "Mode line construct to display if the current buffer has a linked sesman session.")

(put 'mode-line-sesman+ 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mode-line-buffer-modified+
                mode-line-kbd-macro+
                mode-line-save-status+
                ;; mode-line-evil-mode+
                mode-line-buffer-name+
                mode-line-major-mode+
                mode-line-git-branch+
                mode-line-sesman+
                ;; mode-line-lsp+
                ;; mode-line-flymake+
                ))

(setq-default mode-line-format
              '("%e"
                mode-line-buffer-modified+
                mode-line-kbd-macro+
                mode-line-save-status+
                ;; mode-line-evil-mode+
                mode-line-buffer-name+
                mode-line-major-mode+
                mode-line-git-branch+
                mode-line-sesman+
                ;; mode-line-lsp+
                ;; mode-line-flymake+
                ))
