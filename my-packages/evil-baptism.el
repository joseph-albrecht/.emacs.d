;; todo
;; - replace ww and anything that uses evil line style pasting
;; - make pasting be in front of cursor
;; - create functions to paste above/below line

(require 'evil)

(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)
(setq evil-search-module 'isearch)

(defun thing-at-pos (pos thing &optional props)
  (save-excursion
    (goto-char pos)
    (thing-at-point thing props)))

;;;my functions

(evil-define-operator evil-delete-to-blackhole (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?\_ yank-handler))

(evil-define-motion evil-forward-symbol (count)
  (if (and evil-want-change-word-to-end
	   (memq evil-this-operator evil-change-commands))
      (evil-forward-symbol-end 1)
    (when (and (thing-at-pos (point) 'symbol t)
	       (thing-at-pos (1+ (point))'symbol t))
      (forward-symbol 1))
    (forward-symbol (or count 1))
    (forward-symbol -1)))

(evil-define-motion evil-jump-item+ (count)
  "Jump to the matching parenthesis."
  (interactive)
  (cond
   ((looking-at "\\s(") (forward-sexp 1))
   ((looking-back "\\s)" 1) (backward-sexp 1))
   (t (forward-sexp 1))))

(evil-define-motion evil-forward-WORD-end+ (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-word-end count t)
  (if (or (equal evil-state 'visual)
          (not (memq evil-this-operator evil-change-commands)))
      (forward-char 1)))

(evil-define-motion evil-find-char+ (count char)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode visual-line-mode))
        (start (point))
        case-fold-search)
    (setq evil-last-find (list #'evil-find-char+ char fwd))
    (unless (prog1
                (search-forward
                 (char-to-string char)
                 (cond (evil-cross-lines nil)
                       ((and fwd visual)
                        (save-excursion
                          (end-of-visual-line)
                          (point)))
                       (fwd (line-end-position))
                       (visual
                        (save-excursion
                          (beginning-of-visual-line)
                          (point)))
                       (t (line-beginning-position)))
                 t count)
              (when (and fwd
                         (not (equal start (point)))
                         (or (memq evil-this-operator evil-change-commands)
                             (eq evil-this-operator #'evil-delete)))
                (backward-char)))
      (user-error "Can't find `%c'" char))))


(evil-define-motion evil-find-char-to+ (count char)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (interactive "<c><C>")
  (let ((start (point))
        adjust-start)
    (unwind-protect
        (progn
          (backward-char -1)
          (setq adjust-start (point))
          (evil-find-char+ count char)
          (setcar evil-last-find #'evil-find-char-to+)))
    (cond ((equal (point) adjust-start) (goto-char start))
          ((equal (point) start) nil)
          ((> (or count 1) 0) (backward-char))
          (t (forward-char)))))

(evil-define-motion evil-forward-symbol-end (count)
  (forward-symbol (or count 1)))

(evil-define-motion evil-backward-symbol (count)
  (forward-symbol (- 0 (or count 1))))

(evil-define-motion evil-forward-sexp (count)
  (if (and evil-want-change-word-to-end
	   (memq evil-this-operator evil-change-commands))
      (evil-forward-sexp-end 1)
    (when (thing-at-point 'sexp)
      (forward-sexp 1))
    (forward-sexp (or count 1))
    (forward-sexp -1)))

(evil-define-motion evil-forward-sexp-end (count)
  (forward-sexp (or count 1)))

(evil-define-motion evil-backward-sexp (count)
  (forward-sexp (- 0 (or count 1))))

(defun evil-insert-new-line+ ()
    (interactive)
    (save-excursion
      (insert "\n")))

(defun evil-paste-before-killring (count &optional register yank-handler)
  (interactive "*P<x>")
  (let ((string (consult--read-from-kill-ring)))
    (kill-new string)
    (call-interactively #'evil-paste-before)))

(defun evil-paste-after-killring (count &optional register yank-handler)
  (interactive "*P<x>")
  (let ((string (consult--read-from-kill-ring)))
    (kill-new string)
    (call-interactively #'evil-paste-after)))

(defun yank-overwrite ()
  (interactive)
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  (yank))

(defun yank-above ()
  (interactive)
  (save-excursion
  (beginning-of-line)
    (let ((start (point)))
      (yank)
      (when (not (string-suffix-p "\n" (current-kill 0)))
        (insert "\n"))
      (indent-region start (point)))))

(defun yank-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert "\n")
    (let ((start (point)))
      (yank)
      (when (string-suffix-p "\n" (current-kill 0))
        (delete-char 1))
      (indent-region start (point)))))

(defun add-region-to-kill-ring ()
  (interactive)
  (when (region-active-p)
    (let ((substring (buffer-substring (region-beginning) (region-end))))
    (kill-new substring)
    (message "Killed: %s" substring))))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))
;;; functions should be added to the evil jump ring

(evil-define-text-object evil-outer-line (count &optional beg end type)
  "Select outer line (including newline)"
  :extend-selection nil
  (evil-range (line-beginning-position)
              (min (point-max) (1+ (line-end-position)))))

(evil-define-text-object evil-inner-line (count &optional beg end type)
  "Select inner line (excluding newline)"
  :extend-selection nil
  (evil-range (line-beginning-position)
              (line-end-position)))


(defun my-bounds-of-quoted-symbol-at-point ()
  "Return bounds of symbol including quote prefixes like ' and #'."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        ;; Check for #' or ' before the symbol
        (when (looking-back "#?'" (- (point) 2))
          (setcar bounds (match-beginning 0)))
        bounds))))

(evil-define-text-object my-evil-quoted-symbol (count &optional beg end type)
  "Select a symbol including its quote prefix."
  (let ((bounds (my-bounds-of-quoted-symbol-at-point)))
    (when bounds
      (list (car bounds) (cdr bounds)))))

(defun evil-inner-line-select ()
  (interactive)
  (funcall-interactively #'evil-visual-state)
  (funcall-interactively #'evil-inner-line)
  (when (< (point) (mark))
    (funcall-interactively #'exchange-point-and-mark)))

(evil-add-command-properties 'isearch-forward :jump t)
(evil-add-command-properties 'isearch-backward :jump t)
(evil-add-command-properties 'query-replace :jump t)
(evil-add-command-properties 'query-replace-regexp :jump t)

;;; Normal state

(setq evil-normal-state-map (make-sparse-keymap))
(setq evil-motion-state-map (make-sparse-keymap))
(setq evil-visual-state-map (make-sparse-keymap))
(setq evil-window-state-map (make-sparse-keymap))
(setq evil-normal-state-map (make-sparse-keymap))
(setq evil-outer-text-objects-map (make-sparse-keymap))
(setq evil-inner-text-objects-map (make-sparse-keymap))
(setq evil-normal-state-map (make-sparse-keymap))
(setq evil-insert-state-map (make-sparse-keymap))
(setq evil-operator-state-map (make-sparse-keymap))
(setq evil-ex-completion-map (make-sparse-keymap))

(define-key evil-normal-state-map (kbd "<escape>") 'evil-force-normal-state)
(define-key evil-normal-state-map "d" 'evil-append)
(define-key evil-normal-state-map "D" 'evil-append-line)
(define-key evil-normal-state-map "c" 'evil-change)
(define-key evil-normal-state-map "C" 'evil-change-line)
(define-key evil-normal-state-map "k" 'evil-delete)
(define-key evil-normal-state-map "K" 'evil-delete-to-blackhole)
(define-key evil-normal-state-map "i" 'evil-insert)
(define-key evil-normal-state-map (kbd "<insert>") 'evil-insert)
(define-key evil-normal-state-map (kbd "<insertchar>") 'evil-insert)
(define-key evil-normal-state-map "I" 'evil-insert-line)
(define-key evil-normal-state-map "M" 'evil-set-marker)
(define-key evil-normal-state-map "o" 'evil-open-below)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "y" 'yank-overwrite)
(define-key evil-normal-state-map (kbd "M-y") 'yank-pop)
(define-key evil-normal-state-map (kbd "Y") nil)
(define-key evil-normal-state-map (kbd "Y p") 'yank-above)
(define-key evil-normal-state-map (kbd "Y n") 'yank-below)
(define-key evil-normal-state-map (kbd "Y d") 'duplicate-line)
(define-key evil-normal-state-map (kbd "Y Y") 'yank-pop)
(define-key evil-normal-state-map "Q" 'evil-record-macro)
(define-key evil-normal-state-map "q" 'evil-execute-macro)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)
(define-key evil-normal-state-map [deletechar] 'evil-delete-char)
(define-key evil-normal-state-map "w" 'evil-yank)
(define-key evil-normal-state-map "W" 'add-region-to-kill-ring)
(define-key evil-normal-state-map "j" 'evil-join)
(define-key evil-normal-state-map "J" 'evil-join-whitespace)
(define-key evil-normal-state-map "gi" 'evil-insert-resume)
(define-key evil-normal-state-map "g;"  #'comment-dwim)
(define-key evil-normal-state-map "." 'evil-repeat)
(define-key evil-normal-state-map "\"" 'evil-use-register)
(define-key evil-normal-state-map "z" 'indent-region)
(define-key evil-normal-state-map "<" 'evil-shift-left)
(define-key evil-normal-state-map ">" 'evil-shift-right)
(define-key evil-normal-state-map (kbd "DEL") 'evil-backward-char)
(define-key evil-normal-state-map (kbd "S-<return>") 'evil-insert-new-line+)
(define-key evil-normal-state-map (kbd "M-/") 'evil-search-next)
(define-key evil-normal-state-map (kbd "M-?") 'evil-search-previous)
(define-key evil-normal-state-map (kbd "x") 'evil-delete-char)

;; undo
(define-key evil-normal-state-map "u" 'evil-undo)
(define-key evil-normal-state-map "U" 'evil-redo)

;;; Motion state
;; "0" is a special command when called first
(define-key evil-motion-state-map (kbd "s-t") 'evil-force-normal-state)
(define-key evil-motion-state-map (kbd "M-t") 'evil-force-normal-state)
(define-key evil-motion-state-map "a" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "A" 'evil-first-non-blank)
(define-key evil-motion-state-map "1" 'digit-argument)
(define-key evil-motion-state-map "2" 'digit-argument)
(define-key evil-motion-state-map "3" 'digit-argument)
(define-key evil-motion-state-map "4" 'digit-argument)
(define-key evil-motion-state-map "5" 'digit-argument)
(define-key evil-motion-state-map "6" 'digit-argument)
(define-key evil-motion-state-map "7" 'digit-argument)
(define-key evil-motion-state-map "8" 'digit-argument)
(define-key evil-motion-state-map "9" 'digit-argument)
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map (kbd "M-b") 'evil-backward-symbol)
(define-key evil-motion-state-map (kbd "C-M-b") 'evil-backward-sexp)
(define-key evil-motion-state-map (kbd "E") 'evil-forward-WORD-end+)
(define-key evil-motion-state-map (kbd "M-e") 'evil-forward-symbol-end)
(define-key evil-motion-state-map (kbd "C-M-e") 'evil-forward-sexp-end)
(define-key evil-motion-state-map "s" 'evil-find-char+)
(define-key evil-motion-state-map "S" 'evil-find-char-backward)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "n" 'evil-next-visual-line)
(define-key evil-motion-state-map "p" 'evil-previous-visual-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-normal-state-map (kbd "M-/") 'evil-search-previous)
(define-key evil-normal-state-map (kbd "M-?") 'evil-search-next)
(define-key evil-motion-state-map "t" 'evil-find-char-to+)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "f" 'evil-forward-word-begin)
(define-key evil-motion-state-map "F" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map (kbd "M-f") 'evil-forward-symbol)
(define-key evil-motion-state-map (kbd "C-M-f") 'evil-forward-sexp)
(define-key evil-motion-state-map "y" 'yank-overwrite)
(define-key evil-motion-state-map "Y" 'yank-pop)
(define-key evil-motion-state-map (kbd "M-y p") 'yank-above)
(define-key evil-motion-state-map (kbd "M-y n") 'yank-below)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)
(define-key evil-motion-state-map "gr" 'xref-find-references)
(define-key evil-motion-state-map "gg" 'beginning-of-buffer)
(define-key evil-motion-state-map "gq" 'fill-region)
(define-key evil-motion-state-map "gb" 'beginning-of-defun)
(define-key evil-motion-state-map "G" 'end-of-buffer)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "e" 'move-end-of-line)
(define-key evil-motion-state-map "," 'evil-jump-item+)
(define-key evil-motion-state-map "m" 'evil-goto-mark)
(define-key evil-motion-state-map (kbd "M-m") 'evil-goto-mark-line)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map ">" 'evil-search-forward)
(define-key evil-motion-state-map "<" 'evil-search-backward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "M-o") 'evil-jump-forward)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map (kbd "C-M-v") 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)
(define-key evil-motion-state-map (kbd "{") 'evil-backward-paragraph)
(define-key evil-motion-state-map (kbd "}") 'evil-forward-paragraph)

;; ignore keys
(define-key evil-motion-state-map (kbd "]") 'ignore)
(define-key evil-motion-state-map (kbd "[") 'ignore)
(define-key evil-motion-state-map (kbd "(") 'ignore)
(define-key evil-motion-state-map (kbd ")") 'ignore)
(define-key evil-motion-state-map (kbd "^") 'ignore)
(define-key evil-motion-state-map (kbd "$") 'ignore)
(define-key evil-motion-state-map (kbd "+") 'ignore)
(define-key evil-motion-state-map (kbd "%") 'ignore)
(define-key evil-motion-state-map (kbd "-") 'ignore)
(define-key evil-motion-state-map (kbd "&") 'ignore)
(define-key evil-motion-state-map "V" 'evil-inner-line-select)

;; text objects
(define-key evil-outer-text-objects-map "f" 'evil-a-word)
(define-key evil-outer-text-objects-map "F" 'evil-a-WORD)
(define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
(define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)
(define-key evil-outer-text-objects-map "b" 'evil-a-paren)
(define-key evil-outer-text-objects-map "(" 'evil-a-paren)
(define-key evil-outer-text-objects-map ")" 'evil-a-paren)
(define-key evil-outer-text-objects-map "[" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "]" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "B" 'evil-a-curly)
(define-key evil-outer-text-objects-map "{" 'evil-a-curly)
(define-key evil-outer-text-objects-map "}" 'evil-a-curly)
(define-key evil-outer-text-objects-map "<" 'evil-an-angle)
(define-key evil-outer-text-objects-map ">" 'evil-an-angle)
(define-key evil-outer-text-objects-map "'" 'evil-a-single-quote)
(define-key evil-outer-text-objects-map "\"" 'evil-a-double-quote)
(define-key evil-outer-text-objects-map "`" 'evil-a-back-quote)
(define-key evil-outer-text-objects-map "t" 'evil-a-tag)
(define-key evil-outer-text-objects-map "o" 'evil-a-symbol)
(define-key evil-outer-text-objects-map "l" 'evil-outer-line)
(define-key evil-outer-text-objects-map "q" 'my-evil-quoted-symbol)
(define-key evil-inner-text-objects-map "q" 'my-evil-quoted-symbol)
(define-key evil-inner-text-objects-map "f" 'evil-inner-word)
(define-key evil-inner-text-objects-map "F" 'evil-inner-WORD)
(define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
(define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
(define-key evil-inner-text-objects-map "b" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "(" 'evil-inner-paren)
(define-key evil-inner-text-objects-map ")" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "[" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "]" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "B" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "{" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "}" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "<" 'evil-inner-angle)
(define-key evil-inner-text-objects-map ">" 'evil-inner-angle)
(define-key evil-inner-text-objects-map "'" 'evil-inner-single-quote)
(define-key evil-inner-text-objects-map "\"" 'evil-inner-double-quote)
(define-key evil-inner-text-objects-map "`" 'evil-inner-back-quote)
(define-key evil-inner-text-objects-map "t" 'evil-inner-tag)
(define-key evil-inner-text-objects-map "o" 'evil-inner-symbol)
(define-key evil-inner-text-objects-map "l" 'evil-inner-line)

;;; Visual state

(define-key evil-visual-state-map "d" 'evil-append)
(define-key evil-visual-state-map "I" 'evil-insert)
(define-key evil-visual-state-map "V" 'exchange-point-and-mark)
(define-key evil-visual-state-map "R" 'evil-change)
(define-key evil-visual-state-map "u" 'evil-downcase)
(define-key evil-visual-state-map "U" 'evil-upcase)
(define-key evil-visual-state-map "d" evil-outer-text-objects-map)
(define-key evil-visual-state-map "i" evil-inner-text-objects-map)
(define-key evil-visual-state-map (kbd "<insert>") 'undefined)
(define-key evil-visual-state-map (kbd "<insertchar>") 'undefined)
(define-key evil-visual-state-map [remap evil-repeat] 'undefined)
(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)

;;; Operator-Pending state

(define-key evil-operator-state-map "d" evil-outer-text-objects-map)
(define-key evil-operator-state-map "i" evil-inner-text-objects-map)
(define-key evil-operator-shortcut-map "w" 'ignore)
(define-key evil-operator-state-map (kbd "s-t") 'evil-force-normal-state)
(define-key evil-operator-state-map (kbd "M-t") 'evil-force-normal-state)

;;; Insert state
(define-key evil-insert-state-map [escape] nil)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-force-normal-state)
(define-key evil-insert-state-map (kbd "S-<return>") 'evil-insert-new-line+)
(global-set-key (kbd "s-t") nil)
(global-set-key (kbd "M-t") nil)
(define-key evil-insert-state-map (kbd "s-t") 'evil-force-normal-state)
(define-key evil-insert-state-map (kbd "M-t") 'evil-force-normal-state)
(define-key evil-emacs-state-map (kbd "s-t") 'evil-force-normal-state)
(define-key evil-emacs-state-map (kbd "M-t") 'evil-force-normal-state)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)
(define-key evil-replace-state-map (kbd "s-t") 'evil-force-normal-state)
(define-key evil-replace-state-map (kbd "M-t") 'evil-force-normal-state)

;;; Emacs state

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map
  (read-kbd-macro evil-toggle-key) 'evil-exit-emacs-state)

;;; Mouse
(define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)
(define-key evil-visual-state-map [mouse-2] 'evil-exit-visual-and-repeat)
(define-key evil-normal-state-map [mouse-2] 'mouse-yank-primary)

(defun skip-dash-backward (n &rest foo)
  (if (eq (char-before (point)) ?-)
      (backward-char))
  (ignore))

(defun skip-dash-forward (n &rest foo)
  (if (eq (char-after (point)) ?-)
      (forward-char))
  (ignore))

(defun skip-dash-forward-end (n &rest foo)
  (if (eq (char-after (+ 1 (point))) ?-)
      (forward-char))
  (ignore))

(advice-add 'evil-forward-word-begin :after #'skip-dash-forward)
(advice-add 'evil-forward-word-end :before #'skip-dash-forward-end)
(advice-add 'evil-backward-word-begin :before #'skip-dash-backward)

;;; fix returning to insert state
(evil-define-key 'normal dired-mode-map (kbd "i") 'evil-insert-state)

(evil-define-state leader
  "Leader state."
  :tag " <L> "
  :message "-- LEADER --"
  :input-method t
  :intercept-esc nil)

(provide 'evil-baptism)
