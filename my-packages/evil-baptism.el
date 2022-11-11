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


;;; functions should be added to the evil jump ring

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
(define-key evil-normal-state-map "0" 'evil-join)
(define-key evil-normal-state-map "M" 'evil-set-marker)

(define-key evil-normal-state-map "o" 'evil-open-below)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "y" 'evil-paste-after)
(define-key evil-normal-state-map "Y" 'evil-paste-before)
(define-key evil-normal-state-map "Q" 'evil-record-macro)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)
(define-key evil-normal-state-map "x" 'evil-delete-char)
(define-key evil-normal-state-map "X" 'evil-delete-backward-char)
(define-key evil-normal-state-map [deletechar] 'evil-delete-char)
(define-key evil-normal-state-map "w" 'evil-yank)
(define-key evil-normal-state-map "W" 'evil-yank-line)
(define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute)
(define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
(define-key evil-normal-state-map "g8" 'what-cursor-position)
(define-key evil-normal-state-map "ga" 'what-cursor-position)
(define-key evil-normal-state-map "gi" 'evil-insert-resume)
(define-key evil-normal-state-map "gJ" 'evil-join-whitespace)
(define-key evil-normal-state-map "gq" 'evil-fill-and-move)
(define-key evil-normal-state-map "gw" 'evil-fill)
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)
(define-key evil-normal-state-map "gf" 'find-file-at-point)
(define-key evil-normal-state-map "gF" 'evil-find-file-at-point-with-line)
(define-key evil-normal-state-map "g;"  #'comment-dwim)
(define-key evil-normal-state-map "gx" 'browse-url-at-point)
(define-key evil-normal-state-map "g/" 'next-error)
(define-key evil-normal-state-map "g?" 'previous-error)
(define-key evil-normal-state-map "g~" 'evil-invert-case)
(define-key evil-normal-state-map "zo" 'evil-open-fold)
(define-key evil-normal-state-map "zO" 'evil-open-fold-rec)
(define-key evil-normal-state-map "zc" 'evil-close-fold)
(define-key evil-normal-state-map "za" 'evil-toggle-fold)
(define-key evil-normal-state-map "zr" 'evil-open-folds)
(define-key evil-normal-state-map "zm" 'evil-close-folds)
(define-key evil-normal-state-map "z=" 'ispell-word)
(define-key evil-normal-state-map "\M-y" 'evil-paste-pop)
(define-key evil-normal-state-map "\M-S-y" 'evil-paste-pop-next)
(define-key evil-normal-state-map "." 'evil-repeat)
(define-key evil-normal-state-map "q" 'evil-execute-macro)
(define-key evil-normal-state-map "\"" 'evil-use-register)
(define-key evil-normal-state-map "~" 'evil-invert-char)
(define-key evil-normal-state-map "=" 'indent-region)
(define-key evil-normal-state-map "<" 'evil-shift-left)
(define-key evil-normal-state-map ">" 'evil-shift-right)
(define-key evil-normal-state-map "j" 'evil-search-next)
(define-key evil-normal-state-map "J" 'evil-search-previous)
(define-key evil-normal-state-map (kbd "DEL") 'evil-backward-char)
(define-key evil-normal-state-map [escape] 'evil-force-normal-state)
(define-key evil-normal-state-map [remap cua-paste-pop] 'evil-paste-pop)
(define-key evil-normal-state-map [remap yank-pop] 'evil-paste-pop)
(define-key evil-normal-state-map (kbd "S-<return>") 'evil-insert-new-line+)

;; go to last change
(define-key evil-normal-state-map "g." 'goto-last-change)
(define-key evil-normal-state-map "g," 'goto-last-change-reverse)

;; undo
(define-key evil-normal-state-map "u" 'evil-undo)
(define-key evil-normal-state-map "U" 'evil-redo)

;; window commands
(define-prefix-command 'evil-window-map)
(define-key evil-window-map "b" 'evil-window-bottom-right)
(define-key evil-window-map "c" 'evil-window-delete)
(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "H" 'evil-window-move-far-left)
(define-key evil-window-map "j" 'evil-window-down)
(define-key evil-window-map "J" 'evil-window-move-very-bottom)
(define-key evil-window-map "k" 'evil-window-up)
(define-key evil-window-map "K" 'evil-window-move-very-top)
(define-key evil-window-map "l" 'evil-window-right)
(define-key evil-window-map "L" 'evil-window-move-far-right)
(define-key evil-window-map "n" 'evil-window-new)
(define-key evil-window-map "o" 'delete-other-windows)
(define-key evil-window-map "p" 'evil-window-mru)
(define-key evil-window-map "q" 'evil-quit)
(define-key evil-window-map "r" 'evil-window-rotate-downwards)
(define-key evil-window-map "R" 'evil-window-rotate-upwards)
(define-key evil-window-map "s" 'evil-window-split)
(define-key evil-window-map "S" 'evil-window-split)
(define-key evil-window-map "t" 'evil-window-top-left)
(define-key evil-window-map "v" 'evil-window-vsplit)
(define-key evil-window-map "w" 'evil-window-next)
(define-key evil-window-map "W" 'evil-window-prev)
(define-key evil-window-map "+" 'evil-window-increase-height)
(define-key evil-window-map "-" 'evil-window-decrease-height)
(define-key evil-window-map "_" 'evil-window-set-height)
(define-key evil-window-map "<" 'evil-window-decrease-width)
(define-key evil-window-map ">" 'evil-window-increase-width)
(define-key evil-window-map "=" 'balance-windows)
(define-key evil-window-map "|" 'evil-window-set-width)
(define-key evil-window-map "\C-b" 'evil-window-bottom-right)
(define-key evil-window-map "\C-c" 'evil-window-delete)
(define-key evil-window-map (kbd "C-S-h") 'evil-window-move-far-left)
(define-key evil-window-map (kbd "C-S-j") 'evil-window-move-very-bottom)
(define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-top)
(define-key evil-window-map (kbd "C-S-l") 'evil-window-move-far-right)
(define-key evil-window-map "\C-n" 'evil-window-new)
(define-key evil-window-map "\C-o" 'delete-other-windows)
(define-key evil-window-map "\C-p" 'evil-window-mru)
(define-key evil-window-map "\C-r" 'evil-window-rotate-downwards)
(define-key evil-window-map (kbd "C-S-r") 'evil-window-rotate-upwards)
(define-key evil-window-map "\C-s" 'evil-window-split)
(define-key evil-window-map (kbd "C-S-s") 'evil-window-split)
(define-key evil-window-map "\C-t" 'evil-window-top-left)
(define-key evil-window-map "\C-v" 'evil-window-vsplit)
(define-key evil-window-map "\C-w" 'evil-window-next)
(define-key evil-window-map (kbd "C-S-W") 'evil-window-prev)
(define-key evil-window-map "\C-_" 'evil-window-set-height)
(define-key evil-window-map "\C-f" 'ffap-other-window)

;;; Motion state

;; "0" is a special command when called first
(define-key evil-motion-state-map "a" 'evil-beginning-of-visual-line)
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
(define-key evil-motion-state-map (kbd "A") 'evil-window-middle)
(define-key evil-motion-state-map (kbd "E") 'evil-forward-WORD-end)
(define-key evil-motion-state-map (kbd "M-e") 'evil-forward-symbol-end)
(define-key evil-motion-state-map (kbd "C-M-e") 'evil-forward-sexp-end)
(define-key evil-motion-state-map "s" 'evil-find-char)
(define-key evil-motion-state-map "S" 'evil-find-char-backward)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "n" 'evil-next-visual-line)
(define-key evil-motion-state-map "p" 'evil-previous-visual-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "$" 'evil-lookup)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
;;(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "j" 'evil-search-next)
(define-key evil-motion-state-map "J" 'evil-search-previous)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "f" 'evil-forward-word-begin)
(define-key evil-motion-state-map "F" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map (kbd "M-f") 'evil-forward-symbol)
(define-key evil-motion-state-map (kbd "C-M-f") 'evil-forward-sexp)
(define-key evil-motion-state-map "y" 'evil-paste-after)
(define-key evil-motion-state-map "Y" 'evil-paste-before)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)
(define-key evil-motion-state-map "gD" 'xref-find-definitions-other-window)
(define-key evil-motion-state-map "gr" 'xref-find-references)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "g\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward)
(define-key evil-motion-state-map "e" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "," 'evil-jump-item)
(define-key evil-motion-state-map "m" 'evil-goto-mark)
(define-key evil-motion-state-map (kbd "M-m") 'evil-goto-mark-line)
(define-key evil-motion-state-map "(" 'evil-backward-sentence-begin)
(define-key evil-motion-state-map ")" 'evil-forward-sentence-begin)
(define-key evil-motion-state-map "][" 'evil-forward-section-begin)
(define-key evil-motion-state-map "]]" 'evil-forward-section-end)
(define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
(define-key evil-motion-state-map "[]" 'evil-backward-section-end)
(define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
(define-key evil-motion-state-map "])" 'evil-next-close-paren)
(define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
(define-key evil-motion-state-map "]}" 'evil-next-close-brace)
(define-key evil-motion-state-map "]s" 'evil-next-flyspell-error)
(define-key evil-motion-state-map "[s" 'evil-prev-flyspell-error)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
(define-key evil-motion-state-map "+" 'evil-next-line-first-non-blank)
(define-key evil-motion-state-map "_" 'evil-next-line-1-first-non-blank)
(define-key evil-motion-state-map "-" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map "|" 'evil-goto-column)
(define-key evil-motion-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "%" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "\C-w" 'evil-window-map)
(define-key evil-motion-state-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
(define-key evil-motion-state-map "\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "M-o") 'evil-jump-forward)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map "\\" 'evil-execute-in-emacs-state)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))
(define-key evil-motion-state-map "zz" 'evil-scroll-line-to-center)
(define-key evil-motion-state-map "z." "zz^")
(define-key evil-motion-state-map "zt" 'evil-scroll-line-to-top)
(define-key evil-motion-state-map "zb" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "z-" "zb^")
(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map "V" 'evil-visual-line)
(define-key evil-motion-state-map (kbd "C-M-v") 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)
(define-key evil-motion-state-map "zl" 'evil-scroll-column-right)
(define-key evil-motion-state-map [?z right] "zl")
(define-key evil-motion-state-map "zh" 'evil-scroll-column-left)
(define-key evil-motion-state-map [?z left] "zh")
(define-key evil-motion-state-map "zL" 'evil-scroll-right)
(define-key evil-motion-state-map "zH" 'evil-scroll-left)
;; (define-key evil-motion-state-map
;;   (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(define-key evil-motion-state-map "gp" 'evil-previous-line)
(define-key evil-motion-state-map "gn" 'evil-next-line)
(define-key evil-motion-state-map (kbd "C-^") 'backward-up-list)
(define-key evil-motion-state-map (kbd "gl") 'evil-avy-goto-line)
(define-key evil-motion-state-map (kbd "gC") 'evil-avy-goto-char)
(define-key evil-motion-state-map (kbd "gc") 'evil-avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "{") 'evil-backward-paragraph)
(define-key evil-motion-state-map (kbd "}") 'evil-forward-paragraph)

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

;;; Visual state

(define-key evil-visual-state-map "d" 'evil-append)
(define-key evil-visual-state-map "I" 'evil-insert)
(define-key evil-visual-state-map "j" 'exchange-point-and-mark)
(define-key evil-visual-state-map "J" 'evil-visual-exchange-corners)
(define-key evil-visual-state-map "R" 'evil-change)
(define-key evil-visual-state-map "u" 'evil-downcase)
(define-key evil-visual-state-map "U" 'evil-upcase)
(define-key evil-visual-state-map "z=" 'ispell-word)
(define-key evil-visual-state-map "d" evil-outer-text-objects-map)
(define-key evil-visual-state-map "i" evil-inner-text-objects-map)
(define-key evil-visual-state-map (kbd "<insert>") 'undefined)
(define-key evil-visual-state-map (kbd "<insertchar>") 'undefined)
(define-key evil-visual-state-map [remap evil-repeat] 'undefined)
(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)

;;; Operator-Pending state

(define-key evil-operator-state-map "d" evil-outer-text-objects-map)
(define-key evil-operator-state-map "i" evil-inner-text-objects-map)
;; (define-key evil-operator-state-map [escape] 'keyboard-quit)

;;; Insert state
(define-key evil-insert-state-map [escape] nil)
(define-key evil-insert-state-map (kbd "S-<return>") 'evil-insert-new-line+)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Emacs state

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map
  (read-kbd-macro evil-toggle-key) 'evil-exit-emacs-state)

(when evil-want-C-w-in-emacs-state
  (define-key evil-emacs-state-map "\C-w" 'evil-window-map))

;;; Mouse
(define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)
(define-key evil-visual-state-map [mouse-2] 'evil-exit-visual-and-repeat)
(define-key evil-normal-state-map [mouse-2] 'mouse-yank-primary)

;; Ex
(define-key evil-motion-state-map ":" 'evil-ex)
(define-key evil-motion-state-map "!" 'evil-shell-command)

(evil-ex-define-cmd "e[dit]" 'evil-edit)
(evil-ex-define-cmd "w[rite]" 'evil-write)
(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
(evil-ex-define-cmd "sav[eas]" 'evil-save)
(evil-ex-define-cmd "r[ead]" 'evil-read)
(evil-ex-define-cmd "b[uffer]" 'evil-buffer)
(evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
(evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
(evil-ex-define-cmd "bN[ext]" "bprevious")
(evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
(evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
(evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
(evil-ex-define-cmd "sbN[ext]" "sbprevious")
(evil-ex-define-cmd "buffers" 'buffer-menu)
(evil-ex-define-cmd "files" 'evil-show-files)
(evil-ex-define-cmd "ls" "buffers")

(evil-ex-define-cmd "c[hange]" 'evil-change)
(evil-ex-define-cmd "co[py]" 'evil-copy)
(evil-ex-define-cmd "t" "copy")
(evil-ex-define-cmd "m[ove]" 'evil-move)
(evil-ex-define-cmd "d[elete]" 'evil-ex-delete)
(evil-ex-define-cmd "y[ank]" 'evil-ex-yank)
(evil-ex-define-cmd "go[to]" 'evil-goto-char)
(evil-ex-define-cmd "j[oin]" 'evil-ex-join)
(evil-ex-define-cmd "le[ft]" 'evil-align-left)
(evil-ex-define-cmd "ri[ght]" 'evil-align-right)
(evil-ex-define-cmd "ce[nter]" 'evil-align-center)
(evil-ex-define-cmd "sp[lit]" 'evil-window-split)
(evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
(evil-ex-define-cmd "new" 'evil-window-new)
(evil-ex-define-cmd "ene[w]" 'evil-buffer-new)
(evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
(evil-ex-define-cmd "clo[se]" 'evil-window-delete)
(evil-ex-define-cmd "on[ly]" 'delete-other-windows)
(evil-ex-define-cmd "q[uit]" 'evil-quit)
(evil-ex-define-cmd "wq" 'evil-save-and-close)
(evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
(evil-ex-define-cmd "qa[ll]" "quitall")
(evil-ex-define-cmd "cq[uit]" 'evil-quit-all-with-error-code)
(evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
(evil-ex-define-cmd "xa[ll]" "wqall")
(evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
(evil-ex-define-cmd "bw[ipeout]" 'evil-delete-buffer)
(evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
(evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
(evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
(evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
(evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
(evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
(evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
(evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
(evil-ex-define-cmd "registers" 'evil-show-registers)
(evil-ex-define-cmd "marks" 'evil-show-marks)
(evil-ex-define-cmd "delm[arks]" 'evil-delete-marks)
(evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
(evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
(evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
(evil-ex-define-cmd "<" 'evil-shift-left)
(evil-ex-define-cmd ">" 'evil-shift-right)
(evil-ex-define-cmd "=" 'evil-ex-line-number)
(evil-ex-define-cmd "!" 'evil-shell-command)
(evil-ex-define-cmd "@:" 'evil-ex-repeat)
(evil-ex-define-cmd "mak[e]" 'evil-make)
(evil-ex-define-cmd "cc" 'evil-goto-error)
(evil-ex-define-cmd "cfir[st]" 'first-error)
(evil-ex-define-cmd "cr[ewind]" 'first-error)
(evil-ex-define-cmd "cn[ext]" 'next-error)
(evil-ex-define-cmd "cp[revious]" 'previous-error)
(evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
(evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
(evil-ex-define-cmd "sor[t]" 'evil-ex-sort)
(evil-ex-define-cmd "res[ize]" 'evil-ex-resize)
(evil-ex-define-cmd "u[ndo]" 'evil-undo)
(evil-ex-define-cmd "red[o]" 'evil-redo)

(when (featurep 'tab-bar)
  (evil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabn[ext]" 'tab-bar-switch-to-next-tab)
  (evil-ex-define-cmd "tabp[revious]" 'tab-bar-switch-to-prev-tab))

;; search command line
					;(define-key evil-ex-search-keymap (kbd "C-c w") 'evil-ex-search-command-window) ;;not working
(define-key evil-ex-search-keymap "\C-r" 'evil-paste-from-register)
(define-key evil-ex-search-keymap "\C-n" 'next-history-element)
(define-key evil-ex-search-keymap "\C-p" 'previous-history-element)

;; ex command line
(define-key evil-ex-completion-map "\t" #'evil-ex-completion)
(define-key evil-ex-completion-map [tab] #'evil-ex-completion)
(define-key evil-ex-completion-map [remap completion-at-point] #'evil-ex-completion)
(define-key evil-ex-completion-map (kbd "C-c w") 'evil-ex-command-window)
(define-key evil-ex-completion-map "\C-g" 'abort-recursive-edit)
(define-key evil-ex-completion-map (kbd "A-p") #'previous-history-element)
(define-key evil-ex-completion-map (kbd "A-n") #'next-history-element)
(define-key evil-ex-completion-map (kbd "A-r") #'select-from-history)
(define-key evil-ex-completion-map [escape] 'abort-recursive-edit)
(define-key evil-ex-completion-map [return] 'exit-minibuffer)
(define-key evil-ex-completion-map (kbd "RET") 'exit-minibuffer)

(global-set-key (kbd "C-S-v") #'evil-scroll-line-down)
(global-set-key (kbd "M-V") #'evil-scroll-line-up)

;; command line window
(evil-define-key 'normal
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)
(evil-define-key 'insert
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)

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
