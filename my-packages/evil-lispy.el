(require 'evil)
(require 'lispy)

;; TODO: update lighter buffer locally as well
;; TODO: create keybindings that i prefer for the various commands
(defun evil-lispy-state+ ()
  (interactive)
  (unless lispy-mode (lispy-mode))
  (unless (boundp 'evil-emacs-state-cursor-original)
    (setq-local evil-emacs-state-cursor-original evil-emacs-state-cursor))
  (setq-local evil-emacs-state-cursor evil-lispy-state-cursor)
  (define-key evil-emacs-state-local-map (kbd "M-t") 'evil-force-normal-state-from-lispy-state)
  (define-key evil-emacs-state-local-map (kbd "s-t") 'evil-force-normal-state-from-lispy-state)
  (evil-emacs-state))

(defun reset-emacs-state (orig-fun &rest args)
  (advice-remove 'evil-force-normal-state #'reset-emacs-state)
  (remove-function (buffer-local-value 'some-function (current-buffer)) #'my-local-advice)
  (apply orig-fun args)
  (message "This advice ran once and is now gone!"))

(defun evil-force-normal-state-from-lispy-state ()
  (interactive)
  (if lispy-mode (lispy-mode -1))
  (setq-local evil-emacs-state-cursor evil-emacs-state-cursor-original)
  (evil-force-normal-state))



(setq lispy-mode-map
      (let ((map (make-sparse-keymap)))
        ;; navigation
        (lispy-define-key map "l" 'lispy-right)
        (lispy-define-key map "h" 'lispy-left)
        (lispy-define-key map "f" 'lispy-flow)
        (lispy-define-key map "n" 'lispy-down)
        (lispy-define-key map "p" 'lispy-up)
        (lispy-define-key map "V" 'lispy-different)
        ;;(lispy-define-key map "o" 'lispy-other-mode)
        ;; (lispy-define-key map "p" 'lispy-eval-other-window)
        (lispy-define-key map "y" 'lispy-paste)
        (lispy-define-key map "o" 'lispy-occur)
        ;; (lispy-define-key map "z" 'lh-knight/body)
        ;; outline
        ;; (lispy-define-key map "N" 'lispy-outline-next)
        ;; (lispy-define-key map "P" 'lispy-outline-prev)
        (lispy-define-key map "L" 'lispy-outline-goto-child)
        ;; Paredit transformations
        (lispy-define-key map ">" 'lispy-slurp)
        (lispy-define-key map "<" 'lispy-barf)
        (lispy-define-key map "/" 'lispy-splice)
        (lispy-define-key map "r" 'lispy-raise)
        (lispy-define-key map "R" 'lispy-raise-some)
        (lispy-define-key map "j" 'lispy-join)
        ;; more transformations
        (lispy-define-key map "C" 'lispy-convolute)
        (lispy-define-key map "X" 'lispy-convolute-left)
        (lispy-define-key map "P" 'lispy-move-up)
        (lispy-define-key map "N" 'lispy-move-down)
        (lispy-define-key map "J" 'lispy-oneline)
        ;; (lispy-define-key map "M" 'lispy-alt-multiline)
        (lispy-define-key map "S" 'lispy-stringify)
        ;; marking
        (lispy-define-key map "a" 'lispy-ace-symbol
          :override '(cond ((looking-at lispy-outline)
                            (lispy-meta-return))))
        (lispy-define-key map "H" 'lispy-ace-symbol-replace)
        (lispy-define-key map "m" 'lispy-mark-list)
        ;; dialect-specific
        (lispy-define-key map "e" 'lispy-eval)
        (lispy-define-key map "E" 'lispy-eval-and-insert)
        (lispy-define-key map "g" 'lispy-goto-local)
        (lispy-define-key map "G" 'lispy-goto)
        ;; (lispy-define-key map "F" 'lispy-follow t)
        ;; (lispy-define-key map "D" 'pop-tag-mark)
        (lispy-define-key map "A" 'lispy-beginning-of-defun)
        (lispy-define-key map "_" 'lispy-underscore)
        ;; miscellanea
        (define-key map (kbd "SPC") 'lispy-space)
        (lispy-define-key map "z" 'indent-region)
        (lispy-define-key map "TAB" 'lispy-tab)
        (lispy-define-key map "<backtab>" 'lispy-shifttab)
        ;; (lispy-define-key map "N" 'lispy-narrow)
        ;; (lispy-define-key map "W" 'lispy-widen)
        (lispy-define-key map "c" 'lispy-clone)
        (lispy-define-key map "u" 'lispy-undo)
        ;; (lispy-define-key map "q" 'lispy-ace-paren
        ;;   :override '(cond ((bound-and-true-p view-mode)
        ;;                     (View-quit))))
        ;; (lispy-define-key map "Q" 'lispy-ace-char)
        ;; (lispy-define-key map "v" 'lispy-view)
        ;; (lispy-define-key map "t" 'lispy-teleport
        ;;   :override '(cond ((looking-at lispy-outline)
        ;;                     (end-of-line))))
        ;; (lispy-define-key map "n" 'lispy-new-copy)
        (lispy-define-key map "b" 'lispy-back)
        ;; (lispy-define-key map "B" 'lispy-ediff-regions)
        ;; (lispy-define-key map "x" 'lispy-x)
        ;; (lispy-define-key map "Z" 'lispy-edebug-stop)
        ;; (lispy-define-key map "V" 'lispy-visit)
        ;; (lispy-define-key map "-" 'lispy-ace-subword)
        (lispy-define-key map "." 'lispy-repeat)
        ;; (lispy-define-key map "~" 'lispy-tilde)
        ;; digit argument
        (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
              (number-sequence 0 9))
        map))

(setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map)

(define-key evil-normal-state-map (kbd "- r") 'lispy-raise)
(define-key evil-visual-state-map (kbd "- r") 'lispy-raise)
(define-key evil-normal-state-map (kbd "- <") 'lispy-slurp-or-barf-left)
(define-key evil-normal-state-map (kbd "- >") 'lispy-slurp-or-barf-right)
(define-key evil-normal-state-map (kbd "- b") 'lispy-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "- s") 'lispy-forward-slurp-sexp)
(define-key evil-motion-state-map (kbd "g l") 'lispy-left)
(define-key evil-motion-state-map (kbd "gap") 'lispy-ace-paren)
(define-key evil-motion-state-map (kbd "gas") 'lispy-ace-symbol)
;; only seems to work from the special position
;; todo: rewrite so that it doesn't mark and it works on the entire defun
(define-key evil-motion-state-map (kbd "gac") 'lispy-ace-char)


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

(with-eval-after-load 'evil
  (evil-define-text-object my-evil-quoted-symbol (count &optional beg end type)
    "Select a symbol including its quote prefix."
    (let ((bounds (my-bounds-of-quoted-symbol-at-point)))
      (when bounds
        (list (car bounds) (cdr bounds)))))
  
  ;; Bind it, e.g., to 'q' for "quoted symbol"
  (define-key evil-inner-text-objects-map "q" 'my-evil-quoted-symbol)
  (define-key evil-outer-text-objects-map "q" 'my-evil-quoted-symbol))

(provide 'evil-lispy)
