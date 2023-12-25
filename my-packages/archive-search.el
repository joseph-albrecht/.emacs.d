;; todos
;; make the search use consult and then an export to archive-search mode
;; make archive-search have a top editable bar and live update like the archive
;; make a consult command to select a #tag present in my note collection

(define-derived-mode archive-search fundamental-mode "Search"
  "Major mode for searching markdown files."
  ;; Define key bindings for this mode
  (let ((map archive-search-map))
    (define-key map (kbd "n")   'my-search-forward-button)
    (define-key map (kbd "p")   'my-search-backward-button)
    (define-key map (kbd "M-n") 'my-search-preview-next-button)
    (define-key map (kbd "M-p") 'my-search-preview-previous-button)))

(defun search-files (query dir)
  "Search recursively in DIR for files containing all words in QUERY."
  (interactive "sSearch query: \nDDirectory: ")
  ;; Ensure grep is available
  (unless (executable-find "grep")
    (error "Grep is not available"))
  ;; Split the query into words
  (let* ((words (split-string query "[ \t\n]+" t))
         ;; Construct the initial grep command with the first word and the directory
         (initial-grep-command (format "grep -irlZ --exclude-dir='.git' '%s' '%s' | sed 's/\\/\\//\\//' | grep '\\.md$' | sort"
                                       (shell-quote-argument (car words))
                                       (expand-file-name dir)))
         ;; Construct the rest of the grep commands for additional words
         (additional-grep-commands (mapconcat (lambda (word)
                                                (format "while read file; do grep -l '%s' \"$file\"; done"
                                                        (shell-quote-argument word)))
                                              (cdr words) " | "))
         ;; Combine the initial command with the rest of the commands
         (combined-grep-command (if (cdr words)
                                   (concat initial-grep-command " | " additional-grep-commands " | sort")
                                   initial-grep-command))
         (results '()))
    ;; Run grep and process output
    
    (with-temp-buffer
     (insert (shell-command-to-string combined-grep-command))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file-path (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (unless (string= file-path "")
            (push (cons (file-name-nondirectory file-path) file-path) results))
        (forward-line 1))))
    results))

(defun my-search-forward-button ()
  "Move to the next button, but don't wrap around."
  (interactive)
  (forward-button 1 nil))

(defun my-search-backward-button ()
  "Move to the previous button, but don't wrap around."
  (interactive)
  (backward-button 1 nil))


(defun my-search-forward-button-preview ()
  "Move to the next button, but don't wrap around."
  (interactive)
  (forward-button 1 nil)
  (my-search-preview-button)
  )

(defun my-search-backward-button-preview ()
  "Move to the previous button, but don't wrap around."
  (interactive)
  (backward-button 1 nil)
  )

(defun my-search-preview-next-button ()
  "Move to the next button and preview the file."
  (interactive)
  (forward-button 1 nil)
  (my-preview-file-at-button))

(defun my-search-preview-previous-button ()
  "Move to the previous button and preview the file."
  (interactive)
  (backward-button 1 nil)
  (my-preview-file-at-button))

(defun my-preview-file-at-button ()
  "Preview the file at the current button without moving the cursor."
  (let ((file-path (button-get (button-at (point)) 'file-path)))
    (when file-path
      (save-selected-window
        (find-file-other-window file-path)))))

(defun my-display-search-results (results query)
  "Display the search results."
  (pop-to-buffer "*Search Results*")
  (setq buffer-read-only nil)
  (erase-buffer)
  (archive-search) ; Switch to your custom major mode
  (insert (format "Search for: %s\n\n" query))
  (dolist (result results)
    (let ((file-name (car result))
          (file-path (cdr result)))
      (let ((button-keymap (let ((map (make-sparse-keymap)))
                             ;; Bind RET to open file in the current window
                             (define-key map (kbd "RET") `(lambda () (interactive) (my-search-follow-button ,file-path)))
                             ;; Bind C-RET to open file in another window, but do not follow
                             (define-key map (kbd "<C-return>") `(lambda () (interactive) (my-search-preview-button ,file-path)))
                             map)))
        (insert-button file-name
                       'action 'button-activate ; Default action
                       'follow-link t
                       'file-path file-path ; Store the file path in the button
                       'help-echo "RET: open file; C-RET: open in other window without moving focus"
                       'keymap button-keymap)
      (insert "\n"))))
  (setq buffer-read-only t))

  (defun archive-search (query)
  "Search for QUERY in markdown files."
  (interactive "sSearch query: ")
  (let ((results (search-files query "/Users/joey/Library/Mobile Documents/iCloud~md~obsidian/Documents/obsidian/")))
    (my-display-search-results results query)))

(defun my-search-results (query)
  "Search for QUERY in markdown files."
  (search-files query "/Users/joey/Library/Mobile Documents/iCloud~md~obsidian/Documents/obsidian/"))


(defun archive-interactive-search ()
  (interactive)
  (let ((vertico-sort-override-function #'identity))
    (find-file (concat "/Users/joey/Library/Mobile Documents/iCloud~md~obsidian/Documents/obsidian/"
                       (consult--read
                        (consult--dynamic-collection
                         (lambda (input)
                           (my-search-results input)))
                        :prompt "Select a note file: "
                        :initial "%")))))

(provide 'archive-search)
