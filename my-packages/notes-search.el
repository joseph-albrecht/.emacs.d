;;; notes-search.el --- Fast note searching with Xapian and Consult -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (consult "1.0"))
;; Keywords: notes, search, convenience
;; URL: https://github.com/you/notes-search

;;; Commentary:

;; Fast incremental note searching using Xapian for full-text search
;; and Consult for the minibuffer interface.  Displays note titles
;; from YAML front matter in search results.
;;
;; Usage:
;;   M-x notes-search
;;
;; Configuration:
;;   (setq notes-search-directory "~/notes")
;;
;; On first use, you will be prompted to download the xapian-lite
;; dynamic module.  Pre-built binaries are available for Linux (amd64)
;; and macOS (amd64).  For other platforms, you need to compile
;; xapian-lite manually from https://github.com/casouri/xapian-lite

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'seq)

;;; Dynamic module loading

(defvar notes-search--module-loaded nil
  "Non-nil if the xapian-lite module has been loaded.")

(defvar notes-search--module-load-attempted nil
  "Non-nil if we've already attempted to load/download the module.")

(defvar notes-search--linux-module-url
  "https://github.com/casouri/xapian-lite/releases/download/v2.1.0/xapian-lite-amd64-linux.so"
  "URL for pre-built dynamic module for Linux.")

(defvar notes-search--mac-module-url
  "https://github.com/casouri/xapian-lite/releases/download/v2.1.0/xapian-lite-amd64-macos.dylib"
  "URL for pre-built dynamic module for Mac.")

(defun notes-search--module-dir ()
  "Return the directory where the module should be installed."
  (file-name-directory
   (or (locate-library "notes-search.el" t)
       (locate-library "notes-search" t)
       (expand-file-name "notes-search.el" user-emacs-directory))))

(defun notes-search--module-path ()
  "Return the path where the module should be installed.
We use .so extension on all platforms - macOS supports both .so and .dylib."
  (expand-file-name "xapian-lite.so"
                    (notes-search--module-dir)))

(defun notes-search--download-module ()
  "Download pre-built module from GitHub.
Return non-nil if successful."
  (require 'url)
  (let ((module-path (notes-search--module-path))
        (url (pcase system-type
               ('gnu/linux notes-search--linux-module-url)
               ('darwin notes-search--mac-module-url)
               (_ nil))))
    (if (null url)
        (progn
          (message "No pre-built module for %s. Please compile manually from https://github.com/casouri/xapian-lite" system-type)
          nil)
      (if (not (y-or-n-p (format "Download xapian-lite module to %s? " module-path)))
          (progn
            (message "Module download cancelled. Cannot proceed without xapian-lite.")
            nil)
        (message "Downloading xapian-lite module...")
        (condition-case err
            (progn
              (url-copy-file url module-path t)
              (if (file-exists-p module-path)
                  (progn
                    (message "Downloaded successfully to %s" module-path)
                    t)
                (message "Download appeared to succeed but file not found")
                nil))
          (error
           (message "Download failed: %s" (error-message-string err))
           nil))))))

(defun notes-search--load-module ()
  "Load the xapian-lite dynamic module.
Returns non-nil if successful."
  ;; Already loaded?
  (when notes-search--module-loaded
    (cl-return-from notes-search--load-module t))
  
  ;; Prevent infinite loops - only try once per session
  (when notes-search--module-load-attempted
    (error "xapian-lite module could not be loaded. Run M-x notes-search-reset-module-state to retry"))
  (setq notes-search--module-load-attempted t)
  
  (let* ((module-dir (notes-search--module-dir))
         (module-path (notes-search--module-path)))
    
    ;; Debug output
    (message "notes-search: module-dir = %s" module-dir)
    (message "notes-search: module-path = %s" module-path)
    (message "notes-search: file exists = %s" (file-exists-p module-path))
    
    ;; Ensure module directory is in load-path
    (unless (member module-dir load-path)
      (add-to-list 'load-path module-dir))
    
    ;; Try to require xapian-lite directly first
    (condition-case err
        (progn
          (require 'xapian-lite)
          (setq notes-search--module-loaded t)
          (message "xapian-lite module loaded successfully")
          t)
      (error
       (message "notes-search: load failed with: %s" (error-message-string err))
       
       ;; If file exists, the error is real - report it
       (if (file-exists-p module-path)
           (error "xapian-lite exists at %s but failed to load: %s"
                  module-path (error-message-string err))
         ;; File doesn't exist, try to download
         (unless (notes-search--download-module)
           (error "Could not obtain xapian-lite module"))
         ;; Try loading again after download
         (condition-case err2
             (progn
               (require 'xapian-lite)
               (setq notes-search--module-loaded t)
               (message "xapian-lite module loaded successfully")
               t)
           (error
            (error "Downloaded module but failed to load: %s"
                   (error-message-string err2)))))))))

(defun notes-search--ensure-module ()
  "Ensure the xapian-lite module is loaded."
  (unless notes-search--module-loaded
    (notes-search--load-module)))

;;; Customization

(defgroup notes-search nil
  "Fast note searching with Xapian and Consult."
  :group 'convenience
  :prefix "notes-search-")

(defcustom notes-search-directory "~/notes"
  "Directory containing note files."
  :type 'directory
  :group 'notes-search)

(defcustom notes-search-database-dir
  (expand-file-name "notes-search-db" user-emacs-directory)
  "Directory for the Xapian database."
  :type 'directory
  :group 'notes-search)

(defcustom notes-search-file-extensions '("md")
  "File extensions to index (without the dot)."
  :type '(repeat string)
  :group 'notes-search)

(defcustom notes-search-recursive t
  "Whether to search subdirectories."
  :type 'boolean
  :group 'notes-search)

(defcustom notes-search-title-fallback 'filename
  "What to display when a note has no title in front matter.
Can be `filename' or `first-line'."
  :type '(choice (const :tag "File name" filename)
                 (const :tag "First line of content" first-line))
  :group 'notes-search)

(setq notes-search-async-split-style 'perl%)
(defcustom notes-search-async-split-style 'notes
  "Async split style for search input.
Set to nil to disable splitting (recommended if you search for #hashtags).
Other options: `comma', `semicolon', `perl' (uses #)."
  :type '(choice (const :tag "None (disable splitting)" nil)
                 (const :tag "Comma" comma)
                 (const :tag "Semicolon" semicolon)
                 (const :tag "Perl (uses #)" perl)
                 (const :tag "Perl (uses %)" notes))
  :group 'notes-search)

;;; Internal variables

(defvar notes-search--front-matter-cache (make-hash-table :test 'equal)
  "Cache mapping file paths to parsed front matter.")

(defvar notes-search--cache-timestamps (make-hash-table :test 'equal)
  "Cache mapping file paths to their modification times when cached.")

(defvar notes-search--display-to-path (make-hash-table :test 'equal)
  "Cache mapping display strings to file paths for embark integration.")

;;; Front matter parsing

(defun notes-search--parse-front-matter (file)
  "Parse YAML front matter from FILE."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file nil 0 2048)
        ;; Remove any BOM
        (goto-char (point-min))
        (when (looking-at "\xef\xbb\xbf")
          (delete-region (point-min) (match-end 0)))
        ;; Handle CRLF
        (goto-char (point-min))
        (while (search-forward "\r" nil t)
          (replace-match ""))
        (goto-char (point-min))
        ;; Look for front matter
        (when (looking-at "^---[ \t]*$")
          (forward-line 1)
          (let ((start (point))
                (end (if (re-search-forward "^---[ \t]*$" nil t)
                         (match-beginning 0)
                       (point-max))))
            (notes-search--parse-yaml-region start end))))
    (error
     (message "Error parsing %s: %s" file err)
     nil)))

(defun notes-search--parse-yaml-region (start end)
  "Parse simple YAML between START and END.
Handles both flat format and nested structure with `note:' parent.

Flat format:
---
id: 123
title: My Title
tags: [a, b]
---

Nested format:
---
note:
  id: 123
  title: My Title
  tags: [a, b]
---"
  (let ((content (buffer-substring-no-properties start end))
        (result nil)
        (in-note-block nil)
        (note-indent nil)
        (flat-format nil))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ;; Start of note: block (nested format)
           ((string-match "^note:[ \t]*$" line)
            (setq in-note-block t)
            (setq note-indent nil)
            (setq flat-format nil))
           
           ;; Inside note block - parse indented keys (nested format)
           ((and in-note-block
                 (string-match "^\\([ \t]+\\)\\([a-zA-Z_-]+\\):[ \t]*\\(.*\\)$" line))
            (let ((indent (match-string 1 line))
                  (key (intern (match-string 2 line)))
                  (value (match-string 3 line)))
              ;; Set indent level from first indented line
              (unless note-indent
                (setq note-indent (length indent)))
              ;; Only parse if at the right indent level
              (when (= (length indent) note-indent)
                (setq value (string-trim value))
                ;; Handle tags as a list
                (when (and (eq key 'tags)
                           (string-match "^\\[\\(.*\\)\\]$" value))
                  (setq value (split-string (match-string 1 value) ",[ \t]*" t)))
                ;; Handle empty values
                (unless (string-empty-p value)
                  (push (cons key value) result)))))
           
           ;; Top-level key: value (flat format)
           ((and (not in-note-block)
                 (string-match "^\\([a-zA-Z_-]+\\):[ \t]*\\(.*\\)$" line))
            (let ((key (intern (match-string 1 line)))
                  (value (match-string 2 line)))
              ;; Only capture keys we care about
              (when (memq key '(id title tags))
                (setq flat-format t)
                (setq value (string-trim value))
                ;; Handle tags as a list
                (when (and (eq key 'tags)
                           (string-match "^\\[\\(.*\\)\\]$" value))
                  (setq value (split-string (match-string 1 value) ",[ \t]*" t)))
                ;; Handle empty values
                (unless (string-empty-p value)
                  (push (cons key value) result)))))
           
           ;; Non-indented line ends note block (nested format)
           ((and in-note-block
                 (not (string-match "^[ \t]*$" line))
                 (not (string-match "^[ \t]" line)))
            (setq in-note-block nil))))
        (forward-line 1)))
    (nreverse result)))

(defun notes-search--get-front-matter (file)
  "Get front matter for FILE, using cache when valid."
  (let* ((file (expand-file-name file))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (cached-mtime (gethash file notes-search--cache-timestamps)))
    (if (and cached-mtime (equal mtime cached-mtime))
        (gethash file notes-search--front-matter-cache)
      ;; Parse and cache
      (let ((fm (notes-search--parse-front-matter file)))
        (puthash file fm notes-search--front-matter-cache)
        (puthash file mtime notes-search--cache-timestamps)
        fm))))

(defun notes-search--fm-get (file key)
  (alist-get key (notes-search--get-front-matter file)))

(defun notes-search--get-title (file)
  "Get display title for FILE."
  (let* ((fm (notes-search--get-front-matter file))
         (title (alist-get 'title fm)))
    (cond
     (title title)
     ((eq notes-search-title-fallback 'filename)
      (file-name-sans-extension (file-name-nondirectory file)))
     ((eq notes-search-title-fallback 'first-line)
      (notes-search--first-content-line file))
     (t (file-name-nondirectory file)))))

(defun notes-search--first-content-line (file)
  "Get first non-empty, non-front-matter line from FILE."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 4096)
        (goto-char (point-min))
        ;; Skip front matter
        (when (looking-at "^---[ \t]*$")
          (forward-line 1)
          (when (re-search-forward "^---[ \t]*$" nil t)
            (forward-line 1)))
        ;; Find first non-empty line
        (while (and (not (eobp))
                    (looking-at "^[ \t]*$"))
          (forward-line 1))
        (if (eobp)
            (file-name-nondirectory file)
          (string-trim (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
    (error (file-name-nondirectory file))))

;;; File listing

(defun notes-search--list-files ()
  "List all note files in `notes-search-directory'."
  (let ((dir (expand-file-name notes-search-directory))
        (extensions notes-search-file-extensions))
    (if notes-search-recursive
        (directory-files-recursively
         dir
         (concat "\\." (regexp-opt extensions) "\\'"))
      (cl-loop for ext in extensions
               nconc (directory-files dir t (concat "\\." ext "\\'"))))))

;;; Xapian indexing

(defun notes-search--ensure-db-dir ()
  "Ensure the database directory exists."
  (unless (file-exists-p notes-search-database-dir)
    (make-directory notes-search-database-dir t)))

(defun notes-search-reindex ()
  "Reindex all notes."
  (interactive)
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (let ((files (notes-search--list-files))
        (count 0))
    (dolist (file files)
      (condition-case err
          (progn
            (xapian-lite-reindex-file file notes-search-database-dir)
            (cl-incf count))
        (error (message "Error indexing %s: %s" file err))))
    (message "Indexed %d notes" count)))

(defun notes-search-reindex-file (file)
  "Reindex a single FILE."
  (interactive "fFile: ")
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (xapian-lite-reindex-file (expand-file-name file) notes-search-database-dir)
  ;; Also refresh front matter cache
  (notes-search--get-front-matter file))

(defun notes-search--maybe-reindex-current-buffer ()
  "Reindex current buffer if it's a note file."
  (when (and buffer-file-name
             (file-in-directory-p buffer-file-name notes-search-directory)
             (member (file-name-extension buffer-file-name)
                     notes-search-file-extensions))
    (notes-search-reindex-file buffer-file-name)))

;;; Search interface

(defun notes-search--query (search-term)
  "Query Xapian for SEARCH-TERM, return list of (title . path) pairs.
When SEARCH-TERM is empty or short, returns all notes.
Space-separated words are combined with AND to narrow results.
Search is case-insensitive."
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (condition-case nil
      (let* ((trimmed (string-trim (or search-term "")))
             (files
              (if (< (length trimmed) 1)
                  ;; Empty query: return all notes
                  (notes-search--list-files)
                ;; Non-empty: join words with AND for narrowing behavior
                (let* ((words (split-string (downcase trimmed)))
                       (query (mapconcat (lambda (w) (concat "+" w))
                                         words " ")))
                  (xapian-lite-query-term
                   query
                   notes-search-database-dir
                   0 500)))))
        (reverse files))
    (error nil)))

(defun notes-search--state ()
  "State function for previewing notes."
  (consult--file-state))

;;; Candidate formatting

(defun notes-search--generate-candidates (input)
  "Generate candidates for INPUT query."
  (notes-search--format-candidates (notes-search--query input)))

(defun notes-search--format-candidates (files)
  "Format all PAIRS with aligned tags."
  (let ((max-width (min 60  ; cap it
                        (apply #'max 0
                               (mapcar (lambda (file)
                                         (string-width (notes-search--get-title file)))
                                       files)))))
    (mapcar (lambda (file) (notes-search--format-candidate file max-width))
            files)))

(->> "[[1234]]"
                  (s-replace "[" "")
                  (s-replace "]" ""))

(defun notes-search--format-candidate (file width)
  "Format TITLE-PATH-PAIR with title padded to WIDTH."
  (let* ((path file)
         (title (notes-search--get-title file))
         (tags (notes-search--fm-get file 'tags))
         (id (->> (or (notes-search--fm-get file 'id) "")
                  (s-replace "[" "")
                  (s-replace "]" "")))
         (tags-str (if tags
                       (propertize
                        (if (listp tags)
                            (mapconcat #'identity tags ", ")
                          tags)
                        'face 'font-lock-comment-face)
                     ""))
         (padded-title (concat (truncate-string-to-width title width nil ?\s " ") id))
         (display (if tags
                      (concat padded-title "  " tags-str)
                    padded-title)))
    (puthash display path notes-search--display-to-path)
    (puthash title path notes-search--display-to-path)
    (propertize display
                'consult--candidate path
                'notes-search-file path)))

(defun notes-search--lookup (selected candidates &rest _)
  "Lookup function to get the file path from SELECTED candidate.
CANDIDATES is the candidates list."
  (or (get-text-property 0 'consult--candidate selected)
      (get-text-property 0 'notes-search-file selected)
      (gethash selected notes-search--display-to-path)
      (gethash (string-trim selected) notes-search--display-to-path)
      selected))

;;;###autoload
(defun notes-search ()
  "Search notes interactively with live preview."
  (interactive)
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (unless (file-exists-p notes-search-database-dir)
    (if (yes-or-no-p "No search database found. Index notes now? ")
        (notes-search-reindex)
      (user-error "Cannot search without an index")))
  (let ((consult-async-min-input 0)
        (consult-async-split-style 'perl%))
    (let ((selected
            (consult--read
             (consult--dynamic-collection #'notes-search--generate-candidates)
             :prompt "Search notes: "
             :initial "%%"
             :require-match nil
             :sort nil
             :lookup #'notes-search--lookup
             :state (notes-search--state))))
      (when selected
        (find-file selected)))))


;;;###autoload
(defun notes-search-at-point ()
  "Search notes for thing at point."
  (interactive)
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (unless (file-exists-p notes-search-database-dir)
    (if (yes-or-no-p "No search database found. Index notes now? ")
        (notes-search-reindex)
      (user-error "Cannot search without an index")))
  (let* ((consult-async-split-style notes-search-async-split-style)
         (initial (thing-at-point 'word t))
         (selected
          (consult--read
           (consult--dynamic-collection #'notes-search--generate-candidates)
           :prompt "Search notes: "
           :require-match t
           :sort nil
           :category 'file
           :lookup #'notes-search--lookup
           :state (notes-search--state)
           :initial initial)))
    (when selected
      (find-file selected))))

;;;###autoload
(defun notes-search-simple ()
  "Simpler synchronous note search (for debugging or small collections)."
  (interactive)
  (notes-search--ensure-module)
  (notes-search--ensure-db-dir)
  (unless (file-exists-p notes-search-database-dir)
    (if (yes-or-no-p "No search database found. Index notes now? ")
        (notes-search-reindex)
      (user-error "Cannot search without an index")))
  (let* ((query (read-string "Search notes: "))
         (results (notes-search--query query)))
    (if (null results)
        (message "No results found")
      (let* ((choices (mapcar (lambda (pair)
                                (cons (car pair) (cdr pair)))
                              results))
             (selected (completing-read "Select note: "
                                        (mapcar #'car choices)
                                        nil t)))
        (when selected
          (find-file (cdr (assoc selected choices))))))))

;;; Auto-indexing

(defun notes-search-setup-auto-index ()
  "Set up automatic reindexing on file save."
  (add-hook 'after-save-hook #'notes-search--maybe-reindex-current-buffer))

;;; Utility functions

(defun notes-search-clear-cache ()
  "Clear the front matter cache."
  (interactive)
  (clrhash notes-search--front-matter-cache)
  (clrhash notes-search--cache-timestamps)
  (clrhash notes-search--display-to-path)
  (message "Front matter cache cleared"))

(defun notes-search-reset-module-state ()
  "Reset module loading state to allow retrying.
Use this if module loading failed and you've fixed the issue."
  (interactive)
  (setq notes-search--module-loaded nil)
  (setq notes-search--module-load-attempted nil)
  (message "Module state reset. Next command will retry loading."))

(defun notes-search-rebuild-database ()
  "Delete and rebuild the entire database."
  (interactive)
  (when (yes-or-no-p "Delete and rebuild the notes search database? ")
    (when (file-exists-p notes-search-database-dir)
      (delete-directory notes-search-database-dir t))
    (notes-search-clear-cache)
    (notes-search-reindex)))

;;; Embark integration

(defun notes-search--embark-target-finder ()
  "Target finder for notes-search candidates in embark-collect buffers."
  ;; First try text properties at point or beginning of line
  (when-let* ((prop (or (get-text-property (point) 'notes-search-file)
                        (get-text-property (line-beginning-position) 'notes-search-file)
                        (get-text-property (point) 'consult--candidate)
                        (get-text-property (line-beginning-position) 'consult--candidate)
                        ;; Fallback: look up the line text in our cache
                        (let ((line (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
                          (or (gethash line notes-search--display-to-path)
                              ;; Try trimmed version
                              (gethash (string-trim line) notes-search--display-to-path))))))
    (when (and (stringp prop) (file-exists-p prop))
      (cons 'file prop))))

(defun notes-search-setup-embark ()
  "Set up embark integration for notes-search.
Call this after loading embark."
  (when (boundp 'embark-target-finders)
    (add-to-list 'embark-target-finders #'notes-search--embark-target-finder)))

;; Auto-setup embark if it's already loaded
(with-eval-after-load 'embark
  (notes-search-setup-embark))

(provide 'notes-search)
;;; notes-search.el ends here
