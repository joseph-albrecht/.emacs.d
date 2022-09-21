(require 'burly)

(cl-defstruct corkboard-location name location)
(cl-defstruct corkboard-board    name locations)

(setq corkboard-archive (list (make-corkboard-board :name "default" :locations nil))) 
(setq corkboard-current (car corkboard-archive))
(setf corkboard-index 0)

(defun corkboard--add-corkboard (name)
  (setf corkboard-archive
        (cons (make-corkboard-board :name name nil)
              corkboard-archive))
  (when (not corkboard-current)
    (setq corkboard-current (car corkboard-archive))))

(defun corkboard--delete-board (name)
  (setf corkboard-archive
        (seq-remove (lambda (board)
                      (equal name (corkboard-board-name board)))
                    corkboard-archive)))

(defun corkboard--goto-location (location)
  (burly-open-url (corkboard-location-location location))
  (corkboard-update-index location)
  (message "location: %s" (corkboard-location-name location)))

(defun corkboard--add-location (name corkboard)
  (setf (corkboard-board-locations corkboard)
        (cons (make-corkboard-location :name name :location (burly-kill-buffer-url (current-buffer)))
              (corkboard-board-locations corkboard)))
  (message "Added location: %s" name))

(defun corkboard--delete-location (name corkboard)
  (setf (corkboard-board-locations corkboard)
        (seq-remove (lambda (location)
                      (equal name (corkboard-location-location location)))
                    (corkboard-board-locations corkboard))))

(defun corkboard-add-board ()
  (interactive)
  (let* ((name (read-string "board name: "))
         (board (make-corkboard-board :name name :locations nil)))
    (setf corkboard-archive (cons board
                                  corkboard-archive))
    (setq corkboard-current board)))

(defun corkboard-add-location ()
  (interactive)
  (let* ((line (s-trim (buffer-substring (progn (beginning-of-line) (point))
                                         (progn (end-of-line) (point)))))
         (default (format "%s: %s" (buffer-name) line))
         (prompt (format "location name (default %s): " default))
         (name (read-string prompt nil nil default)))
    (corkboard--add-location name corkboard-current)))

(defun corkboard-find-location-named (name corkboard)
  (seq-find (lambda (location)
              (equal name (corkboard-location-name location)))
            (corkboard-board-locations corkboard-current)))

(defun corkboard-update-index (location)
  (setq corkboard-index
        (seq-position (corkboard-board-locations corkboard-current) location #'cl-equalp)))

(defun corkboard-goto-location ()
  (interactive)
  (let* ((location-name (consult--read (seq-map #'corkboard-location-name (corkboard-board-locations corkboard-current))
                                       :prompt "location: "
                                       :require-match t
                                       :state (lambda (action candidate)
                                                (when-let ((location (corkboard-find-location-named candidate corkboard-current)))
                                                  (corkboard--goto-location location)))))
         (location      (corkboard-find-location-named location-name corkboard-current)))
    (corkboard--goto-location location)))

(defun corkboard-delete-location ()
  (interactive)
  (let* ((name (->> corkboard-current
                    corkboard-board-locations
                    (completing-read "location: "))))
    (corkboard--delete-location name corkboard-current)))

(defun corkboard-next-location ()
  (interactive)
  (when (> (length (corkboard-board-locations corkboard-current))
             0)
    (let* ((next-index (mod (1+ corkboard-index)
                          (length (corkboard-board-locations corkboard-current))))
         (location (nth next-index (corkboard-board-locations corkboard-current))))
    (corkboard--goto-location location)
    (corkboard-update-index location))))

(defun corkboard-previous-location ()
  (interactive)
  (when (> (length (corkboard-board-locations corkboard-current))
             0)
    (let* ((next-index (mod (1- corkboard-index)
                          (length (corkboard-board-locations corkboard-current))))
         (location (nth next-index (corkboard-board-locations corkboard-current))))
    (corkboard--goto-location location)
    (corkboard-update-index location))))

(defun corkboard-delete-board ()
  (interactive)
  (let* ((name (completing-read "board: " corkboard-archive)))
    (corkboard--delete-board name)))

(defun corkboard-select-board ()
  (interactive)
  (let* ((board-name (completing-read "board: " (seq-map #'corkboard-board-name corkboard-archive)))
         (board      (seq-find (lambda (board) (equal (corkboard-board-name board) board-name)) corkboard-archive)))
    (setq corkboard-current board)
    (message "current board: %s" (corkboard-board-name corkboard-current))))

(provide 'corkboard)

