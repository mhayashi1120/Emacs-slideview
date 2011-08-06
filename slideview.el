;;; slideview.el --- File slideshow
;;; Usage:

;; (require 'slideview)
;; (define-key view-mode-map "N" 'slideview-next-file)
;; (define-key view-mode-map "P" 'slideview-prev-file)



;;; Commentary:
;; 

(require 'view)

;; todo use view-exit-action
;;; Code:

(defun slideview-next-file ()
  "View next file (have same extension, sorted by string order)"
  (interactive)
  (slideview--step))

(defun slideview-prev-file ()
  "View next file (have same extension, sorted by reverse string order)"
  (interactive)
  (slideview--step t))

(defun slideview--step (&optional reverse-p)
  (unless buffer-file-name
    (error "Not a file buffer"))
  (cond
   ((and (boundp 'archive-subfile-mode)
         archive-subfile-mode)
    (slideview--archive-step reverse-p))
   (t
    (slideview--directory-step reverse-p))))

(defun slideview--directory-step (reverse-p)
  (let* ((file buffer-file-name)
         (next (slideview--directory-files file reverse-p)))
    (unless next
      (error "No more file"))
    (kill-buffer (current-buffer))
    (view-file (car next))
    (unless view-mode
      (view-mode 1))))

(defun slideview--directory-files (file reverse)
  (let* ((dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (ext (file-name-extension file))
         (files (directory-files dir nil (concat ext "$")))
         (files (if reverse (nreverse files) files)))
    (mapcar 
     (lambda (x) (expand-file-name x dir))
     (cdr (member name files)))))

(defun slideview--archive-step (reverse-p)
  (let ((superior (slideview--archive-buffer))
        (path (aref archive-subfile-mode 0))
        (prev (current-buffer)))
    (with-current-buffer superior
      (let ((next (slideview--archive-paths path reverse-p)))
        (unless next
          (error "No more file"))
        (slideview--archive-view-file (car next))))
    (kill-buffer prev)))

(defun slideview--archive-paths (path reverse)
  (let* ((files (sort 
                 (loop for f across archive-files
                       collect (aref f 0))
                 'string-lessp))
         (files (if reverse (nreverse files) files)))
    (cdr (member path files))))

(defun slideview--archive-buffer ()
  (or (and archive-superior-buffer
           (buffer-live-p archive-superior-buffer)
           archive-superior-buffer)
      (and archive-subfile-mode
           buffer-file-name
           (let ((file buffer-file-name)
                 (archive-path (aref archive-subfile-mode 0)))
             (when (string-match (concat (regexp-quote archive-path) "$") file)
               (let ((archive (substring file 0 (1- (match-beginning 0)))))
                 (or (get-file-buffer archive)
                     ;; Open archive buffer with no confirmation
                     (find-file-noselect archive))))))))

(defun slideview--archive-view-file (file)
  (let ((first (point))
        desc)
    (goto-char archive-file-list-start)
    (catch 'done
      (while (setq desc (archive-get-descr t))
        (when (string= (aref desc 0) file)
          (archive-view)
          (throw 'done t))
        (archive-next-line 1))
      ;; not found. restore the previous point
      (goto-char first)
      (error "%s not found" file))))

(provide 'slideview)

;;; slideview.el ends here
