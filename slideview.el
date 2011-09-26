;;; slideview.el --- File slideshow

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: slideshow
;; Emacs: GNU Emacs 22 or later
;; Version: 0.0.1
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Usage:

;; (require 'slideview)
;; (define-key view-mode-map "N" 'slideview-next-file)
;; (define-key view-mode-map "P" 'slideview-prev-file)


;;; TODO:
;; * turbulance mode
;; * arc-mode, tar-mode filter by file extension
;; * refactor arc-mode, tar-mode

;;; Commentary:
;; 

(eval-when-compile
  (require 'cl))

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
  (let ((vars (slideview--serialize-local-variables)))
    (cond
     ((and (boundp 'archive-subfile-mode)
           archive-subfile-mode)
      (slideview--archive-step reverse-p))
     ((and (boundp 'tar-subfile-mode)
           tar-subfile-mode)
      (slideview--tar-step reverse-p))
     (t
      (slideview--directory-step reverse-p)))
    (slideview--restore-local-variables vars)
    (unless view-mode
      (view-mode 1))))

;;
;; for directory files
;;

(defun slideview--directory-step (reverse-p)
  (let* ((file buffer-file-name)
         (next (slideview--directory-files file reverse-p)))
    (unless next
      (error "No more file"))
    (kill-buffer (current-buffer))
    (view-file next)))

(defun slideview--directory-files (file reverse)
  (let* ((dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (ext (file-name-extension file))
         (files (directory-files dir nil (concat ext "$")))
         (files (if reverse (nreverse files) files)))
    (car
     (mapcar 
      (lambda (x) (expand-file-name x dir))
      (cdr (member name files))))))

;;
;; tar-mode
;;

(defun slideview--tar-step (reverse-p)
  (let ((superior (slideview--tar-buffer))
        (path (tar-header-name tar-superior-descriptor))
        (prev (current-buffer)))
    (with-current-buffer superior
      (let ((next (slideview--tar-next path reverse-p)))
        (unless next
          (error "No more file"))
        (slideview--tar-view-file next)))
    (kill-buffer prev)))

(defun slideview--tar-buffer ()
  (or (and tar-superior-buffer
           (buffer-live-p tar-superior-buffer)
           tar-superior-buffer)
      (and buffer-file-name
           (let ((file buffer-file-name))
             (and (string-match "\\(.+?\\)!" file)
                  (match-string 1 file))))))

(defun slideview--tar-next (path reverse)
  (let* ((files (sort 
                 (mapcar 'tar-header-name tar-parse-info)
                 'string-lessp))
         (files (if reverse (nreverse files) files)))
    (cadr (member path files))))

(defun slideview--tar-view-file (file)
  (let ((first (point))
        desc)
    (goto-char (point-min))
    (catch 'done
      (while (setq desc (tar-current-descriptor))
        (when (string= (tar-header-name desc) file)
          (tar-extract 'view)
          (throw 'done t))
        (tar-next-line 1))
      ;; not found. restore the previous point
      (goto-char first)
      (error "%s not found" file))))

;;
;; for archive-mode
;;

(defun slideview--archive-step (reverse-p)
  (let ((superior (slideview--archive-buffer))
        (path (aref archive-subfile-mode 0))
        (prev (current-buffer)))
    (with-current-buffer superior
      (let ((next (slideview--archive-next path reverse-p)))
        (unless next
          (error "No more file"))
        (slideview--archive-view-file next)))
    (kill-buffer prev)))

(defun slideview--archive-next (path reverse)
  (let* ((files (sort 
                 (loop for f across archive-files
                       collect (aref f 0))
                 'string-lessp))
         (files (if reverse (nreverse files) files)))
    (cadr (member path files))))

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

;;
;; Utilities
;;

(defun slideview--restore-local-variables (variables)
  (loop for p in variables
        do (let ((sym (car p)))
             (set (make-local-variable sym) (cdr p)))))

(defun slideview--serialize-local-variables ()
  (loop for p in (buffer-local-variables)
        if (string-match "^slideview" (symbol-name (car p)))
        collect p))

(provide 'slideview)

;;; slideview.el ends here
