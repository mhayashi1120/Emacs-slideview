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
;; (define-key view-mode-map "S" 'slideview-toggle-slideshow)

;; todo
;; 1. open any file. (C-x C-f)
;; 2. M-x view-mode
;; 3. Type `N' to next file.

;; 1. view any file (M-x view-file)
;; 2. Type `N' to next file.

;; todo spc to next file.

;;; TODO:
;; * turbulance(?) mode (ignore extension)
;; * arc-mode, tar-mode filter by file extension
;; * refactor arc-mode, tar-mode
;; * use view-exit-action
;; * when directory contains numbered file
;;   1.xml 10.xml 2.xml
;; * can include subdirectory
;; * look ahead

;;; Commentary:
;; 

(eval-when-compile
  (require 'cl))

(require 'view)

;;; Code:

(defcustom slideview-slideshow-interval 5.0
  "*todo"
  :group 'slideview
  :type 'float)

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
    (slideview--restore-local-variables vars)))

(defun slideview--anticipate-background (func)
  (run-with-idle-timer 
   0.5 nil 
   `(lambda (buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-window-excursion
            (let ((next (,func)))
              (when (bufferp next)
                (bury-buffer next)))))))
   (current-buffer)))

;;
;; for directory files
;;

(defun slideview--directory-step (reverse-p)
  (let ((prev (current-buffer))
        (next (slideview--directory-find-next reverse-p)))
    (if reverse-p
        (bury-buffer prev)
      (kill-buffer prev))
    (unless reverse-p
      (when next
        (switch-to-buffer next)
        (slideview--anticipate-background 
         'slideview--directory-anticipate)))))

(defun slideview--directory-find-next (reverse-p &optional no-error)
  (let* ((file buffer-file-name)
         (next (slideview--directory-next file reverse-p)))
    (cond
     (next
      (slideview--view-file next))
     (no-error nil)
     (t
      (error "No more file")))))

(defun slideview--view-file (file)
  (view-file file)
  ;; force to `view-mode'
  ;; because `image-mode' `mode-class' is `special' (TODO why?)
  (unless view-mode
    (view-mode 1))
  (current-buffer))

(defun slideview--directory-next (file reverse)
  (let* ((dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (ext (file-name-extension file))
         (files (directory-files dir nil (concat ext "$")))
         (files (if reverse (nreverse files) files)))
    (car
     (mapcar 
      (lambda (x) (expand-file-name x dir))
      (cdr (member name files))))))

(defun slideview--directory-anticipate ()
  (let ((next (slideview--directory-next buffer-file-name nil)))
    (when next
      (find-file-noselect next t))))

;;
;; tar-mode
;;

(defun slideview--tar-step (reverse-p)
  (let ((prev (current-buffer))
        (next (slideview--tar-find-next reverse-p)))
    (if reverse-p
        (bury-buffer prev)
      (kill-buffer prev))
    (unless reverse-p
      (when next
        (switch-to-buffer next)
        (slideview--anticipate-background 
         (lambda ()
           (slideview--tar-find-next nil t)))))))

(defun slideview--tar-find-next (reverse-p &optional no-error)
  (let ((superior (slideview--tar-buffer))
        (path (tar-header-name tar-superior-descriptor)))
    (with-current-buffer superior
      (let ((next (slideview--tar-next path reverse-p)))
        (cond
         (next
          (slideview--tar-view-file next))
         (no-error nil)
         (t
          (error "No more file")))))))

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
  (or
   (let* ((name (concat buffer-file-name "!" file))
          (buf (get-file-buffer name)))
     (when buf
       (switch-to-buffer buf)
       buf))
   (let ((first (point))
         desc)
     (goto-char (point-min))
     (catch 'done
       (while (setq desc (tar-current-descriptor))
         (when (string= (tar-header-name desc) file)
           (tar-extract 'view)
           (throw 'done (current-buffer)))
         (tar-next-line 1))
       ;; not found. restore the previous point
       (goto-char first)
       (error "%s not found" file)))))

;;
;; for archive-mode
;;

(defun slideview--archive-step (reverse-p)
  (let ((prev (current-buffer))
        (next (slideview--archive-find-next reverse-p)))
    (if reverse-p
        (bury-buffer prev)
      (kill-buffer prev))
    (unless reverse-p
      (when next
        (switch-to-buffer next)
        (slideview--anticipate-background 
         (lambda ()
           (slideview--archive-find-next nil t)))))))

(defun slideview--archive-find-next (reverse-p &optional no-error)
  (let ((superior (slideview--archive-buffer))
        (path (aref archive-subfile-mode 0)))
    (with-current-buffer superior
      (let ((next (slideview--archive-next path reverse-p)))
        (cond
         (next
          (slideview--archive-view-file next))
         (no-error nil)
         (t
          (error "No more file")))))))

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
  (or
   (let* ((name (concat buffer-file-name ":" file))
          (buf (get-file-buffer name)))
     (when buf
       (switch-to-buffer buf)
       buf))
   (let ((first (point))
         desc)
     (goto-char archive-file-list-start)
     (catch 'done
       (while (setq desc (archive-get-descr t))
         (when (string= (aref desc 0) file)
           (archive-view)
           (throw 'done (current-buffer)))
         (archive-next-line 1))
       ;; not found. restore the previous point
       (goto-char first)
       (error "%s not found" file)))))

;;
;; slideshow TODO test
;;

(defvar slideview--slideshow-timer nil)
(make-variable-buffer-local 'slideview--slideshow-timer)

;;TODO  message

(defun slideview-toggle-slideshow ()
  (interactive)
  (cond
   ((and slideview--slideshow-timer
         (timerp slideview--slideshow-timer))
    (message "Stopping slideshow...")
    (cancel-timer slideview--slideshow-timer))
   (t
    (message "Starting slideshow...")
    (slideview-start-slideshow))))

;; TODO accept interval
(defun slideview-start-slideshow ()
  (interactive)
  (setq slideview--slideshow-timer
        (run-with-timer slideview-slideshow-interval nil
                        (slideview--slideshow-next (current-buffer)))))

;;TODO switch to other buffer after start.
(defun slideview--slideshow-next (buffer)
  `(lambda ()
     (condition-case nil
         (progn
           (when (buffer-live-p ,buffer)
             ;;TODO
             (save-window-excursion
               (with-current-buffer ,buffer
                 (slideview-next-file)
                 ;;TODO how to start new timer before slide to next
                 (slideview-start-slideshow)))))
       ;;TODO restrict by signal type
       (error 
        (when (buffer-live-p ,buffer)
          (kill-buffer ,buffer))))))

;;
;; Utilities
;;

(defvar slideview-slideshow-mode-map nil)

(unless slideview-slideshow-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map " " 'slideview-next-file)
    (define-key map "\d" 'slideview-prev-file)

    (setq slideview-slideshow-mode-map map)))

(define-minor-mode slideview-slideshow-mode
  ""
  :init-value nil
  :lighter " Slide"
  :keymap slideview-slideshow-mode-map
  )

(defun slideview--restore-local-variables (variables)
  (loop for p in variables
        do (let ((sym (car p)))
             (set (make-local-variable sym) (cdr p)))))

(defun slideview--serialize-local-variables ()
  (loop for p in (buffer-local-variables)
        if (string-match "^slideview-" (symbol-name (car p)))
        collect p))

(provide 'slideview)

;;; slideview.el ends here
