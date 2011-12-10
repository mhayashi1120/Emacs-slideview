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
;;
;; (require 'slideview)

;;; Commentary:
;; 

;;TODO M-x slideview-mode

;; todo
;; 1. open any file. (C-x C-f)
;; 2. M-x view-mode
;; 3. Type `N' to next file.

;; 1. view any file (M-x view-file)
;; 2. Type SPACE to next file.

;; todo spc to next file.

;;; TODO:
;; * use view-exit-action
;; * when directory contains numbered file
;;   1.xml 10.xml 2.xml
;; * can include subdirectory
;; * look ahead (more sophisticated)


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'view)
(require 'eieio)

(defgroup slideview ()
  "Sequential viewing files."
  :group 'applications)

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

(defcustom slideview-prefetch-count 2
  "*Number of count prefetching slideshow files.")

(defun slideview--prefetch-background (buffer context &optional count)
  "FUNC must receive one arg and return next buffer.
That arg is CONTEXT.
"
  (run-with-idle-timer 
   0.5 nil `slideview--prefetch-body
   buffer context (or count slideview-prefetch-count)))

(defun slideview--prefetch-body (buf ctx remain)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (save-window-excursion
        (let ((next (slideview--find-next ctx nil t)))
          (when (bufferp next)
            (bury-buffer next)
            (when (> remain 0)
              (slideview--prefetch-background
               next ctx (1- remain)))))))))

(defclass slideview-context ()
  ((buffers :type list
            :initform nil)
   (base-file :initarg :base-file
              :type string))
  :abstract t)

(defvar slideview--context nil)
(make-variable-buffer-local 'slideview--context)

(defvar slideview--next-context nil)

(defun slideview-new-context ()
  (unless buffer-file-name
    (error "Not a file buffer"))
  (cond
   ((and (boundp 'archive-subfile-mode)
         archive-subfile-mode)
    (make-instance slideview-archive-context))
   ((and (boundp 'tar-buffer)           ;FIXME tar-buffer is local-variable
         tar-buffer)
    (make-instance slideview-tar-context))
   (t
    (make-instance slideview-directory-context))))

(defun slideview--step (&optional reverse-p)
  (let* ((context slideview--context)
         (prev (current-buffer))
         (next (slideview--find-next context reverse-p)))
    (if reverse-p
        (bury-buffer prev)
      (slideview--kill-buffer prev))
    (unless reverse-p
      (when next
        (switch-to-buffer next)
        (slideview--prefetch)))))

(defun slideview--kill-buffer (buffer)
  "Kill BUFFER suppress `slideview--cleanup' execution."
  (with-current-buffer buffer
    (remove-hook 'kill-buffer-hook 'slideview--cleanup t))
  (kill-buffer buffer))

(defun slideview--find-next (context reverse-p &optional no-error)
  (let* (
         ;; pass to next `slideview-mode'
         (slideview--next-context context) 
         (next (slideview--next-buffer context reverse-p)))
    (cond
     (next 
      (let ((bufs (remq nil (mapcar 
                              (lambda (b)
                                (and (buffer-live-p b) b))
                              (oref context buffers)))))
        (with-current-buffer next
          (slideview-mode 1))
        (unless (memq next bufs)
          (oset context buffers (cons next bufs)))
        next))
     (no-error nil)
     (t
      (error "No more file")))))

(defun slideview--prefetch (&optional buffer context)
  (slideview--prefetch-background 
   (or buffer (current-buffer))
   (or context slideview--context)))

(defun slideview--next-item (now items reverse-p)
  (let ((items (if reverse-p (reverse items) items)))
    (car (cdr (member now items)))))

;;
;; for directory files
;;

(defclass slideview-directory-context (slideview-context)
  (
   (files :initarg :files
          :type list)
   )
  "Slideshow context for a regular file."
  )

(defmethod initialize-instance ((this slideview-directory-context) &rest fields)
  (call-next-method)
  (oset this :base-file default-directory)
  (let* ((file (expand-file-name buffer-file-name))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (files (directory-files dir nil "^\\(?:[^.]\\|\\.\\(?:[^.]\\|\\..\\)\\)")))
    (oset this files
          (remq nil 
                (mapcar 
                 (lambda (x) 
                   ;; exclude directory
                   (unless (eq (car (file-attributes x)) t)
                     (expand-file-name x dir)))
                 files)))))

(defmethod slideview--next-buffer ((context slideview-directory-context) reverse-p)
  (let ((next (slideview--next-item buffer-file-name (oref context files) reverse-p)))
    (and next (slideview--view-file next))))

(defun slideview--view-file (file)
  (view-file file)
  ;; force to `view-mode'
  ;; because `image-mode' `mode-class' is `special' (TODO why?)
  (unless view-mode
    (view-mode 1))
  (current-buffer))

;;
;; tar-mode
;;

(defclass slideview-tar-context (slideview-context)
  (
   (superior-buffer :initarg :superior-buffer
                    :type buffer)
   (paths :initarg :paths
          :type list)
   )
  "Slideshow context for `tar-mode'"
  )

(defmethod initialize-instance ((this slideview-tar-context) &rest fields)
  (call-next-method)
  (with-current-buffer (slideview--superior-buffer this)
    (oset this :base-file buffer-file-name)
    (oset this paths
          (let* ((files (sort 
                         (mapcar 'tar-header-name tar-parse-info)
                         'string-lessp)))
            files))))

(defmethod slideview--next-buffer ((context slideview-tar-context) reverse-p)
  (let* ((superior (slideview--superior-buffer context))
         (path (and superior tar-superior-descriptor 
                    (tar-header-name tar-superior-descriptor))))
    (with-current-buffer superior
      (let ((next (and path (slideview--next-item path (oref context paths) reverse-p))))
        (and next (slideview--tar-view-file next))))))

(defmethod slideview--superior-buffer ((context slideview-tar-context))
  (or 
   (and (bufferp tar-superior-buffer)
        (buffer-live-p tar-superior-buffer)
        tar-superior-buffer)
   ;;FIXME this is local variable defined at tar-mode.el
   (and (boundp 'tar-buffer)
        tar-buffer)
   (and buffer-file-name
        (let ((file buffer-file-name))
          (and (string-match "^\\(.+?\\)!" file)
               (let ((archive (match-string 1 file)))
                 (or (get-file-buffer archive)
                     ;; Open archive buffer with no confirmation
                     (find-file-noselect archive))))))))

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

(defclass slideview-archive-context (slideview-context)
  (
   (paths :initarg :paths)
   )
  "Slideshow context for `archive-mode'"
  )

(defmethod initialize-instance ((this slideview-archive-context) &rest fields)
  (call-next-method)
  (with-current-buffer (slideview--superior-buffer this)
    (oset this :base-file buffer-file-name)
    (oset this paths
          (let* ((files (sort 
                         (loop for f across archive-files
                               collect (aref f 0))
                         'string-lessp)))
            files))))

(defmethod slideview--next-buffer ((context slideview-archive-context) reverse-p)
  (let* ((superior (slideview--superior-buffer context))
         (path (and archive-subfile-mode
                    (aref archive-subfile-mode 0))))
    (with-current-buffer superior
      (let ((next (and path (slideview--next-item path (oref context paths) reverse-p))))
        (and next (slideview--archive-view-file next))))))

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

(defmethod slideview--superior-buffer ((context slideview-archive-context))
  (or 
   (and (bufferp archive-superior-buffer)
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

;;
;; slideshow TODO test
;;

(defvar slideview--slideshow-timer nil)
;;TODO only one slideshow in one emacs process
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
;; Minor mode
;;

(defvar slideview-mode-map nil)

(unless slideview-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map " " 'slideview-next-file)
    (define-key map "\d" 'slideview-prev-file)

    (setq slideview-mode-map map)))

(define-minor-mode slideview-mode
  ""
  :init-value nil
  :lighter " Slide"
  :keymap slideview-mode-map
  (cond
   (slideview-mode
    (add-hook 'kill-buffer-hook 'slideview--cleanup nil t)
    (setq slideview--context 
          (or slideview--next-context
              (slideview-new-context))))
   (t
    (remove-hook 'kill-buffer-hook 'slideview--cleanup t)
    (when slideview--context 
      (slideview--cleanup)))))

(defun slideview--cleanup ()
  (condition-case nil
      (slideview--cleanup-buffers slideview--context)
    (error nil)))

(defun slideview--cleanup-buffers (context)
  (mapc 
   (lambda (b)
     ;; save the current buffer
     (and (not (eq (current-buffer) b))
          (buffer-live-p b)
          (slideview--kill-buffer b)))
   (oref context buffers)))

;;
;; Utilities
;;

(defun slideview-sequence-lessp (x1 x2)
  (let ((l1 (length x1))
              (l2 (length x2)))
        (cond
         ((> l2 l1) t)
         ((< l2 l1) nil)
         (t
          (string-lessp x1 x2)))))

(provide 'slideview)

;;; slideview.el ends here
