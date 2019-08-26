;;; helm-perspective.el --- Helm integration with perspective -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 18 Feb 2018
;; Version: 0.0.1
;; URL: https://github.com/dzop/helm-perspective

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Show the buffers in every `perspective' of the current frame, each
;; perspective is shown as a `helm' source.

;;; Code:

(require 'perspective)
(require 'helm-buffers)

(defgroup helm-perspective nil
  "Helm integration with perspective"
  :group 'helm)

(declare-function hash-table-keys "subr-x" (hash-table))

(defclass helm-persp-buffers-source-add (helm-source-buffers)
  ((buffer-list
    :initform #'helm-persp-buffers--orphaned-buffers))
  :documentation "Source for adding a buffer that does not belong to any perspective.")

(cl-defmethod helm-setup-user-source ((source helm-persp-buffers-source-add))
  (setf (slot-value source 'persistent-help) "Add buffer(s)")
  (helm-aif (slot-value source 'persistent-help)
      (setf (slot-value source 'header-line)
            (helm-source--persistent-help-string it source))
    (setf (slot-value source 'header-line) (helm-source--header-line source)))
  (setf (slot-value source 'persistent-action)
        (lambda (_candidate)
          (dolist (buf (helm-marked-candidates))
            (persp-add-buffer buf)))))

(defmacro helm-persp-perspectify-action (fun switch)
  "Return a helm action that call's FUN in a different perspective.
If SWITCH is non-nil, the returned function calls FUN after
switching to the perspective whose name is identical to the name
of the current `helm' source and stays in the switched
perspective. Otherwise, the perspective is temporarily switched
to before calling FUN."
  `(lambda (candidates)
     ,@(if switch
           `((persp-switch (helm-attr 'name))
             (funcall #',fun candidates))
         `((with-perspective (helm-attr 'name)
             (funcall #',fun candidates))))))

(defun helm-persp-buffers--orphaned-buffers ()
  (cl-loop
   for buf in (apply #'nconc (mapcar (lambda (name) (helm-persp-buffers--get name t))
                                (persp-names)))
   unless (memq buf all-persp-bufs)
   collect buf into all-persp-bufs
   finally return (cl-loop
                   for buf in (buffer-list)
                   unless (memq buf all-persp-bufs)
                   collect (buffer-name buf))))

(defun helm-persp-buffers--get (name &optional as-buffers)
  (let ((persp (gethash name (perspectives-hash))))
    (when (and persp (not (persp-killed-p persp)))
      (cl-loop
       for buf in (persp-buffers persp)
       when (buffer-live-p buf)
       collect (if as-buffers buf (buffer-name buf))))))

(defun helm-persp-buffers-get ()
  "Return the buffer names for the current `helm' `perspective' source."
  (helm-persp-buffers--get (helm-attr 'name)))

(defconst helm-persp-actions
  `(("Switch to buffer(s)" .
     ,(helm-persp-perspectify-action helm-buffer-switch-buffers t))
    ("Set buffer(s)" .
     (lambda (_candidate)
       (mapc #'persp-set-buffer (helm-marked-candidates))))
    ("Add buffer(s)" .
     (lambda (_candidate)
       (mapcar #'persp-add-buffer (helm-marked-candidates))))
    ("Remove buffer(s)" .
     ,(helm-persp-perspectify-action
       (lambda (_candidate)
         (mapc #'persp-remove-buffer (helm-marked-candidates)))
       nil))
    ("Switch to buffer(s) other window `C-c o'" .
     ,(helm-persp-perspectify-action helm-buffer-switch-buffers-other-window t))
    ("Browse project from buffer" .
     ,(helm-persp-perspectify-action helm-buffers-browse-project t))
    ("Query replace regexp `C-M-%'" .
     ,(helm-persp-perspectify-action helm-buffer-query-replace-regexp t))
    ("Query replace `M-%'" .
     ,(helm-persp-perspectify-action helm-buffer-query-replace t))
    ("View buffer" .
     ,(helm-persp-perspectify-action view-buffer t))
    ("Display buffer" .
     ,(helm-persp-perspectify-action display-buffer t))
    ("Rename buffer" .
     ,(helm-persp-perspectify-action helm-buffers-rename-buffer nil))
    ("Grep buffers `M-g s' (C-u grep all buffers)" .
     ,(helm-persp-perspectify-action helm-zgrep-buffers t))
    ("Multi occur buffer(s) `C-s'" .
     ,(helm-persp-perspectify-action helm-multi-occur-as-action t))
    ("Revert buffer(s) `M-U'" . helm-revert-marked-buffers)
    ("Insert buffer" . insert-buffer)
    ("Kill buffer(s) `M-D'" . helm-kill-marked-buffers)
    ("Diff with file `C-='" .
     ,(helm-persp-perspectify-action diff-buffer-with-file t))
    ("Ediff Marked buffers `C-c ='" .
     ,(helm-persp-perspectify-action helm-ediff-marked-buffers t))
    ("Ediff Merge marked buffers `M-='" .
     ,(helm-persp-perspectify-action
       (lambda (candidate)
         (helm-ediff-marked-buffers candidate t))
       t))))

(defun helm-persp--names ()
  (let ((current (persp-name (persp-curr)))
        (last (when (persp-last) (persp-name (persp-last))))
        (names (persp-names)))
    (when last
      (setq names (cons last (delete last names))))
    (cons current (delete current names))))

(defun helm-persp--kill-source (names)
  (helm-make-source "Kill perspective" 'helm-source-sync
    :candidates names
    :action
    (lambda (_candidate)
      (let ((candidates (helm-marked-candidates)))
        (if (member (persp-name (persp-curr)) candidates)
            (error "Can't kill the current perspective while helm is open")
          (dolist (persp (helm-marked-candidates))
            (persp-kill persp)))))
    :persistent-help "Kill perspective(s)"))

;;;###autoload
(defun helm-persp-kill ()
  "Pre-configured `helm' to kill a perspective."
  (interactive)
  (helm :sources (list (helm-persp--kill-source (helm-persp--names)))
        :truncate-lines helm-buffers-truncate-lines
        :buffer "*helm-persp-kill*"))

;;;###autoload
(defun helm-persp-buffers ()
  "Pre-configured `helm' for `perspective'.
The buffers contained in each perspective of the `selected-frame'
is a separate `helm' source. The `persp-curr' is always the first
source and the `persp-last' is always the second.

There are also sources to kill a perspective and to associate a
buffer that is not in any perspective with the current
perspective.

There are also `perspective' specific `helm' actions and all
other buffer actions are \"perspectified\" so that they switch to
the corresponding perspective if needed before performing the
action."
  (interactive)
  (let ((names (helm-persp--names)))
    (helm
     :sources
     (append
      (cl-loop
       for name in names
       for src = (helm-make-source name 'helm-source-buffers
                   :buffer-list #'helm-persp-buffers-get)
       do (setf (alist-get 'action src) helm-persp-actions)
       and collect src)
      (list
       (helm-persp--kill-source names)
       (helm-make-source "Associate buffer" 'helm-persp-buffers-source-add)))
     :truncate-lines helm-buffers-truncate-lines
     :buffer "*helm-persp-buffers*")))

(provide 'helm-perspective)

;;; helm-perspective.el ends here
