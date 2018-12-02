;;; helm-perspective.el --- Helm integration with perspective -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 18 Feb 2018
;; Version: 0.0.1
;; X-URL: https://github.com/nathan/helm-perspective

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

(defun helm-persp-buffers-list--init ()
  ;; Adapted from `helm-buffers-list--init'
  (helm-attrset
   'candidates (cl-loop
                for buf in (persp-buffers
                            (gethash (helm-attr 'name) (perspectives-hash)))
                when (buffer-live-p buf) collect (buffer-name buf)))
  (let ((result (cl-loop for b in (helm-attr 'candidates)
                         maximize (length b) into len-buf
                         maximize (length (with-current-buffer b
                                            (format-mode-line mode-name)))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defmacro helm-persp-perspectify-action (fun switch)
  `(lambda (candidates)
     ,(if switch
          `(progn
             (let ((this-command 'persp-switch))
               (persp-switch (helm-attr 'name)))
             (funcall #',fun candidates))
        `(with-perspective (helm-attr 'name)
           (funcall #',fun candidates)))))

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

;;;###autoload
(defun helm-persp-buffers ()
  "Pre-configured `helm' for `perspective'."
  (interactive)
  (let* ((current (persp-name (persp-curr)))
         (last (when (persp-last) (persp-name (persp-last))))
         (names (persp-names))
         (names
          (progn
            (when last
              (setq names (cons last (cl-delete last names :test #'equal))))
            (cons current (cl-delete current names :test #'equal)))))
    (helm
     :sources (append (cl-loop
                       for name in names
                       for src = (helm-make-source name 'helm-source-buffers
                                   :init 'helm-persp-buffers-list--init
                                   :keymap helm-buffer-map)
                       do (setf (alist-get 'action src) helm-persp-actions)
                       and collect src)
                      (list
                       (helm-make-source "Kill perspective" 'helm-source-sync
                         :candidates names
                         :action (lambda (_candidate)
                                   (let ((candidates (helm-marked-candidates)))
                                     (if (member (persp-name (persp-curr)) candidates)
                                         (error "Can't kill the current perspective while helm is open")
                                       (dolist (persp (helm-marked-candidates))
                                         (persp-kill persp)))))
                         :persistent-help "Kill perspective(s)")))
     :truncate-lines helm-buffers-truncate-lines
     :buffer "*helm-persp-buffers*")))

(provide 'helm-perspective)

;;; helm-perspective.el ends here
