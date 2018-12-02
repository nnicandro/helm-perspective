(require 'perspective)
(require 'helm-buffers)

(declare-function hash-table-keys "subr-x" (hash-table))

;; TODO: Caching buffer lists

(defun helm-persp-buffers-list--init ()
  ;; Adapted from `helm-buffers-list--init'
  (helm-attrset
   'candidates (cl-loop
                for buf in (persp-buffers
                            (gethash (helm-attr 'name) perspectives-hash))
                for name = (buffer-name buf)
                if (not (null name)) collect name))
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

;; TODO: The buffer actions for `helm-source-buffers' are still available.
;; Merge these actions into those?
(defconst helm-persp-actions
  (helm-make-actions
   "Switch to buffer in perspective"
   (lambda (candidate)
     (persp-switch (helm-attr 'name))
     (switch-to-buffer candidate))
   "Set buffer to current perspective" #'persp-set-buffer
   "Add buffer to current perspective" #'persp-add-buffer
   "Remove buffer from its perspective"
   (lambda (candidate)
     (with-perspective (helm-attr 'name)
       (persp-remove-buffer candidate)))))

;;;###autoload
(defun helm-persp-buffers ()
  "Pre-configured `helm' for `perspective'."
  (interactive)
  (helm
   :sources (cl-loop
             for name in
             (sort (hash-table-keys perspectives-hash)
                   (lambda (a b)
                     (let ((c (compare-strings a nil nil b nil nil t)))
                       (or (eq c t) (< c 0)))))
             for persp = (gethash name perspectives-hash)
             for src = (helm-make-source name 'helm-source-buffers
                         :init 'helm-persp-buffers-list--init
                         :keymap helm-buffer-map)
             do (setf (alist-get 'action src) helm-persp-actions)
             and collect src)
   :truncate-lines helm-buffers-truncate-lines
   :keymap helm-buffer-map
   :buffer "*helm-persp-buffers*"))

(provide 'helm-perspective)
