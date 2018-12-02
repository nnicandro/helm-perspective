(require 'perspective)
(require 'helm-buffers)

(declare-function hash-table-keys "subr-x" (hash-table))

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
  (append (helm-make-actions
           "Switch to buffer(s) in perspective"
           (lambda (_candidate)
             (persp-switch (helm-attr 'name))
             (helm-window-show-buffers (helm-marked-candidates)))
           "Set buffer(s) to current perspective"
           (lambda (_candidate)
             (mapc #'persp-set-buffer (helm-marked-candidates)))
           "Add buffer(s) to current perspective"
           (lambda (_candidate)
             (mapcar #'persp-add-buffer (helm-marked-candidates)))
           "Remove buffer(s) from their perspective"
           (lambda (_candidate)
             (with-perspective (helm-attr 'name)
               (mapc #'persp-remove-buffer (helm-marked-candidates))))
           "Kill perspective"
           (lambda (_candidate)
             (persp-kill (helm-attr 'name))))
          ;; NOTE: Assumes the first element is `helm-buffer-switch-buffers'
          (cdr helm-type-buffer-actions)))

;;;###autoload
(defun helm-persp-buffers ()
  "Pre-configured `helm' for `perspective'."
  (interactive)
  (helm
   :sources (cl-loop
             with current = (persp-name persp-curr)
             with last = (when persp-last (persp-name persp-last))
             for name in
             (let ((names (hash-table-keys perspectives-hash)))
               (when last
                 (setq names (cons last (cl-delete
                                         last names :test #'equal))))
               (setq names (cons current (cl-delete
                                          current names :test #'equal))))
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
