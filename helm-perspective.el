(require 'perspective)
(require 'helm-buffers)

(declare-function hash-table-keys "subr-x" (hash-table))

;; TODO: Caching buffer lists

(defun helm-persp-buffers-list--init ()
  (let (persp-before-switch-hook
        persp-switch-hook
        persp-activated-hook
        ;; prevent overriding the current value when calling
        ;; `helm-buffers-list--init'
        helm-buffers-list-cache
        ;; disable these since `perspective' activates buffers which results in
        ;; lots of calls to buffer display functions.
        window-configuration-change-hook
        purpose--active-p
        golden-ratio-mode)
    (with-perspective (helm-attr 'name)
      (helm-buffers-list--init)
      (helm-attrset 'persp-buffers helm-buffers-list-cache))))

(defun helm-persp-buffers-list ()
  (helm-attr 'persp-buffers))

;; TODO: More actions
(defconst helm-persp-actions
  (helm-make-actions
   "Switch to buffer in its perspective"
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
  (interactive)
  (helm
   :sources (cl-loop
             for name in (hash-table-keys perspectives-hash)
             for persp = (gethash name perspectives-hash)
             for src = (helm-make-source name 'helm-source-buffers
                         :init 'helm-persp-buffers-list--init
                         :candidates 'helm-persp-buffers-list)
             do (setf (alist-get 'action src) helm-persp-actions)
             and collect src)
   :truncate-lines helm-buffers-truncate-lines
   :keymap helm-buffer-map
   :buffer "*helm-persp-buffers*"))

(provide 'helm-perspective)
