(require 'perspective)
(require 'helm-buffers)

(declare-function hash-table-keys "subr-x" (hash-table))

(defun helm-persp-buffers-list--init ()
  (let (persp-before-switch-hook
		persp-switch-hook
		persp-activated-hook
		helm-buffers-list-cache)
	(with-perspective (helm-attr 'name)
	  (helm-buffers-list--init)
	  (helm-attrset 'persp-buffers helm-buffers-list-cache))))

(defun helm-persp-buffers-list ()
  (helm-attr 'persp-buffers))

;; TODO: Make more actions
(defconst helm-persp-actions
  (helm-make-actions
   "Switch to buffer"
   (lambda (candidate)
	 (persp-switch (helm-attr 'name))
	 (switch-to-buffer candidate))))

;;;###autoload
(defun helm-persp-buffers ()
  (interactive)
  (helm
   :sources (cl-loop
			 for name in (hash-table-keys perspectives-hash)
			 for persp = (gethash name perspectives-hash)
			 collect (helm-make-source name 'helm-source-buffers
					   :init 'helm-persp-buffers-list--init
					   :candidates 'helm-persp-buffers-list))
   :truncate-lines helm-buffers-truncate-lines
   :keymap helm-buffer-map
   :buffer "*helm-persp-buffers*"))

(provide 'helm-perspective)
