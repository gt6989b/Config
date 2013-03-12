; copied from /bbsrc/tools/scripts/skeleton-stuff/iceberg.el 10/30/00

(defun add-handler (handler) "insert a new skeleton handler at point"
  (interactive "*sHandler name: ")
  (save-excursion
    (goto-char 0)
    (if (not (search-forward "/*SKELETON-PROTOTYPE-END:" nil t))
	(error "Buffer is not an iceberg driver: prototype marker not found"))
    (beginning-of-line)
    (shell-command-on-region 
     (point) (point) 
     (concat "/bbsrc/tools/scripts/skeleton :prototype: " handler)
     (current-buffer) t))
  (save-excursion
    (shell-command-on-region
     (point) (point) 
     (concat "/bbsrc/tools/scripts/skeleton :handler: " handler)
     (current-buffer) t)
    (goto-char (point)))
)
