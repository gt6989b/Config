(defun hack-msd () 
  (search-forward (getenv "USER"))
  (search-backward (getenv "USER"))
  (backward-char 4)
  (delete-char 4)
  (insert "????")
  (save-buffer)
  (kill-emacs)
)
