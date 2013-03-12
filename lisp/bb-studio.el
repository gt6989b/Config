; copied from /bbsrc/tools/script/bb-studio.el 10/30/00

(load-file "~/lisp/iceberg.el")

(defun look-at (file library) "show the source for FILE in LIBRARY"
  (interactive "sFile name: \nslibrary: ")
  (if (not (file-exists-p (concat "/bbsrc/tools/tags/" library)))
      (error ("Unknown library")))
  (if (not (file-exists-p (concat "/bbsrc/tools/tags/" library 
				  "/" file ",v")))
      (error (concat "Can't find file " file " in " library)))
  (find-file
   (concat "/bbsrc/tools/tags/" library "/" file)))

