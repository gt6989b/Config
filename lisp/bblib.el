(defun look-at (file library) "show the source for FILE in LIBRARY"
  (interactive "*sFile name: *slibrary: ")
  (if (not (file-exists-p (concat "/bbsrc/tools/tags/" library)))
      (error ("Unknown library"))
    )

  (if (not (file-exists-p (concat "/bbsrc/tools/tags/" library 
				  "/" file ",v")))
      (error (concat "Can't find file " file " in " library)))

  (find-file-other-window 
   (concat "/bbsrc/tools/tags/" library "/" file)))
