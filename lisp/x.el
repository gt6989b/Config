(defun find-tag-internal (tagname)
  (let ((next (null tagname))
	(exact (or tags-always-exact (consp tagname)))
	tag-target
	tag-tables tag-table-point file linebeg startpos buf
	offset found pat syn-tab)
    (if (consp tagname) (setq tagname (car tagname)))
    (cond (next
	   (setq tag-tables (cdr (cdr last-tag-data)))
	   (setq tagname (car last-tag-data))
	   (setq tag-table-point (car (cdr last-tag-data))))
	  (t
	   (setq tag-tables (buffer-tag-table-list))
	   (setq tag-table-point 1)))
    ;; If tagname is a list: (TAGNAME), this indicates requiring an exact
    ;; symbol match.  Similarly, \_ in the tagname is used to indicate a
    ;; symbol boundary.
    (cond ((or exact
	       (string-match "\\\\_" tagname))
	   (if exact
	       (setq tag-target (concat "\\_" tagname "\\_"))
	     (setq tag-target (copy-sequence tagname)))
	   (while (string-match "\\\\_" tag-target)
	     (aset tag-target (1- (match-end 0)) ?b))
	   (setq syn-tab (get-symbol-syntax-table (syntax-table)))
	   ;;	   (let ((i 0)
	   ;;		 (len (length tag-target))
	   ;;		 j)
	   ;;	     (while (< i len)
	   ;;	       (cond ((eq ?\\ (aref tag-target i))
	   ;;		      (setq j (1+ i))
	   ;;		      (if (eq ?_ (aref tag-target j))
	   ;;			  (aset tag-target j ?b))))
	   ;;	       (setq i (1+ i))))
	   )
	  (t
	   (setq tag-target tagname)
	   (setq syn-tab (syntax-table))))
    (with-caps-disable-folding tag-target
      (save-excursion
        (catch 'found
          (while tag-tables
            (set-buffer (get-tag-table-buffer (car tag-tables)))
            (bury-buffer (current-buffer))
            (goto-char (or tag-table-point (point-min)))
            (setq tag-table-point nil)
            (let ((osyn (syntax-table))
                  case-fold-search)
              (set-syntax-table syn-tab)
              (unwind-protect
                  ;; **** should there be support for non-regexp tag searches?
                  (while (re-search-forward tag-target nil t)
                    (if (looking-at "[^\n\C-?]*\C-?")
                        (throw 'found t)))
                (set-syntax-table osyn)))
            (setq tag-tables (cdr tag-tables)))
          (error "No %sentries %s %s"
                 (if next "more " "")
                 (if exact "matching" "containing")
                 tagname))
        (search-forward "\C-?")
        (setq file (expand-file-name (file-of-tag)
                                     ;; XEmacs change: this needs to be
                                     ;; relative to the 
                                     (or (file-name-directory (car tag-tables))
                                         "./")))
        (setq linebeg (buffer-substring (1- (point)) (point-at-bol)))
        (search-forward ",")
        (setq startpos (read (current-buffer)))
        (setq last-tag-data (nconc (list tagname (point)) tag-tables)))
      (setq buf (find-file-noselect file))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (setq offset 1000)
            (setq pat (concat "^" (regexp-quote linebeg)))
            (or startpos (setq startpos (point-min)))
            (while (and (not found)
                        (progn
                          (goto-char (- startpos offset))
                          (not (bobp))))
              (setq found (re-search-forward pat (+ startpos offset) t))
              (setq offset (* 3 offset)))
            (or found
                (re-search-forward pat nil t)
                (error "%s not found in %s" pat file))
            (beginning-of-line)
            (setq startpos (point)))))
      (cons buf startpos))))
