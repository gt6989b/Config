;;(setq ediff-diff-program "/usr/opt/gnu/bin/diff")
;;(setq ediff-diff-options "--exclude=^     +'\\$Id: .emacs,v 1.36 2003/05/09 20:12:07 zhuang Exp zhuang $$")
(setq ediff-diff-program "/home/zhuang/lisp/dumbdiff")

(defun flatten (l)
  (cond ((null l) ())
	((listp (car l)) (append (car l) (flatten (cdr l))))
	(t (cons (car l) (flatten (cdr l))))))

(defun ediff-exec-process (program buffer synch &rest args)
  (let ((data (match-data)))
    (if (and (not (listp (car args)))
	     (string-match "^[ \t]*$" (car args))) ; delete blank string
	(setq args (cdr args)))
    (setq args (delq nil args)) ; delete nil from arguments
    (if (listp (car args))
	(setq args (flatten args))
      (setq args (ediff-split-string (mapconcat 'identity args " "))))
    (unwind-protect
	(let ((directory default-directory)
	      proc)
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (setq default-directory directory)
	    (if (or (memq system-type '(emx ms-dos windows-nt windows-95))
		    synch)
		;; In OS/2 (emx) do it synchronously, since OS/2 doesn't let us
		;; delete files used by other processes. Thus, in ediff-buffers
		;; and similar functions, we can't delete temp files because
		;; they might be used by the asynch process that computes
		;; custom diffs. So, we have to wait till custom diff
		;; subprocess is done.
		;; Similarly for Windows-*
		;; In DOS, must synchronize because DOS doesn't have
		;; asynchronous processes.
		(apply 'call-process program nil buffer nil args)
	      ;; On other systems, do it asynchronously.
	      (setq proc (get-buffer-process buffer))
	      (if proc (kill-process proc))
	      (setq proc
		    (apply 'start-process "Custom Diff" buffer program args))
	      (setq mode-line-process '(":%s"))
	      (set-process-sentinel proc 'ediff-process-sentinel)
	      (set-process-filter proc 'ediff-process-filter)
	      )))
      (store-match-data data))))

(defun find-tag-internal (tagname)
  "Harvey's kludge"
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
            (let ((osyn (syntax-table)))
;;                  case-fold-search case-fold-search)
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
