(defun org-id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((remote-match (string-match "^file:\\([^:]+\\)\\(\\|:.+\\)$" id)))
    (if remote-match
        (let* ((file-raw2 (match-string 1 id))
               (table-id (match-string 2 id))
               (file-raw (org-table-formula-substitute-names file-raw2))
               (file (remove-if (lambda (c) (member c '(40 41))) file-raw)))
          (if (file-exists-p file)
              (let* ((visiting (find-buffer-visiting file))
	             (buffer (or visiting (find-file-noselect file))))
                (unwind-protect
	            (with-current-buffer buffer
                      (beginning-of-buffer)
	              (let ((pos (progn
                                   (unless (string= table-id "")
                                     (let* ((ident (subseq table-id 1))
                                            (id-match (search-forward (concat "#+NAME: " ident) nil t)))
                                       (unless id-match
                                         (error "File \"%s\" has no table with NAME \"%s\"." file ident))
                                       (next-line)))
                                   (re-search-forward "^|-")
                                   (move-beginning-of-line nil))))
                        (cond
	                 ((null pos) nil)
	                 (markerp (move-marker (make-marker) pos buffer))
	                 (t (cons file pos)))))
	          ;; Remove opened buffer in the process.
	          (unless (or visiting markerp) (kill-buffer buffer))))
            (error "org-id-find:  reference to missing file %s" file)))
      (let ((file (org-id-find-id-file id))
	    org-agenda-new-buffers where)
        (when file
          (setq where (org-id-find-id-in-file id file markerp)))
        (unless where
          (org-id-update-id-locations nil t)
          (setq file (org-id-find-id-file id))
          (when file
	    (setq where (org-id-find-id-in-file id file markerp))))
        where))))

(defun org-table-get-remote-range (name-or-id form)
  "Get a field value or a list of values in a range from table at ID.

NAME-OR-ID may be the name of a table in the current file as set
by a \"#+NAME:\" directive.  The first table following this line
will then be used.  Alternatively, it may be an ID referring to
any entry, also in a different file.  In this case, the first
table in that entry will be referenced.
FORM is a field or range descriptor like \"@2$3\" or \"B3\" or
\"@I$2..@II$2\".  All the references must be absolute, not relative.

The return value is either a single string for a single field, or a
list of the fields in the rectangle."
  (save-match-data
    (let ((case-fold-search t) (id-loc nil)
	  ;; Protect a bunch of variables from being overwritten by
	  ;; the context of the remote table.
 	  org-table-column-names (org-table-column-name-regexp org-table-column-name-regexp)
	  ;; org-table-column-names org-table-column-name-regexp
	  org-table-local-parameters org-table-named-field-locations
	  org-table-current-line-types
	  org-table-current-begin-pos org-table-dlines
	  org-table-current-ncol
	  org-table-hlines
	  org-table-last-column-widths
	  org-table-last-alignment
	  buffer loc)
      (setq form (org-table-convert-refs-to-rc form))
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward
	    (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
		    (regexp-quote name-or-id) "[ \t]*$")
	    nil t)
	   (setq buffer (current-buffer) loc (match-beginning 0))
	 (setq id-loc (org-id-find name-or-id 'marker))
	 (unless (and id-loc (markerp id-loc))
	   (user-error "Can't find remote table \"%s\"" name-or-id))
	 (setq buffer (marker-buffer id-loc)
	       loc (marker-position id-loc))
	 (move-marker id-loc nil))
       (with-current-buffer buffer
	 (org-with-wide-buffer
	  (goto-char loc)
	  (forward-char 1)
	  (unless (and (re-search-forward "^\\(\\*+ \\)\\|^[ \t]*|" nil t)
		       (not (match-beginning 1)))
	    (user-error "Cannot find a table at NAME or ID %s" name-or-id))
	  (org-table-analyze)
	  (setq form (org-table-formula-substitute-names
		      (org-table-formula-handle-first/last-rc form)))
	  (if (and (string-match org-table-range-regexp form)
		   (> (length (match-string 0 form)) 1))
	      (org-table-get-range
	       (match-string 0 form) org-table-current-begin-pos 1)
	    form)))))))
