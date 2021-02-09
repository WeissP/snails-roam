;;; snails-roam.el
;;; -*- lexical-binding: t -*-

(require 'org-roam)
(require 'snails-core)

(defvar snails-roam-file-and-tags-query
  [:select [file] :from tags])

(defun snails-roam--process-like (word)
  "add percentage around `word'"
  (interactive)
  (format "%%%s%%" word)
  )

(defun snails-roam-filter-by-tags (tag &rest tags)
  "generate query to filter files by tags"
  (interactive)
  (let ((query `[:where (like tags '(,(snails-roam--process-like tag)))])
        )
    (dolist (x tags query)
      (setq query (vconcat query `[:and (like tags '(,(snails-roam--process-like x)))]))
      )    
    )
  )

(defun snails-roam-filter-by-input (input)
  "generate query to filter files by input"
  (interactive)
  (let ((query)
        )
    (dolist (x (split-string input " ") query)
      (setq query (vconcat query `[:and (like file ,(snails-roam--process-like x))]))
      )    
    )
  )

(defun snails-roam-generate-candidates (input query &optional len)
  "generate snails candidates"
  (when (or (not len) (> (length input) len)) 
    (let ((res (org-roam-db-query (vconcat query
                                           (snails-roam-filter-by-input input))))
          file candidates
          )
      (dolist (x res)
        (setq file (car x))
        (snails-add-candiate 'candidates (file-name-sans-extension (file-name-nondirectory file)) file)
        )
      (snails-sort-candidates input candidates 0 0)
      candidates)
    )
  )

(provide 'snails-roam)
