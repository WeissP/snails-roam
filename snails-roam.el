;;; snails-roam.el
;;; -*- lexical-binding: t -*-

(require 'org-roam)
(require 'snails-core)

(defvar snails-roam-new-note-length 4)

(defvar snails-roam-tag-alias '())

(defvar snails-roam-file-and-tags-query
  [:select :distinct [titles:file]
           :from tags
           :left :join titles
           :on (= titles:file tags:file)
           :where titles:title :is :not :null
           ])

(defun snails-roam--process-like (word)
  "add percentage around `word'"
  (interactive)
  (format "%%%s%%" word)
  )

(defun snails-roam-filter-by-tags (tags)
  "generate query to filter files by tags"
  (interactive)
  (let ((query)
        )
    (dolist (x tags query) 
      (let ((tag (or (cdr (assoc x snails-roam-tag-alias))  x))
            )
        (setq query (vconcat query `[:and (like tags:tags '(,(snails-roam--process-like tag)))]))        
        )
      )    
    )
  )

(defun snails-roam-filter-by-input (input)
  "generate query to filter files by input"
  (interactive)
  (let ((query)
        )
    (dolist (x (split-string input " ") query)
      (setq query (vconcat query `[:and (like titles:title ,(snails-roam--process-like x))]))
      )    
    )
  )

(defun snails-roam-generate-candidates (input query &optional len)
  "generate snails candidates"
  (when (or (not len) (> (length input) len)) 
    (let ((search-info (snails-roam-pick-tags-from-input input))
          res file candidates
          )
      (if search-info 
          (setq res (org-roam-db-query
                     (vconcat
                      query
                      (snails-roam-filter-by-tags (split-string (cadr search-info)))
                      (snails-roam-filter-by-input (car search-info))
                      )))
        (setq res (org-roam-db-query
                   (vconcat
                    query
                    (snails-roam-filter-by-input input))))
        )
      (dolist (x res)
        (setq file (car x))
        (snails-add-candiate 'candidates (file-name-sans-extension (file-name-nondirectory file)) file)
        )
      (snails-sort-candidates input candidates 0 0)
      candidates)
    )
  )

(defun snails-roam-pick-tags-from-input (input)
  "get tags after separator symbol"
  (when (string-match-p "," input)
    (split-string input ",")
    ))

(snails-create-sync-backend
 :name
 "ORG-ROAM-NEW"

 :candidate-filter
 (lambda (input)
   (when (> (length input) snails-roam-new-note-length)
     (let ((candidates)
           )
       (snails-add-candiate 'candidates input input)       
       )
     )
   )

 :candidate-do
 (lambda (candidate)
   (org-roam-find-file candidate nil nil t)))

(provide 'snails-roam)
