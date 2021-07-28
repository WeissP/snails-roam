;; Copyright (C) 2010-2021 WeissP

;; Author: WeissP <kingbaiing@163.com>
;; Maintainer: WeissP <kingbaiing@163.com>
;; Created: 20 juni 2021
;; Keywords: org-roam snails

;; This file is not part of GNU Emacs.

;; This file is free software…

;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

(require 'org-roam)
(require 'snails-core)

(defvar snails-roam-new-note-length 4)

(defvar snails-roam-tag-alias '())

(defvar snails-roam-file-and-tags-query
  [:select :distinct [nodes:file]
           :from nodes
           :join tags
           :on (= nodes:id tags:node-id)
           :where tags:tag :is :not :null
           ])

(defvar snails-roam-tags-query "SELECT tags.node_id FROM tags WHERE tags.tag IS NOT NULL ")

(defvar snails-roam-tag-subquery-name "filterd_id")

(defun snails-roam--process-like (word &optional times)
  "add percentage around `word'"
  (interactive)
  (unless times (setq times 1))
  (if (<= times 0)
      word
    (format "%%%s%%" (snails-roam--process-like word (1- times)))))

(defun snails-roam-filter-by-tags (tags)
  "generate query to filter files by tags"
  (interactive)
  ;; tag example: '"link"'
  (concat snails-roam-tags-query
          (mapconcat
           (lambda (x)
             (format "AND tags.node_id in (%s AND tags.tag like '\"%s\"')" snails-roam-tags-query
                     (snails-roam--process-like x 2)))
           tags " ")))

(defun snails-roam-wrap-subquery (subquery query-name)
  "DOCSTRING"
  (interactive)
  (format "WITH %s AS (%s)" query-name subquery))

(defun snails-roam-filter-by-input (input)
  "generate query to filter files by input"
  (interactive)
  (mapconcat
   (lambda (x)
     (format "AND (nodes.title LIKE '%s' or aliases.alias LIKE '%s')"
             ;; org-roam-directory
             (snails-roam--process-like x 2)
             (snails-roam--process-like x 2)))
   (split-string input " ")
   " "))

(defun snails-roam-generate-candidates (input tags &optional len)
  "generate snails candidates"
  (when (or (not len) (> (length input) len))
    (let* ((search-info (snails-roam-processing-input input))
           (input (car search-info))
           (tags (append tags (cdr search-info)))
           res file candidates
           (rows
            (org-roam-db-query
             (format "%s select distinct nodes.file, nodes.title, nodes.pos, nodes.properties from %s left join aliases on aliases.node_id = %s.node_id left join nodes on nodes.id = %s.node_id where 0=0 %s"
                     (snails-roam-wrap-subquery
                      (snails-roam-filter-by-tags tags)
                      snails-roam-tag-subquery-name)
                     snails-roam-tag-subquery-name
                     snails-roam-tag-subquery-name
                     snails-roam-tag-subquery-name
                     (snails-roam-filter-by-input input))))
           (nodes-info
            (mapcar
             (lambda (row)
               (pcase-let* ((`(,file ,title ,pos ,properties)
                             row))
                 `(,(if (> pos 1)
                        (format "%s ⮨ %s"
                                title
                                (file-name-sans-extension
                                 (file-name-nondirectory file)))
                      (file-name-sans-extension
                       (file-name-nondirectory file)))
                   .
                   ,(format
                     "'((file . \"%s\") (point . %s) (link . %s)) " file pos
                     (when-let ((node-link
                                 (cdr (assoc "ROAM_REFS" properties))))
                       (format "\"%s\"" node-link))))))
             rows)))
      (dolist (node-info nodes-info)
        (snails-add-candiate 'candidates
                             (car node-info)
                             (cdr node-info)))
      candidates)))

(defun snails-roam-find-file (node &optional follow-file-path)
  "DOCSTRING"
  (interactive)
  (let ((alist (weiss-read-from-string node)))
    (if-let ((link (cdr (assoc 'link alist)))
             (_ (not follow-file-path)))
        (browse-url link)
      (find-file (cdr (assoc 'file alist))))))

(defun snails-roam-processing-input (input)
  "get tags after separator symbol"
  (if (string-match-p "," input)
      (let ((info (split-string input ","))
            )
        (cons (car info) (split-string (nth 1 info) " ")))
    `(,input)))

(snails-create-sync-backend
 :name "ORG-ROAM-NEW"

 :candidate-filter (lambda
                     (input)
                     (when (> (length input) snails-roam-new-note-length)
                       (let ((candidates)
                             )
                         (snails-add-candiate 'candidates input input))))

 :candidate-do (lambda
                 (candidate)
                 (org-roam-capture-
                  :node (org-roam-node-create :title candidate)
                  :props '(:finalize find-file))
                 ))

(provide 'snails-roam)
