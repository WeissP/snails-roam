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

(defun snails-roam--process-like (word)
  "add percentage around `word'"
  (interactive)
  (format "%%%s%%" word))

(defun snails-roam-filter-by-tags (tags)
  "generate query to filter files by tags"
  (interactive)
  (concat snails-roam-tags-query
          (mapconcat
           (lambda (x)
             (format "AND tags.node_id in (%s AND tags.tag = '\"%s\"')" snails-roam-tags-query x))
           tags " ")))


(defun snails-roam-wrap-subquery (subquery query-name)
  "DOCSTRING"
  (interactive)
  (format "WITH %s AS (%s)" query-name subquery))

(defun snails-roam-filter-by-input (input)
  "generate query to filter files by input"
  (interactive)
  (mapconcat
   (lambda (x) (format "AND nodes.file LIKE '%%%%%s%%%%'" x))
   (split-string input " ")
   " "))

(defun snails-roam-generate-candidates (input tags &optional len)
  "generate snails candidates"
  (when (or (not len) (> (length input) len))
    (let ((tags
           (append tags (snails-roam-pick-tags-from-input input)))
          res file candidates)
      (setq res
            (org-roam-db-query
             (format "%s select distinct nodes.file from nodes, %s where nodes.id in %s %s"
                     (snails-roam-wrap-subquery
                      (snails-roam-filter-by-tags tags)
                      snails-roam-tag-subquery-name)
                     snails-roam-tag-subquery-name
                     snails-roam-tag-subquery-name
                     (snails-roam-filter-by-input input))))
      (dolist (x res)
        (setq file (car x))
        (snails-add-candiate 'candidates
                             (file-name-sans-extension
                              (file-name-nondirectory file))
                             file))
      (snails-sort-candidates input candidates 0 0)
      candidates)))

(defun snails-roam-pick-tags-from-input (input)
  "get tags after separator symbol"
  (when (string-match-p "," input) (split-string input ",")))

(snails-create-sync-backend
 :name "ORG-ROAM-NEW"

 :candidate-filter (lambda
                     (input)
                     (when (> (length input) snails-roam-new-note-length)
                       (let ((candidates)
                             )
                         (snails-add-candiate 'candidates input input))))

 :candidate-do (lambda (candidate) (org-roam-find-file candidate nil nil t)))

(provide 'snails-roam)
