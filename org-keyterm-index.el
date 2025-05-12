;;; org-keyterm-index.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-keyterm-index
;; Keywords: text, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (org-ml "6.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Like `org-make-toc' but to create book-like indices (for keyterm-page pairs).

;;; Code:
(require 'org-ml)

;;; Variables

;;;; Options
(defgroup org-keyterm-index ()
  "Create a keyterm index in an `org-mode' buffer."
  :group 'org-mode
  :prefix "org-keyterm-index-")

(defcustom org-keyterm-index-keyword-name "KEYTERM"
  "Name of the org keyterm used to store the index contents.
For instance, a value of \"KEYTERM\" means that #+KEYTERM: will
associate a keyterm with pages.

Capitalization has no effect."
  :type 'string)

(defcustom org-keyterm-index-drawer-name "KEYTERM_INDEX"
  "Name of the drawer used to store the index contents."
  :type 'string)

;;;; Internal
(defconst org-keyterm-index-property-value-name "KEYTERM_INDEX"
  "Name of the drawer used to store the index contents.")

;;; Functions

;; TODO 2025-05-08: Allow several formatting options? Plain list and table come
;; to mind.
(defun org-keyterm-index-generate-index-drawer (parse-tree)
  "Return an org-element drawer for keyterms in PARSE-TREE.
If there are no keyterms to inside PARSE-TREE, return nil.

PARSE-TREE is the org-element parse tree scanned for keyterms."
  (let ((index-table (make-hash-table :test #'equal))
        list-items)
    ;; FIXME 2025-05-12: Not sure why `org-ml-match-do*' does not work.  Created
    ;; issue upstream: https://github.com/ndwarshuis/org-ml/issues/49
    (org-ml-match-do `(:any * (:and keyword (:key ,org-keyterm-index-keyword-name)))
      ;; TODO 2025-05-13: Handle cases in which user strings are erroneous
      (lambda (keyword-element)
        (let* ((components (mapcar #'string-trim (string-split (org-ml-get-property :value keyword-element) "::")))
               (keyterm (car components))
               (pages (cdr components)))
          (puthash keyterm (append pages (gethash keyterm index-table)) index-table)))
      parse-tree)
    (maphash
     (lambda (keyterm pages)
       (push (org-ml-build-item! :bullet '-
                                 :tag keyterm
                                 :paragraph (string-join (reverse pages) ", "))
             list-items))
     index-table)
    (when list-items
      (org-ml-build-drawer org-keyterm-index-drawer-name (apply #'org-ml-build-plain-list list-items)))))

(defun org-keyterm-index-update-headline (headline scope)
  "Update HEADLINE\\='s keyterm index.
This function replaces the first drawer whose :DRAWER-NAME property is
the value of `org-keyterm-index-drawer-name' with a version listing the
keyterms within SCOPE.

HEADLINE is an org-element headline.

SCOPE is an org-element parse tree for the region of the buffer that
should be scanned for keyterms.  For instance, if SCOPE is the return
value of `org-element-parse-buffer' then the entire buffer is scanned
for keyterms.

Return the org-element of the newly updated drawer.  If the drawer was
not updated, return do not change the buffer contents and return nil.
If there are multiple drawers whose :DRAWER-NAME property is the value
of `org-keyterm-index-drawer-name', update only the first one.  If there
are no keyterm index drawers in this headline, then insert one at the
end of it."
  (when-let* ((index-drawer
               ;; Get only the first keyterm index drawer
               (car (org-ml-match `(:first :any (:and drawer (:drawer-name ,org-keyterm-index-drawer-name)))
                                  headline)))
              (index-drawer-begin (org-ml-get-property :begin index-drawer))
              (updated-drawer
               (org-ml-set-property :post-blank (org-ml-get-property :post-blank index-drawer)
                                    (org-keyterm-index-generate-index-drawer scope))))
    (unless (string= (org-ml-to-trimmed-string updated-drawer) (org-ml-to-trimmed-string index-drawer))
      (org-ml-update-element-at* index-drawer-begin
        (org-ml-set-property :post-blank (org-ml-get-property :post-blank index-drawer) updated-drawer))
      (message "Updated drawer at point %s" index-drawer-begin)
      updated-drawer)))

(defun org-keyterm-index--get-scope (headline)
  "Get keyterm index scope of HEADLINE.
HEADLINE is an org-element headline.  Return the parse tree
corresponding to the value of the heading property whose name is the
value of `org-keyterm-index-property-value-name'.

The possible property values are:
- \"buffer\": the entire buffer
- \"t\": the entire buffer
- \"subtree\": the subtree of HEADLINE and its subheadings

If there is no such property, or if its value is erroneous, return nil."
  (pcase (org-ml-headline-get-node-property org-keyterm-index-property-value-name headline)
    ((or "buffer" "t") (org-ml-parse-this-buffer))
    ("subtree" (org-ml-parse-subtree-at (org-ml-get-property :begin headline)))))

;;; Commands
;; TODO 2025-05-08: Handle narrowed buffers?
(defun org-keyterm-index-update-buffer ()
  "Update every headline\\='s keyterm index in this buffer.
Headlines which will be updated are those which have a valid value for
the property designated by the value of
`org-keyterm-index-property-value-name'.  See the docstring of
`org-keyterm-index--get-scope' for the possible values for this
property."
  (interactive)
  ;; FIXME 2025-05-12: Not sure why `org-ml-match-do*' does not work.  Created
  ;; issue upstream: https://github.com/ndwarshuis/org-ml/issues/49
  (let ((count 0))
    (org-ml-match-do `(:any * headline)
      (lambda (headline)
        (when (org-keyterm-index-update-headline headline (org-keyterm-index--get-scope headline))
          (setq count (1+ count))))
      (org-ml-parse-this-buffer))
    (if (plusp count)
        (message "Updated %s headlines' keyterm index in the buffer!" count)
      (message "Buffer's keyterm index drawers already up-to-date"))))

;;; Provide
(provide 'org-keyterm-index)
;;; org-keyterm-index.el ends here
