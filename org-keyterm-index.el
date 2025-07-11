;;; org-keyterm-index.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-keyterm-index
;; Keywords: text, convenience
;; Version: 0.1.4
;; Package-Requires: ((emacs "30.1") (org-ml "6.0.2"))

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
(defun org-keyterm-index-generate-index-drawer (parse-tree sorting-type)
  "Return an org-element drawer for keyterms in PARSE-TREE.
If there are no keyterms to inside PARSE-TREE, return nil.

PARSE-TREE is the org-element parse tree scanned for keyterms.

SORTING-TYPE is provided, must be a symbol indicating the kind of
sorting to do on the generated index drawer's org list.  There are two
available sorting types:
- alphabetical: sort list items alphabetically
- chronological: sort list items chronologically, as they appear in the
  buffer"
  (let ((index-table (make-hash-table :test #'equal))
        list-items)
    ;; Collect keyterm-pages pairs
    (org-ml-match-do*
     `(:any * (:and keyword (:key ,org-keyterm-index-keyword-name)))
     ;; TODO 2025-05-13: Handle cases in which user strings are erroneous
     (let* ((components (mapcar #'string-trim (string-split (org-ml-get-property :value it) "::")))
            (keyterm (car components))
            (pages (cdr components)))
       (puthash keyterm (append (gethash keyterm index-table) pages) index-table))
     parse-tree)
    ;; Build org bullet point list items
    (maphash
     (lambda (keyterm pages)
       (push (org-ml-build-item! :tag keyterm :paragraph (string-join pages ", "))
             list-items))
     index-table)
    ;; Sort org bullet point list items
    (setq list-items
          (pcase sorting-type
            ;; ('alphabetical (sort list-items #'string<))
            ('alphabetical (sort list-items
                                 ;; FIXME 2025-07-11: Is there a way
                                 ;; to sort without converting the org
                                 ;; element node into strings?
                                 :key (lambda (node) (org-ml-to-string node))
                                 :lessp #'string<))
            ('chronological (nreverse list-items))))
    ;; Build and return final drawer
    (when list-items
      (org-ml-build-drawer org-keyterm-index-drawer-name (apply #'org-ml-build-plain-list list-items)))))

(defun org-keyterm-index--get-settings (headline)
  "Get a list of the keyterm index settings of HEADLINE.
HEADLINE is an org-element headline.  Given HEADLINE, get the value of
the property whose name is the value of
`org-keyterm-index-property-value-name'.  If there is no such property,
return nil.  If such a property exists, take the value of that property
and split it by spaces, returning the resulting list."
  (when-let* ((value (org-ml-headline-get-node-property org-keyterm-index-property-value-name headline)))
    (string-split value nil t)))

(defun org-keyterm-index--get-scope (headline)
  "Get keyterm index scope of HEADLINE.
HEADLINE is an org-element headline.  Return the parse tree
corresponding to the scope indicated by HEADLINE\\='s keyterm index
settings.

The possible scope settings are:
- \"buffer\": the entire buffer
- \"t\": the entire buffer
- \"subtree\": the subtree of HEADLINE and its subheadings

If there is no such property, or if its value is erroneous, return nil."
  (when-let* ((settings (org-keyterm-index--get-settings headline)))
    (pcase (car settings)
      ((or "buffer" "t") (org-ml-parse-this-buffer))
      ("subtree" (org-ml-parse-subtree-at (org-ml-get-property :begin headline))))))

(defun org-keyterm-index--get-sorting-type (headline)
  "Get keyterm index sorting type of HEADLINE.
HEADLINE is an org-element headline.  For HEADLINE, return the sorting
type, as a symbol.

The possible sorting types are:
- \\='chronological: the entire buffer
- \\='alphabetical: the entire buffer
If there is no sorting type specified, default to the \\='chronological
sorting type.

If there is no such property, or if its value is erroneous, return nil."
  (when-let* ((settings (org-keyterm-index--get-settings headline)))
    (pcase (nth 1 settings)
      ("alphabetical" 'alphabetical)
      ((or "chronological" "nil" (pred not)) 'chronological))))

;; REVIEW 2025-07-11: Should the SCOPE parameter be removed?
;; TODO 2025-05-13: Should I locally bind `org-ml-memoize-match-patterns' for
;; the pattern I use to detect keyword index drawers?
;; FIXME 2025-05-13: Figure out away to have the keyterms picked up ignore the
;; scope boundaries of other index headlines.  So, for example, if I have a
;; buffer-level scope headline, ignore the scope of a subtree-level scope of
;; another headline within that buffer.
(defun org-keyterm-index--updated-headline (headline &optional scope)
  "Return HEADLINE with an updated keyterm index drawer.
HEADLINE is an org-element headline.

A keyterm index drawer is an org drawer drawer whose :DRAWER-NAME
property is the value of `org-keyterm-index-drawer-name'.

This function returns HEADLINE with the first keyterm index drawer
replaced with an updated keyterm index drawer org-element.  If there are
no keyterm index drawers in HEADLINE, then return HEADLINE with a new
keyterm index drawer at the end of its contents (i.e., before any
existing subheadings).

The optional parameter SCOPE is an org-element parse tree for the region
of the buffer that should be scanned for keyterms.  For instance, if
SCOPE is the return value of `org-element-parse-buffer' then the entire
buffer is scanned for keyterms.  If SCOPE is nil, it will be determined
by the value of the headline property whose name is the value of
`org-keyterm-index-property-value-name'.  For a list of valid values for
this property, see the docstring of `org-keyterm-index--get-scope'."
  (let* ((index-scope (or scope (org-keyterm-index--get-scope headline)))
         (index-sorting-type (org-keyterm-index--get-sorting-type headline))
         (generated-drawer
          (org-keyterm-index-generate-index-drawer index-scope index-sorting-type))
         ;; Query for only the first keyterm index drawer
         (index-drawer-query `(:first :any (:and drawer (:drawer-name ,org-keyterm-index-drawer-name)))))
    ;; REVIEW 2025-05-13: Is there a more efficient way to do this?
    ;; One that doesn't call `org-ml-match' then `org-ml-match-map*'?
    (if (org-ml-match index-drawer-query headline)
        ;; Replace index-drawer (the first keyterm index drawer) in
        ;; headline
        (org-ml-match-map* index-drawer-query
          (org-ml-set-property :post-blank (org-ml-get-property :post-blank it) generated-drawer)
          headline)
      ;; When there isn't already an existing keyterm index drawer,
      ;; add an updated one to the end of the content of HEADLINE
      (let* ((supercontents (org-ml-headline-get-supercontents nil headline))
             (post-blank (or (org-ml-get-property :post-blank (car (last (org-ml-headline-get-contents nil headline))))
                             (plist-get supercontents :blank)
                             0))
             (final-drawer (org-ml-set-property :post-blank post-blank generated-drawer)))
        ;; We use supercontents because its :contents is the content
        ;; of the headline excluding subheadings, if HEADLINE has any
        ;; (i.e., if HEADLINE is a subtree)
        (org-ml-headline-set-supercontents
         nil
         (org-ml-supercontents-set-contents
          (append (org-ml-supercontents-get-contents supercontents)
                  (list final-drawer))
          supercontents)
         headline)))))

;;; Commands
;;;###autoload
(defun org-keyterm-index-update-headline-at-point ()
  "Update the headline at point\\='s keyterm index.
The headline will only be changed if the it has a valid value for the
property whose name is the value of
`org-keyterm-index-property-value-name'.  See the docstring of
`org-keyterm-index--get-scope' for the possible values for this
property."
  (interactive)
  (org-with-wide-buffer
   ;; We must pass then update the subtree instead of just the heading
   ;; since updating just the heading will effectively erase any
   ;; pre-existing subheadings
   (if-let* ((subtree-at-point (org-ml-parse-this-subtree)))
       (when (org-keyterm-index--get-scope subtree-at-point)
         (org-ml->> subtree-at-point
           (org-keyterm-index--updated-headline)
           (org-ml-update-this-subtree*)))
     (message "No subtree at point"))))

;;;###autoload
(defun org-keyterm-index-update-buffer ()
  "Update every headline\\='s keyterm index in this buffer.
Headlines which will be updated are those which have a valid value for
the property designated by the value of
`org-keyterm-index-property-value-name'.  See the docstring of
`org-keyterm-index--get-scope' for the possible values for this
property."
  (interactive)
  (org-with-wide-buffer
   (org-ml->> (org-ml-parse-this-buffer)
     ;; Only check headlines where with a valid property value (i.e.,
     ;; `org-keyterm-index--get-scope' returns non-nil)
     (org-ml-match-map* `(:any * (:and headline (:pred org-keyterm-index--get-scope)))
       (org-keyterm-index--updated-headline it))
     ;; TODO 2025-05-13: We use `org-ml-update-this-buffer*' to
     ;; leverage the Myers diff algorithm, but can we avoid having to
     ;; diff the entire buffer?  This can become very expensive
     ;; because the algorithm is quadratic in complexity.  Maybe we
     ;; should use `org-ml-update-headline-at*' on every headline in
     ;; the buffer from end to beginning (to prevent headlines updated
     ;; later from having their boundaries become stale)?
     (org-ml-update-this-buffer*))))

;;; Provide
(provide 'org-keyterm-index)
;;; org-keyterm-index.el ends here
