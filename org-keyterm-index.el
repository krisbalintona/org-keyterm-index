;;; org-keyterm-index.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-keyterm-index
;; Keywords: text, convenience
;; Version: 0.1.4
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

;; TODO 2025-05-13: Should I locally bind `org-ml-memoize-match-patterns' for
;; the pattern I use to detect keyword index drawers?
;; FIXME 2025-05-13: Figure out away to have the keyterms picked up ignore the
;; scope boundaries of other index headlines.  So, for example, if I have a
;; buffer-level scope headline, ignore the scope of a subtree-level scope of
;; another headline within that buffer.
(defun org-keyterm-index--new-headline-node (headline &optional scope)
  "Return HEADLINE with an updated keyterm index drawer.
This function returns HEADLINE with the first keyterm index drawer (a
drawer whose :DRAWER-NAME property is the value of
`org-keyterm-index-drawer-name') replaced with an updated keyterm index
drawer org-element.

If there are multiple keyterm index drawers in HEADLINE, mutate only the
first one.  If there are no keyterm index drawers in HEADLINE, then
place one at the end of HEADLINE.

HEADLINE is an org-element headline.

The optional parameter SCOPE is an org-element parse tree for the region
of the buffer that should be scanned for keyterms.  For instance, if
SCOPE is the return value of `org-element-parse-buffer' then the entire
buffer is scanned for keyterms.  If SCOPE is nil, it will be determined
by the value of the headline property whose name is the value of
`org-keyterm-index-property-value-name'.  For a list of valid values for
this property, see the docstring of `org-keyterm-index--get-scope'."
  (let* ((generated-drawer
          (org-keyterm-index-generate-index-drawer (or scope (org-keyterm-index--get-scope headline))))
         ;; Query for only the first keyterm index drawer
         (index-drawer-query `(:first :any (:and drawer (:drawer-name ,org-keyterm-index-drawer-name)))))
    ;; REVIEW 2025-05-13: Is there a more efficient way to do this?  One that
    ;; doesn't call `org-ml-match' then `org-ml-match-map*'?
    (if (org-ml-match index-drawer-query headline)
        ;; Replace index-drawer (the first keyterm index drawer) in headline
        (org-ml-match-map* index-drawer-query
          (org-ml-set-property :post-blank (org-ml-get-property :post-blank it) generated-drawer)
          headline)
      ;; When there isn't already an existing keyterm index drawer, append an
      ;; updated one to headline
      (let* ((section (org-ml-headline-get-section headline)) ; We use section to consider :post-blank of property drawer
             (last-child-post-blank (org-ml-get-property :post-blank (car (last section))))
             (new-drawer (org-ml-set-property :post-blank (or last-child-post-blank 0) generated-drawer)))
        (org-ml-headline-set-section (append section (list new-drawer)) headline)))))

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
;;;###autoload
(defun org-keyterm-index-update-headline-at-point ()
  "Update the headline at point\\='s keyterm index.
The headline will only be changed if the it has a valid value for the
property whose name is the value of
`org-keyterm-index-property-value-name'.  See the docstring of
`org-keyterm-index--get-scope' for the possible values for this
property."
  (interactive)
  ;; We aren't binding the intermediate nodes during our process of modifying
  ;; the org-element nodes, so we don't have to worry about side effects to
  ;; variables these nodes are bound to.  Therefore, we can safely use org-ml
  ;; impurely for performance gains with no downside.  See
  ;; https://github.com/ndwarshuis/org-ml?tab=readme-ov-file#node-copying for
  ;; more information on using impure versions of org-ml functions.
  (let ((headline-at-point (org-ml-parse-this-headline)))
    (when (org-keyterm-index--get-scope headline-at-point)
      (org-ml->> headline-at-point
        (org-keyterm-index--new-headline-node)
        (org-ml-update-this-headline*)))))

;; TODO 2025-05-08: Handle narrowed buffers?
;;;###autoload
(defun org-keyterm-index-update-buffer ()
  "Update every headline\\='s keyterm index in this buffer.
Headlines which will be updated are those which have a valid value for
the property designated by the value of
`org-keyterm-index-property-value-name'.  See the docstring of
`org-keyterm-index--get-scope' for the possible values for this
property."
  (interactive)
  ;; We aren't binding the intermediate nodes during our process of modifying
  ;; the org-element nodes, so we don't have to worry about side effects to
  ;; variables these nodes are bound to.  Therefore, we can safely use org-ml
  ;; impurely for performance gains with no downside.  See
  ;; https://github.com/ndwarshuis/org-ml?tab=readme-ov-file#node-copying for
  ;; more information on using impure versions of org-ml functions.
  (org-ml->> (org-ml-parse-this-buffer)
    ;; Only check headlines where with a valid property value (i.e.,
    ;; `org-keyterm-index--get-scope' returns non-nil)
    (org-ml-match-map* `(:any * (:and headline (:pred org-keyterm-index--get-scope)))
      (org-keyterm-index--new-headline-node it))
    ;; TODO 2025-05-13: We use `org-ml-update-this-buffer*' to leverage the
    ;; Myers diff algorithm, but can we avoid having to diff the entire buffer?
    ;; This can become very expensive because the algorithm is quadratic in
    ;; complexity.  Maybe we should use `org-ml-update-headline-at*' on every
    ;; headline in the buffer from end to beginning (to prevent headlines
    ;; updated later from having their boundaries become stale)?
    (org-ml-update-this-buffer*)))

;;; Provide
(provide 'org-keyterm-index)
;;; org-keyterm-index.el ends here
