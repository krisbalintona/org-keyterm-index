;;; org-keyterm-index.el --- Implementation of Oblique Strategies  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-keyterm-index
;; Keywords: text, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))


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
(require 'org-element)

;;; Variables

;;;; Options
(defgroup org-keyterm-index ()
  "Create a keyterm index in an `org-mode' buffer."
  :group 'org-mode
  :prefix "org-keyterm-index-")

(defcustom org-keyterm-index-keyterm-name "KEYTERM"
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

(defconst org-keyterm-index-drawer-start-regexp
  (rx bol (0+ blank) ":" (literal org-keyterm-index-drawer-name)":" (0+ blank) eol)
  "Regular expression for the beginning of the index drawer.
Based on the drawer name supplied by `org-keyterm-index-drawer-name'.")

;;; Functions

;; REVIEW 2025-05-11: Don't use any insert functions, only modify the AST then
;; interpret it?

;; TODO 2025-05-08: Allow several formatting options? Plain list and table come
;; to mind.

(defun org-keyterm-index-generate-index (parse-tree)
  "Return the keyterm index as a string.
PARSE-TREE is the org-element parse tree scanned for keyterms."
  (let* ((index-table (make-hash-table :test #'equal))
         lines)
    (org-element-map parse-tree '(keyword)
      (lambda (keyword-element)
        (when (string-equal-ignore-case (org-element-property :key keyword-element)
                                        org-keyterm-index-keyterm-name)
          (let* ((components
                  (mapcar #'string-trim (string-split (org-element-property :value keyword-element) "::")))
                 (keyterm (car components))
                 ;; TODO 2025-05-08: Handle case of no pages
                 (pages (cdr components)))
            (puthash keyterm (append pages (gethash keyterm index-table)) index-table)))))
    (maphash
     (lambda (keyterm pages)
       (push (format "- %s :: %s"
                     keyterm
                     (string-join (reverse pages) ", "))
             lines))
     index-table)
    (string-join (sort lines #'string<) "\n")))

(defun org-keyterm-index--replace-drawer-contents (drawer new-contents)
  "Replace the contents of DRAWER with NEW-CONTENTS.
DRAWER is an org-element drawer whose text will be replaced with
NEW-CONTENTS."
  (when (string-equal-ignore-case (org-element-property :drawer-name drawer) org-keyterm-index-drawer-name)
    (let ((beg (org-element-begin drawer))
          (end (- (org-element-end drawer) (org-element-post-blank drawer)))
          ;; For cleanliness, we ensure that the drawer block's delimiters are
          ;; capitalized
          (drawer-text
           (concat ":" (upcase org-keyterm-index-drawer-name) ":\n" new-contents "\n:END:\n")))
      (if (and beg end)
          (replace-region-contents beg end drawer-text)
        ;; When there are no contents in the drawer
        (save-excursion
          (goto-char beg)
          (forward-line 1)
          (insert drawer-text))))
    t))

(defun org-keyterm-index--update-headline-index (headline scope)
  "Update HEADLINE\\='s keyterm index.
HEADLINE is an org-element headline.

SCOPE is an org-element parse tree for the region of the buffer that
should be scanned for keyterms.  For instance, if SCOPE is the return
value of `org-element-parse-buffer' then the entire buffer is scanned
for keyterms."
  (let ((new-contents (org-keyterm-index-generate-index scope)))
    (or
     ;; Replace the keyterm index drawer if one already exists
     (org-element-map headline '(drawer)
       (lambda (drawer)
         (org-keyterm-index--replace-drawer-contents drawer new-contents))
       nil 'first-match)) ; We assume that there is only one index drawer per headline
    ;; If there is no keyterm index drawer, then insert one to the end of the
    ;; headline
    (goto-char (- (org-element-contents-end headline) (org-element-post-blank headline)))
    (insert ":" (upcase org-keyterm-index-drawer-name) ":\n" new-contents "\n:END:\n"))
  t)

;;; Commands
;; TODO 2025-05-08: Handle narrowed buffers
(defun org-keyterm-index-update-buffer ()
  "Insert or update an index under a heading marked with :index: t, using a drawer."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (info
          (org-element-ast-map ast '(headline)
            (lambda (headline)
              (let ((val (org-element-property
                          (intern (concat ":" org-keyterm-index-property-value-name))
                          ;; FIXME 2025-05-11: Do I need to 'force-undefer?
                          headline nil 'force-undefer)))
                (pcase val
                  ((or "buffer" "t") (cons headline ast))
                  ("subtree"
                   (save-restriction
                     (org-narrow-to-subtree headline)
                     (cons headline (org-element-parse-buffer))))
                  ("nil" (throw :org-element-skip t))
                  ;; Disregard any other value
                  (_ (throw :org-element-skip t))))))))
    ;; REVIEW 2025-05-11: Can we do this with only the AST so we don't have to
    ;; do iterate in reverse order?
    ;; We insert or replace the drawers starting with the last matched headline
    ;; and ending with the first matched headline.  The reason is because the
    ;; points cached in the AST change if we insert/replace contents.  But these
    ;; changes only affect points later in the buffer, so we start at the end so
    ;; earlier points aren't effected.
    (dolist (pair (reverse info))
      (org-keyterm-index--update-headline-index (car pair) (cdr pair))))
  t)

;;; Provide
(provide 'org-keyterm-index)
;;; org-keyterm-index.el ends here
