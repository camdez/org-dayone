;;; org-dayone.el --- Import Day One exports into Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Cameron Desautels

;; Author: Cameron Desautels <camdez@gmail.com>
;; Package-Requires: ((emacs "27") (parse-csv "20160512.1723"))
;; Keywords: org, org-mode
;; Homepage: http://github.com/camdez/org-dayone

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;

;;; Code:

(require 'parse-csv)

(defvar org-dayone-csv-file nil
  "Default location to look for a Day One CSV journal export..")

(defvar org-dayone-photo-directory nil
  "Default location to look for Day One photos.
If nil, photos will not be added to imported entries.")

(defvar org-dayone-property-columns
  '("uuid" "date" "modifiedDate" "timeZoneIdentifier" "latitude" "longitude"
    "placeName" "localityName" "administrativeArea" "country")
  "Columns to retain as header properties.
Generated properties will be named like: `DAYONE_COLUMNNAME'.")

(defvar org-dayone-property-generators
  `(("SOURCE" . (lambda (_entry) "Day One"))
    ;; DEBUG: example
    ("ADDRESS" . (lambda (entry)
                   (format "%s, %s, %s, %s"
                           (gethash "placeName" entry)
                           (gethash "localityName" entry)
                           (gethash "administrativeArea" entry)
                           (gethash "country" entry)))))
  "Alist of custom Org property generators.

Keys are property names, values are 1-arity functions which take
the entry (hash-map) and return a property value.  nil properties
will not be added (use the empty string if you want to insert
empty properties).")

(defun org-dayone--parse-csv-file (f)
  "Parse Day One CSV export F to entry hash-maps."
  (let* ((data (parse-csv-string-rows
                (with-temp-buffer
                  (insert-file-contents f)
                  (buffer-string))
                ?\,
                ?\"
                "\n"))
         (headers (first data))
         (rows (rest data)))
    (mapcar (lambda (row)
              (let ((h (make-hash-table :test 'equal)))
                (cl-mapc (lambda (k v)
                           (puthash k v h))
                         headers
                         row)
                h))
            rows)))

(defun org-dayone--decoded-date-to-calendar-date (d)
  "Convert decoded date D to format used by Org Mode."
  (list
   (nth 4 d)
   (nth 3 d)
   (nth 5 d)))

(defun org-dayone--process-text (text)
  "Tranform Day One entry TEXT to Org Mode format."
  (thread-last text
    (replace-regexp-in-string "^•" "  -")
    (replace-regexp-in-string "^---$" "-----")
    (replace-regexp-in-string "\\[\\(.+?\\)\\](\\(.+?\\))" "[[\\2][\\1]]")))

;; Assumes point is on a heading
(defun org-dayone--heading-has-contents-p ()
  "Report if Org heading at point has any contents or is empty."
  (save-excursion
    (outline-end-of-heading)
    (let ((start (point)))
      (outline-next-preface)
      (re-search-backward "\\w" start t))))

(defun org-dayone--import-entry (new? conflicts root-pos photo-dir entry)
  "Import parsed Day One ENTRY into current buffer.

NEW? indicates if the output file is newly created (empty),
  enabling some optimizations.

CONFLICTS signifies how to handle entry UUID collisions.

ROOT-POS, if not nil, specifies the location of the existing Org
  header to insert the datetree under.

PHOTO-DIR names a directory containing photos named after the
entry UUIDs.  If nil, no photos will be inserted."
  (let* ((entry-date   (iso8601-parse (gethash "date" entry)))
         (uuid         (gethash "uuid" entry))
         (existing-pos (unless new?     ; optimization
                         (org-find-property "DAYONE_UUID" uuid)))
         (continue?    (or (not existing-pos)
                           (pcase conflicts
                             ('replace (goto-char existing-pos)
                                       (org-cut-subtree)
                                       t)
                             ('skip    (message "Skipping entry %s" uuid)
                                       nil)
                             (_        (error "Invalid UUID conflict resolution strategy '%s'"
                                              conflicts))))))
    (when continue?
      (when root-pos
        (goto-char root-pos))
      (org-datetree-find-date-create
       (org-dayone--decoded-date-to-calendar-date entry-date)
       (when root-pos
         'subtree-at-point))
      ;; If date already exists in datetree, make a new heading
      ;; below the current one.
      (when (org-dayone--heading-has-contents-p) ; not newly created
        (let* ((headline-text (nth 4 (org-heading-components)))
               (new-headline-text (format "%s (%s)" headline-text uuid)))
          (org-insert-heading-after-current) ; inserts a newline after, datetree sometimes doesn't
          (insert new-headline-text)
          (org-back-to-heading)
          (unless (looking-back "\n\n")
            (newline))))
      ;; Fill out date heading properties and body
      (org-end-of-meta-data t)
      (when photo-dir
        (let ((photo-file (expand-file-name (concat uuid ".jpg") photo-dir)))
          (when (file-exists-p photo-file)
            (insert (format "[[%s]]\n\n" photo-file)))))
      (insert (org-dayone--process-text (gethash "text" entry)))
      (unless (looking-back "\n")
        (newline))
      (backward-char) ; keep us in the right header
      (mapc (lambda (prop)
              (let ((prop-val (gethash prop entry)))
                (when (and prop-val (not (string-blank-p prop-val)))
                  (org-set-property (format "DAYONE_%s" (upcase prop))
                                    prop-val))))
            org-dayone-property-columns)
      (mapc (lambda (el)
              (when-let (val (funcall (cdr el) entry))
                (org-set-property (car el) val)))
            org-dayone-property-generators))))

(defun org-dayone-import (&optional csv-file photo-dir org-file conflicts olp)
  "Import Day One CSV export CSV-FILE into Org Mode as a datetree.

If not provided, CSV-FILE defaults to the value of
  `org-dayone-csv-file'.  If nil, prompts for a file.

PHOTO-DIR names a directory containing .jpeg files named after
  entry UUIDs (this can be exported from Day One).  If not
  provided, defaults to the value of
  `org-dayone-photo-directory'.  If nil, photos will not be
  inserted into entries.

ORG-FILE names an existing Org file to merge the entries into.
  If nil, a new buffer will be created.

CONFLICTS is a symbol specifying the strategy for dealing with
  UUID conflicts between existing entries in the Org file and
  entries being imported (based on the DAYONE_UUID property).  Should
  be either `skip' or `replace'.  Defaults to `skip'.

OLP specifies an `org-capture'-style outline path to an existing
  headline under which the datetree should be inserted
  (e.g. `(\"Journals\" \"Day One\")'.  Import will abort if matching
  headline cannot be found.  If nil, datetree will be inserted at the
  top level."
  (interactive)
  (let* ((csv-file  (or csv-file
                        org-dayone-csv-file
                        (read-file-name "CSV file to import: " nil nil t)))
         (photo-dir (or photo-dir
                        org-dayone-photo-directory))
         (new?      (or (not org-file)
                        (not (file-exists-p org-file))))
         (conflicts (if (eq 'replace conflicts)
                        'replace
                      'skip))
         (buf       (if org-file
                        (find-file-other-window org-file)
                      (generate-new-buffer "*day-one*")))
         (root-pos  (when (and olp (not new?))
                      (with-current-buffer buf
                        (org-find-olp olp t))))
         (entries   (thread-last csv-file
                      (org-dayone--parse-csv-file)
                      ;; Parser seems to return some blank entries at the
                      ;; end of the file.
                      (seq-filter (apply-partially #'gethash "uuid")))))
    (with-current-buffer buf
      (when new?
        (org-mode)
        (insert (format "#+TITLE: Day One Journal\n\n%d entries imported "
                        (length entries)))
        (org-time-stamp '(16) t)
        (insert ".\n\n"))
      (mapc (apply-partially #'org-dayone--import-entry new? conflicts root-pos photo-dir) entries)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "%d entries imported from %s" (length entries) csv-file)))

(provide 'org-dayone)
;;; org-dayone.el ends here
