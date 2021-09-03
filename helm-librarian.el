;;; helm-librarian.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'helm)
(require 'helm-source)

(defvar librarian-executable
  "librarian"
  "Librarian executable.")

(defvar librarian-library-directory
  "~/library"
  "Library directory.")

(defvar librarian-display-string-format
  "@title@ (@authors[0].last@, @date.year@) [@content_type@/@document_type@]"
  "Specifies how each candidate should be displayed in the helm buffer.
Parts of the string between '@' are replaced with their
corresponding resource value..")

(defun helm-librarian//field-value (field resource)
  "Get the field value for a resource.
FIELD is a string specifier of the field and RESOURCE is a hash
table of the resource.  FIELD can accomodate embedded lists and
hash tables.  To get an element of a list specify the index with
[index] (e.g., authors[0]).  To specify the key of a hash table,
use a dot ('.') followed by the key name (e.g.,
authors[0].last)."
  ;; Separate `field' into keys. Then, use each key (and the
  ;; associated list index, if provided) to restrict the original
  ;; resource hash, `resource' to the subhash corresponding to the
  ;; value of the key.
  (let* ((field-list (split-string field "\\.")))
    (dolist (field field-list)
      ;; List elements can be hash tables. Normally, we access some
      ;; desired element of the list and then use the key to get the
      ;; desired value. But, the list can be empty in which case it's
      ;; not possible to access a value of a non-existant item. We
      ;; signal this within the loop by setting `resource' to an empty
      ;; string. Here (at the beginning of the iteration), we only
      ;; perform the iteration if `resource' is not currently the
      ;; empty string. TODO it would actually be more efficient to
      ;; break out of the loop, but I'm not sure this is possible with
      ;; `dolist'.
      (if (not (eq resource ""))
          (let* ((key-index (split-string field "\\["))
                 (key (car key-index)))
            ;; "\\[" didn't match if list has 1 element
            (if (not (eq (length key-index) 1))
                (let ((index (string-to-number (nth 0 (split-string (nth 1 key-index) "\\]"))))
                      (value (gethash key resource)))
                  (if (seq-empty-p value)
                      (setq resource "")
                    (setq resource (elt (gethash key resource) index))))
              (setq resource (gethash key resource)))))))
  ;; Replace null values with an empty string.
  (if (eq resource :null)
      ""
    (if (numberp resource)
        (number-to-string resource)
      resource)))

(defun helm-librarian//candidate-display (candidate)
  "Used to generate the string display for each resource in the helm buffer.
CANDIDATE is a hash table corresponding to the json-parsed resource."
  ;; `format-list' contains a list of substrings of the format
  ;; specifier. All elements at odd indices correspond to parts of the
  ;; string that had been delimited by '@'.
  (let ((format-list (split-string librarian-display-string-format "@"))
        (index 0)
        (display-string ""))
    (while (< index (length format-list))
      (setq display-string
            (concat display-string
                    (if (eq (mod index 2) 0)
                        (nth index format-list)
                      (helm-librarian//field-value (nth index format-list) candidate))))
      (setq index (+ 1 index)))
    display-string))

;; TODO this should handle websites
(defun helm-librarian//open-resource-file (candidate)
  "Open the file corresponding to the resource CANDIDATE.
The resource is formatted as a hash table as returned by `json-parse-string'."
  (find-file (concat librarian-library-directory
                     "/resources/"
                     (elt (gethash "historical_checksums" candidate) 0))))

(defun helm-librarian//candidates ()
  "Helm candidates.
Returns an alist with the first item"
  (let ((json-parse-result
         (ignore-errors
           (json-parse-string
            (shell-command-to-string
             ;; TODO the quotes will prevent doing something like
             ;; r"search string".
             (concat librarian-executable
                     " -d "
                     librarian-library-directory
                     " search \""
                     helm-pattern
                     "\""))))))
    (if json-parse-result
        (mapcar (lambda (result)
                  `(,(helm-librarian//candidate-display result) . ,result))
                json-parse-result))))

(defun helm-librarian//action (candidate)
  "Helm action performed when selecting CANDIDATE."
  (helm-librarian//open-resource-file candidate))

(defun helm-librarian ()
  "Helm completions for librarian's search command."
  (interactive)
  (helm
   :sources (helm-build-sync-source "librarian"
              :match-dynamic t
              :candidates 'helm-librarian//candidates
              :action 'helm-librarian//action
              :volatile t)
   :buffer "*helm librarian*"
   :prompt "librarian search: "))

(provide 'helm-librarian)

;;; helm-librarian.el ends here
