;;; helm-librarian.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar librarian-executable
  "librarian"
  "Librarian executable.")

(defvar librarian-library-directory
  "~/library"
  "Library directory.")

;; TODO this should be something like "@title@ (@author@) etc." And it should be a defvar.
(defun helm-librarian//candidate-display (candidate)
  "Used to generate the string display for each resource in the helm buffer.
CANDIDATE is a hash table corresponding to the json-parsed resource."
  (gethash "title" candidate))

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
             (concat librarian-executable
                     " -d "
                     librarian-library-directory
                     " search "
                     helm-pattern))))))
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
