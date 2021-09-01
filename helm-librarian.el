;;; helm-librarian.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar librarian-executable
  "librarian"
  "Librarian executable.")

(defvar librarian-library-directory
  "~/library"
  "Library directory.")

(setq librarian-executable "/home/matt/src/librarian/target/release/librarian")
(setq librarian-library-directory "~/doc/library")

(defun helm-librarian//candidates ()
  "Helm candidates."
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
                  (gethash "title" result))
                json-parse-result))))

(defun helm-librarian//action (candidate)
  "Helm action performed when selecting CANDIDATE."
  (message "%s" candidate))

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
