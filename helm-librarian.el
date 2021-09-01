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

(defun helm-librarian ()
  "Helm completions for librarian's search command."
  (interactive)
  (helm
   :sources (helm-build-sync-source "librarian"
              :match-dynamic t
              :candidates
              (lambda ()
                (let ((candidates nil)
                      (json-parse-result
                       (ignore-errors
                         (json-parse-string
                          (shell-command-to-string
                           (concat librarian-executable
                                   " -d "
                                   librarian-library-directory
                                   " search "
                                   helm-pattern))))))
                  (if json-parse-result
                      (progn
                        (let ((results (seq-into json-parse-result 'list)))
                          (dolist (result results)
                            (setq candidates
                                  (append
                                   candidates
                                   (list (gethash "title" result))))))))
                  candidates))

              :action
              (lambda (candidate)
                (message "%s" candidate))

              :volatile t)
   :buffer "*helm librarian*"
   :prompt "librarian search: "))

(provide 'helm-librarian)

;;; helm-librarian.el ends here
