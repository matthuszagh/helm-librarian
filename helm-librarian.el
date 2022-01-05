;;; helm-librarian.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;; helm-librarian provides a helm interface for the 'librarian search'
;; command along with additional convenience utilities for displaying
;; matching resources.

;;; Code:

(require 'helm)
(require 'helm-source)

(defvar librarian-executable
  "librarian"
  "Librarian executable.")

(defvar librarian-library-directory
  "~/library"
  "Library directory.")

(defun librarian//date-year (date)
  "Extract the year from a date string, DATE."
  (if (and (not (eq date :null))
           (>= (length date) 4))
      (substring date 0 4)
    ""))

(defun librarian//name-last (name)
  "Extract the last name from a name string, NAME."
  (if (not (eq name :null))
      (let* ((subnames (split-string name " "))
             (subname-count (length subnames)))
        (if (eq subname-count 1)
            (car subnames)
          (if (eq subname-count 2)
              (nth 1 subnames)
            (if (eq subname-count 3)
                (nth 2 subnames)))))
    ""))

(defun helm-librarian//default-display-function (resource)
  "The default display function for RESOURCE.
RESOURCE is a hash map representing the resource to be
displayed."
  ;; TODO for some reason `(window-width (get-buffer-window))' reports
  ;; accurate results but `(window-width)' doesn't. From the
  ;; documentation, I can't tell what the difference between them.
  (let ((window-width (window-width (get-buffer-window)))
        (prefix (concat
                 (helm-librarian/field-value "title" resource)
                 (helm-librarian/treat-as-unit
                  ": "
                  (lambda ()
                    (helm-librarian/field-value "subtitle" resource)))
                 (helm-librarian/treat-as-unit
                  ", Vol. "
                  (lambda ()
                    (helm-librarian/field-value "volume" resource)))
                 (helm-librarian/treat-as-unit
                  " ["
                  (propertize
                   "ver. "
                   'face
                   'italic)
                  (lambda ()
                    (helm-librarian/field-value "version" resource))
                  "]")
                 ))
        (suffix
         (concat
          (propertize
           (helm-librarian/first-nonempty-field
            (helm-librarian/field-value "author[0].'librarian//name-last" resource)
            (helm-librarian/field-value "organization" resource))
           'face
           'italic)
          (helm-librarian/treat-as-unit
           ", "
           (lambda ()
             (helm-librarian/field-value "date.'librarian//date-year" resource)))
          (propertize
           (helm-librarian/treat-as-unit
            " ["
            (lambda ()
              (helm-librarian/field-value "content" resource))
            "/"
            (lambda ()
              (helm-librarian/field-value "document" resource))
            "]")
           'face
           '(foreground-color . "grey40")))))
    (if (> (+ (length prefix)
              1
              (length suffix))
           window-width)
        (setq prefix (concat (substring prefix nil (- window-width
                                                      2
                                                      (length suffix)))
                             "…")))
    (concat
     (string-pad prefix
                 (+ (length prefix)
                    (max (- window-width (length prefix) (length suffix)) 1)))
     suffix)))

(defcustom librarian-display-function #'helm-librarian//default-display-function
  "A function used to display resource candidates in the helm buffer.
The function takes a single argument that is the resource to display."
  :type 'function)

(defun helm-librarian/field-value (field resource)
  "Get the field value for a resource.
FIELD is a string specifier of the field and RESOURCE is a hash
table of the resource.  FIELD can accomodate embedded lists, hash
tables and custom functions.  To get an element of a list specify
the index with [index] (e.g., authors[0]).  To specify the key of
a hash table, use a dot ('.') followed by the key name (e.g.,
authors[0].last).  To use a custom function, prefix the function
name with a single quote (e.g.,
@date.'librarian//date-year@).  A custom function will be
applied to the portion of the resource that's already been
extracted by the field specifier.  So, for instance
@date.'librarian//date-year@ will extract the date
string for a resource and then call 'librarian//date-year,
which takes the date string as an argument."
  ;; Separate `field' into keys. Then, use each key (and the
  ;; associated list index, if provided) to restrict the original
  ;; resource hash, `resource' to the subhash corresponding to the
  ;; value of the key.
  (let* ((field-list (split-string field "\\."))
         (initial-key (car (split-string (car field-list) "\\["))))
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
      (unless (eq resource "")
        (let* ((key-index (split-string field "\\["))
               (key (car key-index)))
          ;; "\\[" didn't match if list has 1 element
          (if (not (eq (length key-index) 1))
              (let ((index (string-to-number (nth 0 (split-string (nth 1 key-index) "\\]"))))
                    (value (gethash key resource "")))
                (if (seq-empty-p value)
                    (setq resource "")
                  (setq resource (elt (gethash key resource "") index))))
            (if (equal (substring key 0 1) "'")
                (setq resource (funcall (intern (substring key 1 nil)) resource))
              (setq resource (gethash key resource "")))))))
    ;; Replace null values with an empty string.
    (if (eq resource :null)
        ""
      (if (numberp resource)
          (number-to-string resource)
        resource))))

(defun helm-librarian//eval-string-or-function (string-or-function)
  "If STRING-OR-FUNCTION is a string, return that string, otherwise
evaluate it as a function."
  (if (equal (type-of string-or-function) 'string)
      string-or-function
    (funcall (string-or-function))))

(defun helm-librarian/first-nonempty-field (&rest args)
  "Return the value of the first argument in ARGS that evaluates to
a nonempty string.  ARGS are one or more functions, each of which
must evaluate to a string.  If all functions evaluate to empty
strings, this function returns the empty string."
  (if (seq-empty-p args)
      ""
    (let* ((i 0)
           (result (helm-librarian//eval-string-or-function (nth i args)))
           (list-length (length args)))
      (while (and
              (string-empty-p result)
              (< (+ i 1) list-length))
        (setq i (+ i 1))
        (setq result (helm-librarian//eval-string-or-function (nth i args))))
      result)))

;; TODO this function has the downside of requiring function arguments
;; as lambda expressions. It should probably be implemented as a macro
;; so this is not necessary.
(defun helm-librarian/treat-as-unit (&rest args)
  "Treat the list of strings and functions in ARGS as a unit.
More specifically, this function will concatenate and return the
value of all arguments as long as a minimum of one of the
function arguments returns a non-empty string.  If all function
arguments return an empty string, an empty string is returned."
  (let ((non-empty-function-string nil)
        (result ""))
    (dolist (string-or-function args)
      (if (equal (type-of string-or-function) 'string)
          (setq result (concat result string-or-function))
        (let ((function-result (funcall string-or-function)))
          (unless (string-empty-p function-result)
            (setq result (concat result function-result))
            (setq non-empty-function-string t)))))
    (if (eq non-empty-function-string t)
        result
      "")))

(defun helm-librarian//render-website (dir)
  "Open and render a website with shr.
DIR is the website base directory."
  ;; Assume only one htm/html file.
  (let* ((file (car (directory-files-recursively dir "\\.htm[l]?")))
         (file-buffer (find-file file))
         (file-window (selected-window))
         (render-buffer (shr-render-buffer (get-file-buffer file))))
    (set-window-buffer (selected-window) (window-old-buffer))
    ;; `shr-render-buffer' stupidly doesn't return the buffer
    ;; name. Instead, it always uses "*html*".
    (set-window-buffer file-window "*html*")
    (select-window file-window)
    (kill-buffer file-buffer)))

(defun helm-librarian//open-resource-file (candidate)
  "Open the file corresponding to the resource CANDIDATE.
The resource is formatted as a hash table as returned by `json-parse-string'."
  (let ((path (concat librarian-library-directory
                      "/resources/"
                      (elt (gethash "historical_checksums" candidate) 0))))
    (if (string-equal "website" (gethash "content" candidate ""))
        (helm-librarian//render-website path)
      (find-file path))))

(defun helm-librarian//candidates ()
  "Helm candidates.
Returns an alist in which the first item is the display string
and the second item is the hash table passed to
`helm-librarian//action'."
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
                  `(,(funcall librarian-display-function result) . ,result))
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
              :nohighlight t
              :volatile t)
   :buffer "*helm librarian*"
   :prompt "librarian search: "
   :input-idle-delay 0.5))

(provide 'helm-librarian)

;;; helm-librarian.el ends here
