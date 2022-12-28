;;; org-similarity.el --- Org-mode Similarity Discovery -*- lexical-binding: t; -*-

;; Copyright © 2020-2023 Bruno Arine
;;
;; Author: Bruno Arine <bruno.arine@runbox.com>
;; URL: https://github.com/brunoarine/org-similarity
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") (f "0.20.0"))
;; Keywords: matching, outlines, wp, org

;; 3-clause BSD License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A package to help discover similar or related org-mode files.
;; Use M-x org-similarity-insert-list RET to insert a list of links containing related files.

;;; Code:

(require 'f)

(defgroup org-similarity nil
  "Org-similarity."
  :prefix "org-similarity-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/brunoarine/org-similarity")
  :link '(emacs-commentary-link :tag "Commentary" "org-similarity"))

(defconst org-similarity-version "0.2"
  "The current version of ORG-SIMILARITY.")

(defcustom org-similarity-language
  "english"
  "The language passed to nltk's Snowball stemmer."
  :type 'string)

(defcustom org-similarity-number-of-documents
  10
  "How many similar entries to list at the end of the buffer."
  :type 'integer)

(defcustom org-similarity-recursive-search
  nil
  "Whether to perform recursive file search."
  :type 'boolean)

(defcustom org-similarity-directory
  "~/org"
  "Directory to scan for possibly similar documents."
  :type 'string)

(defcustom org-similarity-show-scores
  nil
  "Whether to prepend the list entries with their cosine similarity score."
  :type 'boolean)

(defcustom org-similarity-use-id-links
  nil
  "Whether to show results as ID links instead of FILE links."
  :type 'boolean)

(defcustom org-similarity-custom-python-interpreter
  nil
  "Override org-similarity default interpreter.
If nul, org-similarity will use a venv inside `emacs-local-directory'."
  :type 'string)

(defvar org-similarity--package-path
  (file-name-directory
   (f-full (or load-file-name buffer-file-name)))
  "Org-similarity location.")

(defvar org-similarity--python-interpreter
  (or org-similarity-custom-python-interpreter
      (concat org-similarity--package-path "venv/bin/python"))
  "Path to the Python executable that you want to use.")

(defvar org-similarity-deps-install-buffer-name
  " *Install org-similarity Python dependencies* "
  "Name of the buffer used for installing org-similarity dependencies.")

(defun org-similarity--system-python-available-p ()
  "Return t if Python is available in the system."
  (unless (executable-find "python3")
    (error "Org-similarity needs Python to run. Please, install Python"))
  t)

(defun org-similarity--venv-python-available-p ()
  "Check if designated virtual environment is available."
  (file-exists-p org-similarity--python-interpreter))

(defun org-similarity--deps-available-p ()
  "Return t if requirements.txt packages are installed, nil otherwise."
  (if (file-exists-p org-similarity--python-interpreter)
      (zerop (call-process org-similarity--python-interpreter nil nil nil
                           (concat org-similarity--package-path "check_deps.py"))) nil))

(defun org-similarity-create-local-venv ()
  "Create environment and install Python dependencies and main script."
  (when (org-similarity--system-python-available-p)
    (let* ((install-commands (concat (executable-find "python3") " -m venv " org-similarity--package-path "venv"))
           (buffer (get-buffer-create org-similarity-deps-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let
                     ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" install-commands)))
          (message "Installation of `org-similarity' Python dependencies succeeded")
        (error "Installation of `org-similarity' Python dependencies failed!")))))

(defun org-similarity-install-dependencies ()
  "Install Python dependencies inside the interpreter's environment."
  (when (org-similarity--venv-python-available-p)
    (let* ((install-commands (concat org-similarity--python-interpreter
                                     " -m pip install --upgrade pip && "
                                     org-similarity--python-interpreter
                                     " -m pip install -r "
                                     org-similarity--package-path
                                     "requirements.txt"))
           (buffer (get-buffer-create org-similarity-deps-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" install-commands)))
          (message "Installation of `org-similarity' Python dependencies succeeded")
        (error "Installation of `org-similarity' Python dependencies failed!")))))

(defun org-similarity--check-interpreter-and-deps-status ()
  "Perform interpreter and dependencies check, and install stuff if needed."
  (progn
    (unless (org-similarity--venv-python-available-p)
      (org-similarity-create-local-venv))
    (unless (org-similarity--deps-available-p)
      (if (y-or-n-p "Org-similarity needs to download some Python packages to work. Download them now? ")
          (org-similarity-install-dependencies)
        (error "Org-similarity won't work until its Python dependencies are downloaded!")))))

(defun org-similarity--run-command ()
  "Run org-similarity's Python script and return the COMMAND output as string."
  (progn
    (org-similarity--check-interpreter-and-deps-status)
    (let ((command (format "%s %sorgsimilarity/__main__.py -i %s -d %s -l %s -n %s %s %s %s"
                           org-similarity--python-interpreter
                           org-similarity--package-path
                           buffer-file-name
                           org-similarity-directory
                           org-similarity-language
                           org-similarity-number-of-documents
                           (if org-similarity-show-scores "--scores" "")
                           (if org-similarity-recursive-search "--recursive" "")
                           (if org-similarity-use-id-links "--id-links" ""))))
      (shell-command-to-string command))))

(defun org-similarity-insert-list ()
  "Insert a list of 'org-mode' links to files that are similar to the buffer file."
  (interactive)
  ;; If org-similarity dependencies are not installed yet, install them
  (goto-char (point-max))
  (newline)
  (insert (org-similarity--run-command)))

(defun org-similarity-sidebuffer ()
  "Puts the results of org-similarity in a side-window."
  (interactive)
  (let ((results (org-similarity--run-command)))
    (with-output-to-temp-buffer "*Similarity Results*"
      (princ results))
    (with-current-buffer "*Similarity Results*"
      (org-mode))))

(provide 'org-similarity)

;;; org-similarity.el ends here
