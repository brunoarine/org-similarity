;;; org-similarity.el --- Org-mode Similarity Discovery -*- lexical-binding: t; -*-

;; Author: Bruno Arine <bruno.arine@runbox.com>
;; URL: https://github.com/brunoarine/org-similarity
;; Version: 0.2
;; Package-Requires: ((org "9.3") (emacs "26.1")))

;; 3-clause BSD License

;; Copyright 2020 Bruno Arine

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice,
;; this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its contributors
;; may be used to endorse or promote products derived from this software without
;; specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

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
  ""
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

(defvar org-similarity-package-path
  (file-name-directory
   (f-full (or load-file-name buffer-file-name)))
  "Org-similarity location.")

;; Dependency installation variables and functions

(defvar org-similarity-python-interpreter
  (concat org-similarity-package-path "venv/bin/python")
  "Path to the Python executable that you want to use.")

(defvar org-similarity-deps-install-buffer-name
  " *Install org-similarity Python dependencies* "
  "Name of the buffer used for installing org-similarity dependencies.")

(defun org-similarity--is-python-available ()
  "Return t if Python is available."
  (unless (executable-find "python")
    (error "Org-similarity needs Python to run. Please, install Python"))
  t)

(defun org-similarity--is-deps-available ()
  "Return t if requirements.txt packages are installed, nil otherwise."
  (if (file-exists-p org-similarity-python-interpreter)
    (zerop (call-process org-similarity-python-interpreter nil nil nil
                  (concat org-similarity-package-path "check_deps.py"))) nil))

(defun org-similarity-install-dependencies ()
  "Create environment and install Python dependencies and main script."
  (when (org-similarity--is-python-available)
    (let* ((install-commands
            (concat
                "cd " org-similarity-package-path " && \
                python -m venv venv && \
                source venv/bin/activate && \
                python -m pip install --upgrade pip && \
                python -m pip install -r requirements.txt && \
                python -m pip install . && \
                cd -"))
       (buffer (get-buffer-create org-similarity-deps-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let
                     ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" install-commands)))
          (message "Installation of `org-similarity' Python dependencies succeeded")
        (error "Installation of `org-similarity' Python dependencies failed!")))))

;; If org-similarity dependencies are not installed yet, install them
(unless (org-similarity--is-deps-available)
  (if (y-or-n-p "Org-similarity needs to download some Python packages to work. Download them now? ")
      (org-similarity-install-dependencies)
    (error "Org-similarity won't work until its Python dependencies are downloaded!")))


;; Main routine.

(defun org-similarity-insert-list ()
  "Insert a list of 'org-mode' links to files that are similar to the buffer file."
  (interactive)
  (goto-char (point-max))
  (newline)
  (let ((command (format "%s -m orgsimilarity -i %s -d %s -l %s -n %s %s %s %s"
                         org-similarity-python-interpreter
                         buffer-file-name
                         org-similarity-directory
                         org-similarity-language
                         org-similarity-number-of-documents
                         (if org-similarity-show-scores "--scores" "")
                         (if org-similarity-recursive-search "--recursive" "")
                         (if org-similarity-use-id-links "--id-links" ""))))
    (insert (shell-command-to-string command))))

(provide 'org-similarity)

;;; org-similarity.el ends here
