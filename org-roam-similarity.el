;;; org-roam-similarity.el --- Org-mode Similarity Discovery Integrated with Org-Roam -*- lexical-binding: t; -*-

;; Initial Author: Bruno Arine <bruno.arine@runbox.com>
;; Author of the fork: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/org-roam-similarity
;; Version: 0.1
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

(defcustom org-roam-similarity-language "english"
  "The language org-roam-similarity uses.

Specifically, in the python script that does the heavy lifting,
the nltk program is used for text pre-processing and this
variable defines the language it should use. Defaults to english
for obvious reasons."
  :type 'string
  :group 'org-roam-similarity)

(defcustom org-roam-similarity-number-of-documents 10
  "Amount of documents org-roam-similarity should return."
  :type 'integer
  :group 'org-roam-similarity)

(defcustom org-roam-similarity-directory org-roam-directory
  "Directory in which org-roam-similarity should look.

Defaults to `org-roam-directory' as this package is meant to be
used with org-roam."
  :type 'directory
  :group 'org-roam-similarity)

(defcustom org-roam-similarity-show-scores nil
  "Show similarity scoring of files in the output."
  :type 'boolean
  :group 'org-roam-similarity)

(defcustom org-roam-similarity-root
  (concat (file-name-directory
	   (f-full (or
		    load-file-name
		    buffer-file-name)))
	  ".")
  "Root directory of org-roam-similarity."
  :type 'directory
  :group 'org-roam-similarity)

(defun org-roam-similarity-insert-list ()
  "Insert a list of 'org-mode' links to files that are similar to the buffer file."
  (interactive)
  (end-of-buffer)
  (newline)
  (let ((command (format "python3 %s -i %s -d %s -l %s -n %s %s"
			 (concat org-roam-similarity-root "/assets/org-similarity.py")
			 buffer-file-name
			 org-roam-similarity-directory
			 org-roam-similarity-language
			 org-roam-similarity-number-of-documents
			 (if org-roam-similarity-show-scores "--score" ""))))
    (insert (shell-command-to-string command)))
  (pop-global-mark)
  )

(defun org-roam-similarity-sidebuffer ()
    "Puts the results of org-similarity in a side-window."
    (interactive)
    (let ((command (format "python3 %s -i %s -d %s -l %s -n %s %s"
	    (concat org-similarity-root "/assets/org-similarity.py")
	     buffer-file-name
	     org-similarity-directory
	     org-similarity-language
	     org-similarity-number-of-documents
	     (if org-similarity-show-scores "--score" ""))))
      (setq similarity-results (shell-command-to-string command)))
      (with-output-to-temp-buffer "*Similarity Results*"
      (princ similarity-results))
      (with-current-buffer "*Similarity Results*"
      (org-mode))
    )
  (add-to-list 'display-buffer-alist
	       '("*Similarity Results*"
		 (display-buffer-in-side-window)
		 (inhibit-same-window . t)
		 (side . right)
		 (window-width . 0.4))
  )

(provide 'org-roam-similarity)

;;; org-similarity.el ends here
