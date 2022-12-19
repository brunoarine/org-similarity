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

(defvar org-roam-similarity-language "english")

(defvar org-roam-similarity-number-of-documents 10)

(defvar org-roam-similarity-directory org-directory)

(defvar org-roam-similarity-show-scores nil)

(defvar org-roam-similarity-root
  (concat (file-name-directory
           (f-full (or
                    load-file-name
                    buffer-file-name)))
          "."))

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

(provide 'org-roam-similarity)

;;; org-similarity.el ends here
