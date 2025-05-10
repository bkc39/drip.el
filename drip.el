;;; drip.el --- Local RAG solution scaffold -*- lexical-binding: t -*-

;; Author: Ben Carriel (pl.proofs@gmail.com)
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/bkc39/drip

;; This file is not part of GNU Emacs.

;;; Code:

(defgroup drip nil
  "Local RAG for LLMs."
  :group 'tools)

(defun dump-keys-to-buffer (json-object)
  "Dump the keys of JSON-OBJECT into a temporary help buffer."
  (with-help-window "*ollama-generate-embedding-debug*"
    (princ "Returned JSON keys:\n")
    (dolist (k (mapcar #'car json-object))
      (princ (format "%S\n" k)))))

(defun ollama-generate-embedding (prompt &optional debug)
  "Get embedding from Ollama for PROMPT.
If DEBUG is non-nil, dump the keys of the returned JSON."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode `(("model" . "mxbai-embed-large")
                                          ("prompt" . ,prompt))))
         (url "http://localhost:11434/api/embeddings")
         embedding)
    (with-current-buffer (url-retrieve-synchronously url t t 5)
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (data (json-read)))
        (when debug
          (dump-keys-to-buffer data))
        (setq embedding (alist-get 'embedding data))))
    embedding))

(defun drip-hello-world ()
  "Display Hello, world!"
  (interactive)
  (message "Hello, world!"))

(provide 'drip)

;;; drip.el ends here
