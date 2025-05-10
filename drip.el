;;; drip.el --- Local RAG solution scaffold -*- lexical-binding: t -*-

;; Author: Ben Carriel (pl.proofs@gmail.com)
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/bkc39/drip

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ansi-color)

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


(defvar drip-chroma-db-host "localhost"
  "Default host for drip-chroma-db.")

(defvar drip-chroma-db-port "8000"
  "Default port for drip-chroma-db.")

(defvar drip-chroma-db-path nil
  "Path to use for drip-chroma-db.

If nil, a temporary directory will be generated.")

(defvar drip-chroma-db-process nil
  "Process handle for drip-chroma-db.")

(defun drip-chroma-db--process-filter (proc output)
  "Process filter for drip-chroma-db.

PROC is the process that produced OUTPUT, a string containing text to
insert.  This function appends OUTPUT to the process buffer and applies
ANSI color codes."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t)
          (start (point-max)))
      (goto-char start)
      (insert output)
      (ansi-color-apply-on-region start (point-max)))))

(defun drip-chroma-db-run (&optional host port path)
  "Start a ChromaDB server process in a dedicated buffer.

Optional arguments HOST, PORT, and PATH specify the server
host, port, and database path.  When called interactively,
prompt for HOST and PORT with defaults from
`drip-chroma-db-host' and `drip-chroma-db-port'.  If PATH is nil,
use `drip-chroma-db-path' or create a temporary directory.

The server output is shown in the *drip-chroma-db* buffer, and
the process will be killed when that buffer is killed."
  (interactive
   (list
    (read-string (format "Host (default %s): " drip-chroma-db-host)
                 nil nil drip-chroma-db-host)
    (read-string (format "Port (default %s): " drip-chroma-db-port)
                 nil nil drip-chroma-db-port)))
  (let* ((host  (or host  drip-chroma-db-host))
         (port  (or port  drip-chroma-db-port))
         (path  (or path  drip-chroma-db-path
                    (make-temp-file "drip-chroma-db" t)))
         (buf   (get-buffer-create "*drip-chroma-db*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (setq drip-chroma-db-process
            (start-process "drip-chroma-db" buf
                           "chroma" "run"
                           "--host" host
                           "--port" port
                           "--path" path))
      (set-process-query-on-exit-flag drip-chroma-db-process nil)
      (set-process-filter drip-chroma-db-process
                          #'drip-chroma-db--process-filter)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (process-live-p drip-chroma-db-process)
                    (kill-process drip-chroma-db-process)))
                nil t)
      (read-only-mode 1))
    (display-buffer buf)))

(defun drip-hello-world ()
  "Display Hello, world!"
  (interactive)
  (message "Hello, world!"))

(provide 'drip)

;;; drip.el ends here
