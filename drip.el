;;; drip.el --- Local RAG solution scaffold -*- lexical-binding: t -*-

;; Author: Ben Carriel (pl.proofs@gmail.com)
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/bkc39/drip

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Local RAG solution scaffold for LLMs, embedding via Ollama and ChromaDB.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)

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


(defun drip-embed-file (file-path &optional debug)
  "Embed the text of FILE-PATH using ollama.
If DEBUG is non-nil, dump JSON keys of the response."
  (interactive "fFile to embed: \nP")
  ;; Step 1: Validate FILE-PATH is readable
  (unless (file-readable-p file-path)
    (error "Cannot read file: %s" file-path))
  ;; Step 2: Read entire file into a string
  (let ((content (with-temp-buffer
                   (insert-file-contents file-path)
                   (buffer-string))))
    ;; Step 3: Call ollama-generate-embedding with content and DEBUG
    (let ((embedding (ollama-generate-embedding content debug)))
      ;; Step 4: Return the embedding result
      embedding)))

(defun list-files-in-directory-tree (dir &optional ignored-dirs ignored-regexps)
  "Return list of all files under DIR.

Skip specified directories or matching regexps.If a directory name is in
IGNORED-DIRS, or a file or directory name matches any regexp in
IGNORED-REGEXPS, skip that entry entirely.  Default IGNORED-DIRS is
'(\".git\" \"node_modules\" \".hg\" \".svn\").  Default IGNORED-REGEXPS
is '(\"\\`#.*#\\'\" \"\\`.*~\\'\" \"\\`\\.?#\" )."
  (interactive "DDirectory: ")
  (message "Starting list-files-in-directory-tree on %s" dir)
  (unless (file-directory-p dir)
    (error "Not a directory: %s" dir))
  (let* ((ignored-dirs (or ignored-dirs
                           '(".git" "node_modules" ".hg" ".svn")))
         (ignored-regexps (or ignored-regexps
                              '("\\`#.*#\\'" "\\`.*~\\'" "\\`\\.?#")))
         (result '())
         (stack (list (directory-file-name (expand-file-name dir)))))
    (message "Ignored dirs: %s, ignored regexps: %s"
             ignored-dirs
             ignored-regexps)
    (while stack
      (let ((current (pop stack)))
        (message "Processing directory: %s" current)
        (dolist (entry (directory-files current t))
          (let ((name (file-name-nondirectory entry)))
            (unless (member name '("." ".."))
              (if (or (member name ignored-dirs)
                      (cl-some (lambda (rx) (string-match-p rx name))
                               ignored-regexps))
                  (message "Skipping %s" entry)
                (cond
                 ((file-directory-p entry)
                  (message "Pushing directory onto stack: %s" entry)
                  (push entry stack))
                 ((file-regular-p entry)
                  (message "Adding file to result: %s" entry)
                  (push entry result))))))))
    (message "Completed scanning. %d files found." (length result)))
    result))

(defun drip-embed-all (tenant db col file-list &optional debug)
  "Add FILE-LIST as records to COL in DB for TENANT.
If DEBUG is non-nil, dump embedding response keys."
  ;; Step 1: Validate file-list
  (dolist (file file-list)
    (unless (file-readable-p file)
      (error "Cannot read file: %s" file)))
  ;; Step 2: Initialize lists
  (let ((docs '())
        (embeds '())
        (ids '())
        (metas '())
        (uris '()))
    ;; Step 3: Process each file
    (dolist (file file-list)
      ;; Step 3a: Read content
      (let* ((content (with-temp-buffer
                        (insert-file-contents file)
                        (encode-coding-string (buffer-string) 'utf-8)))
             ;; Step 3b: Generate embedding
             (embedding
              (progn
                (message "embedding file %s" file)
                (ollama-generate-embedding content debug)))
             ;; Step 3c: Convert to vector
             (embed-vec (apply #'vector embedding))
             ;; Step 3d: Generate uuid
             (id (uuid-string))
             ;; Step 3e: Absolute path
             (abs (expand-file-name file))
             ;; Step 3f: Metadata alist
             (meta `(("path" . ,abs)
                     ("model" . "mxbai-embed-large")))
             ;; Step 3g: URI
             (uri abs))
        ;; Step 3h: Append to lists
        (push content docs)
        (push embed-vec embeds)
        (push id ids)
        (push meta metas)
        (push uri uris)))
    ;; Step 4: Build vectors
    (let* ((docs-vec   (apply #'vector (nreverse docs)))
           (embed-vecs (apply #'vector (nreverse embeds)))
           (ids-vec    (apply #'vector (nreverse ids)))
           (meta-vec   (apply #'vector (nreverse metas)))
           (uris-vec   (apply #'vector (nreverse uris)))
           ;; Step 5: Build record-data
           (record-data
            `(("documents" . ,docs-vec)
              ("embeddings" . ,embed-vecs)
              ("ids" . ,ids-vec)
              ("metadatas" . ,meta-vec)
              ("uris" . ,uris-vec))))
      ;; Step 6: Call ChromaDB API
      (chroma-add-records tenant db col record-data))))

(defun drip-retrieve-documents-for-prompt (prompt n-results)
  "Retrieve DOCUMENTS and URIS for PROMPT with top N-RESULTS.
Return a list of alists of the form ((uri . URIP) (document . DOCP)),
with each document decoded as UTF-8."
  (let* ((embedding-list (ollama-generate-embedding prompt))
         (embed-vec       (apply #'vector embedding-list))
         (payload
          `(("include"          . ,(vector "documents" "uris"))
            ("n_results"        . ,n-results)
            ("query_embeddings" . ,(vector embed-vec))))
         (response
          (chroma-query-collection drip-chroma-db-tenant
                                   drip-chroma-db-database
                                   collection-id
                                   payload))
         (docs (alist-get 'documents response))
         (uris (alist-get 'uris response)))
    (let ((decoded-docs
           (mapcar (lambda (doc)
                     (decode-coding-string doc 'utf-8))
                   (aref docs 0))))
      (cl-mapcar (lambda (uri doc)
                   `((uri      . ,uri)
                     (document . ,doc)))
                 (aref uris 0)
                 decoded-docs))))

(defvar drip-chroma-db-host "localhost"
  "Default host for drip-chroma-db.")

(defvar drip-chroma-db-port "8000"
  "Default port for drip-chroma-db.")

(defvar drip-chroma-db-path nil
  "Path to use for drip-chroma-db.

If nil, a temporary directory will be generated.")

(defvar drip-chroma-db-process nil
  "Process handle for drip-chroma-db.")

(defvar drip-chroma-db-tenant "default_tenant"
  "Default tenant name for ChromaDB.")

(defvar drip-chroma-db-database "default_database"
  "Default collection name for ChromaDB.")

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

(defun drip-chroma-db-create-collection (name &optional tenant database)
  "Create a new collection named NAME in the Chroma database.

NAME is the string name of the collection to create.  Optional TENANT is
the tenant identifier; defaults to `drip-chroma-db-tenant' or
\"default_tenant\".  Optional DATABASE is the database name; defaults to
`drip-chroma-db-database' or \"default_database\".  Sends a POST request
to the Chroma API endpoint
/api/v2/tenants/TENANT/databases/DATABASE/collections and returns the
parsed JSON response."
  (interactive "sCollection name: ")
  (let* ((tenant (or tenant drip-chroma-db-tenant "default_tenant"))
         (database (or database drip-chroma-db-database "default_database"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `(("name" . ,name))))
         (url
          (format "http://%s:%s/api/v2/tenants/%s/databases/%s/collections"
                  drip-chroma-db-host
                  drip-chroma-db-port
                  tenant
                  database))
         data)
    (with-current-buffer
        (url-retrieve-synchronously url t t 5)
      (goto-char url-http-end-of-headers)
      (setq data (json-read)))
    data))



(provide 'drip)

;;; drip.el ends here
