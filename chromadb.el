;;; chroma.el --- Chroma API client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides functions to interact with the Chroma REST API.

;;; Code:

(require 'url)

(require 'url-util)

(require 'json)

(defvar chroma-base-url "http://localhost:8000"
  "Base URL for the Chroma API.")

(defun chroma-api-request (method path &optional data params)
  "Make an HTTP request to the Chroma API.
METHOD is the HTTP method as a string (\"GET\", \"POST\", etc.).
PATH is the API endpoint path.
DATA, if non-nil, is encoded as JSON and sent as the request body.
PARAMS, if non-nil, is an alist of query parameters."
  (let* ((url-request-method method)
         (url-request-data (when data (json-encode data)))
         (url (concat chroma-base-url path
                      (when params
                        (concat \"?\"
                                (url-build-query-string params))))))
    (with-current-buffer
        (url-retrieve-synchronously url t t)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun chroma-get-identity ()
  "Retrieve the authenticated identity from the Chroma API."
  (chroma-api-request "GET" "/api/v2/auth/identity"))

(defun chroma-get-healthcheck ()
  "Retrieve healthcheck information from the Chroma API."
  (chroma-api-request "GET" "/api/v2/healthcheck"))

(defun chroma-get-heartbeat ()
  "Retrieve heartbeat status from the Chroma API."
  (chroma-api-request "GET" "/api/v2/heartbeat"))

(defun chroma-get-pre-flight-checks ()
  "Retrieve pre-flight checks status from the Chroma API."
  (chroma-api-request "GET" "/api/v2/pre-flight-checks"))

(defun chroma-reset ()
  "Reset the Chroma service via the API."
  (chroma-api-request "POST" "/api/v2/reset"))

(defun chroma-create-tenant (data)
  "Create a new tenant in Chroma with DATA as an alist."
  (chroma-api-request "POST" "/api/v2/tenants" data))

(defun chroma-get-tenant (tenant)
  "Retrieve information about TENANT from the Chroma API."
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant)))

(defun chroma-list-databases (tenant &optional params)
  "List databases for TENANT.
PARAMS is an optional alist of query parameters."
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant "/databases")
                      nil params))

(defun chroma-create-database (tenant data)
  "Create a database under TENANT with DATA as an alist."
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant "/databases")
                      data))

(defun chroma-get-database (tenant db)
  "Retrieve database DB under TENANT."
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant "/databases/" db)))

(defun chroma-delete-database (tenant db)
  "Delete database DB under TENANT."
  (chroma-api-request "DELETE"
                      (concat "/api/v2/tenants/" tenant "/databases/" db)))

(defun chroma-list-collections (tenant db)
  "List collections in database DB under TENANT."
  (chroma-api-request
   "GET"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections")))

(defun chroma-create-collection (tenant db data)
  "Create a collection in database DB under TENANT with DATA as an alist."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections")
   data))

(defun chroma-get-collection (tenant db col)
  "Retrieve collection COL in database DB under TENANT."
  (chroma-api-request
   "GET"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col)))

(defun chroma-update-collection (tenant db col data)
  "Update collection COL in database DB under TENANT with DATA as an alist."
  (chroma-api-request
   "PUT"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col)
   data))

(defun chroma-delete-collection (tenant db col)
  "Delete collection COL from database DB under TENANT."
  (chroma-api-request
   "DELETE"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col)))

(defun chroma-add-records (tenant db col data)
  "Add records defined by DATA to collection COL in database DB under TENANT."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/add")
   data))

(defun chroma-count-records (tenant db col)
  "Count records in collection COL within database DB under TENANT."
  (chroma-api-request
   "GET"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/count")))

(defun chroma-delete-records (tenant db col data)
  "Delete records defined by DATA from collection COL in database DB under TENANT."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/delete")
   data))

(defun chroma-fork-collection (tenant db col data)
  "Fork collection COL in database DB under TENANT with DATA options."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/fork")
   data))

(defun chroma-get-records (tenant db col data)
  "Retrieve records defined by DATA from collection COL in database DB under TENANT."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/get")
   data))

(defun chroma-query-collection (tenant db col data)
  "Query collection COL in database DB under TENANT using DATA as query payload."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/query")
   data))

(defun chroma-update-records (tenant db col data)
  "Update records defined by DATA in collection COL in database DB under TENANT."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/update")
   data))

(defun chroma-upsert-records (tenant db col data)
  "Upsert records defined by DATA into collection COL in database DB under TENANT."
  (chroma-api-request
   "POST"
   (concat "/api/v2/tenants/" tenant "/databases/" db "/collections/" col "/upsert")
   data))

(defun chroma-collections-count (tenant db)
  "Retrieve the number of collections in database DB under TENANT."
  (chroma-api-request
   "GET"
   (concat "/api/v2/tenants/"
           tenant
           "/databases/"
           db
           "/collections_count")))

(defun chroma-get-version ()
  "Retrieve the current version of the Chroma API."
  (chroma-api-request "GET" "/api/v2/version"))


(provide 'chromadb)

;;; chromadb.el ends here
