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
  (let* ((url-request-method method)
         (url-request-data (when data (json-encode data)))
         (url (concat chroma-base-url path
                      (when params
                        (concat "?"
                                (url-build-query-string params))))))
    (with-current-buffer
        (url-retrieve-synchronously url t t)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun chroma-get-identity ()
  (chroma-api-request "GET" "/api/v2/auth/identity"))

(defun chroma-get-healthcheck ()
  (chroma-api-request "GET" "/api/v2/healthcheck"))

(defun chroma-get-heartbeat ()
  (chroma-api-request "GET" "/api/v2/heartbeat"))

(defun chroma-get-pre-flight-checks ()
  (chroma-api-request "GET" "/api/v2/pre-flight-checks"))

(defun chroma-reset ()
  (chroma-api-request "POST" "/api/v2/reset"))

(defun chroma-create-tenant (data)
  (chroma-api-request "POST" "/api/v2/tenants" data))

(defun chroma-get-tenant (tenant)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant)))

(defun chroma-list-databases (tenant &optional params)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant "/databases")
                      nil params))

(defun chroma-create-database (tenant data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases")
                      data))

(defun chroma-get-database (tenant db)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db)))

(defun chroma-delete-database (tenant db)
  (chroma-api-request "DELETE"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db)))

(defun chroma-list-collections (tenant db)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections")))

(defun chroma-create-collection (tenant db data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections")
                      data))

(defun chroma-get-collection (tenant db col)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col)))

(defun chroma-update-collection (tenant db col data)
  (chroma-api-request "PUT"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col)
                      data))

(defun chroma-delete-collection (tenant db col)
  (chroma-api-request "DELETE"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col)))

(defun chroma-add-records (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/add")
                      data))

(defun chroma-count-records (tenant db col)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/count")))

(defun chroma-delete-records (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/delete")
                      data))

(defun chroma-fork-collection (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/fork")
                      data))

(defun chroma-get-records (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/get")
                      data))

(defun chroma-query-collection (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/query")
                      data))

(defun chroma-update-records (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/update")
                      data))

(defun chroma-upsert-records (tenant db col data)
  (chroma-api-request "POST"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections/" col
                              "/upsert")
                      data))

(defun chroma-collections-count (tenant db)
  (chroma-api-request "GET"
                      (concat "/api/v2/tenants/" tenant
                              "/databases/" db
                              "/collections_count")))

(defun chroma-get-version ()
  (chroma-api-request "GET" "/api/v2/version"))

(provide 'chromadb)

;;; chromadb.el ends here
