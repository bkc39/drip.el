;;; ci-setup.el --- Install deps & prep for batch compile  -*- lexical-binding:t; -*-
;; Usage:  emacs -Q --batch -l ci-setup.el -f batch-byte-compile FILES...
;;; Commentary:
;;; Code:
(require 'package)
(require 'cl-lib)

;; 1) Package archives (MELPA + GNU by default)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 2) Refresh once per job
(unless package-archive-contents
  (package-refresh-contents))

(defun ci/read-package-requires (file)
  "Return a list of symbols found in FILE's ‘Package-Requires’ header."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward
           "^;;[ \t]*Package-Requires:[ \t]*\\(([^)]*)\\)" nil t)
      (mapcar (lambda (spec) (if (consp spec) (car spec) spec))
              (read (match-string 1))))))

(defun ci/ensure-installed (pkg)
  "Install PKG if needed and print what happened."
  (if (package-installed-p pkg)
      (princ (format "✓ %-12s already installed (%s)\n"
                     pkg (package-desc-version
                          (cadr (assq pkg package-alist)))))
    (progn
      (princ (format "✚ installing %-12s …\n" pkg))
      (let ((desc (package-install pkg)))
        (princ (format "  package-install returned: %S\n" desc))))))

;; 3) Install every dependency mentioned in any *.el file passed on
;; the command line
(cl-loop for arg in command-line-args-left
         ;; zsh may give us "file1.el\nfile2.el" in a single ARG
         for files = (split-string arg "\n" t)
         do (dolist (file files)
              (when (string-match-p "\\.el\\'" file)
                (dolist (pkg '(uuidgen))
                  (ci/ensure-installed pkg)))))

;; 4) Expose freshly installed packages to the compiler
(add-to-list 'load-path package-user-dir)
(add-to-list 'load-path default-directory)

;; 5) Make warnings fatal & verbose
(setq byte-compile-warnings '(error)
      byte-compile-verbose t)


(provide 'ci-setup)
;;; ci-setup.el ends here
