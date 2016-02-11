;;; init.el --- Where all the magic begins
;;
;; Based on the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; load Org-mode from source when the ORG_HOME environment variable is set

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(message (concat "0 Org version is " (org-version)))
;; Try to unload built-in org, so I can update with current version.
(mapc
 (lambda (x)
   (let ((feat (cdr (assoc 'provide (cdr x)))))
     (and (string-match "^org\\|^ox\\\|^ob" (symbol-name feat))
	  (featurep feat)
	  (unload-feature feat t))))
 load-history)

(package-initialize)

(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))
(message (concat "1 Org version is " (org-version)))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq config-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the first configuration file
    (message "hello from init hook")
    (org-babel-load-file (expand-file-name "config.org" config-dir))))

;;; init.el ends here
(provide 'init) ; make (require 'init) happy

;; For debugging
;; (setq config-dir (expand-file-name "~/.emacs.d"))
