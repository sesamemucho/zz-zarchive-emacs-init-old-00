(require 'workgroups2)

(defvar wgsw-frame-name-defaults '()
  "Property list of wg names vs descriptions.")

(defvar wgsw-number-of-frames 1
  "Desired number of frames")

(defvar wgsw-number-of-workgroups 12
  "Desired number of workgroups")

(defvar wgsw-keybinding-template "s-<f%d>"
  "Template for keybinding to switch between workspaces")

(defvar wgsw-default-frame-descriptions '()
  "Plist for default frame descriptions."
  ;; Indexed by frame name (i.e., "w2s0")
  )

(defvar wgsw-keybinding-func)

(defun wgsw-get-frame-description-param ()
  "Gets the current frame descriptions from the wg session"
  (wg-session-parameter 'frame-desc)
  ;;;(wg-session-parameter (wg-current-session t) 'frame-desc)
  ;;(wg-session-parameter (wg-current-session t) 'frame-desc '(('ha "")))
  )

;; (defun wgsw-get-blank-frame-description ()
;;   "Finds all entries for which the description is blank"
;;   (interactive)
;;   (let ((fdcopy (copy-alist (wgsw-get-frame-description-param))))
    
;;     (wg-set-session-parameter (wg-current-session) 'frame-desc 
;;                               (rassq-delete-all "" fdcopy))
;;     ))

;; (defun wgsw-clean-frame-description ()
;;   "Removes all entries for which the description is blank"
;;   (interactive)
;;   (let ((fdcopy (copy-alist (wgsw-get-frame-description-param))))
;;     (wg-set-session-parameter (wg-current-session) 'frame-desc 
;;                               (rassq-delete-all "" fdcopy))
;;     ))

(defun wgsw-set-current-frame-description (desc)
  "Set the description for the current wg frame to `desc'"
  (interactive "MEnter frame description: ")
  (let ((wg-name (wg-workgroup-name (wg-current-workgroup t))))
    (wgsw-set-frame-description wg-name desc)
    )
  )

(defun wgsw-set-frame-description (wg-name desc)
  "Set the description for wg workgroup wg-name to `desc'.
   Remove duplicates before saving so that we don't see the 
   old descriptions in wgsw-read-desc-from-completion."
  (let ((frame-names (assq-delete-all wg-name (wgsw-get-frame-description-param))))
    (add-to-list 'frame-names (cons (intern wg-name) desc))
    ;;(wg-set-session-parameter (wg-current-session) 'frame-desc frame-names)
    (wg-set-session-parameter 'frame-desc frame-names)
    )
  )

;; (defun wgsw-clear-current-frame-description ()
;;   "Clear the description for the current wg frame."
;;   (interactive)
;;   (let ((wg-name (wg-workgroup-name (wg-current-workgroup t))))
;;     (wgsw-set-frame-description wg-name "")
;;     )
;;   )

;; (defun wgsw-clear-frame-description (wg-name)
;;   "Clear the description for wg workgroup wg-name."
;;   (interactive)
;;   (wgsw-set-frame-description wg-name "")
;;   )

;; (defun wgsw-clear-current-frame-description-and-maybe-kill-projectile-buffers ()
;;   "Clear current wg frame and kill the projectile buffers."
;;   (interactive)
;;   (if (fboundp 'projectile-kill-buffers)
;;       (progn
;;         (projectile-kill-buffers)
;;         (wgsw-clear-current-frame-description)
;;         ))
;;   )

(defun wgsw-set-frame-description-from-default (wg-name)
  "Uses description from wgsw-default-frame-descriptions if present. Otherwise sets to blank."
  (wgsw-set-frame-description wg-name (or (cdr (assoc wg-name wgsw-default-frame-description))
                                          ""))
  )

(setq frame-title-format '(multiple-frames ("%b" "     " (:eval (wgsw-show-frame-desc)) )
                                           ("" invocation-name "@" system-name)))

(defun wgsw-show-frame-desc ()
  (interactive)
  (let ((frame-names (wgsw-get-frame-description-param)))
    (or (cdr (assoc (intern (wg-workgroup-name (wg-current-workgroup t)))
                    frame-names))
        "")
    ))

;; (defun wgsw-read-desc-from-completion ()
;;   (interactive)
;;   (let* ((desc (completing-read
;;                 "Enter workgroup description: "
;;                 (wgsw-get-wg-descs)
;;                 ))
;;          (fram (car (rassoc desc (wgsw-get-frame-description-param)))))
;;     (message (format "desc is %s" desc))
;;     fram
;;     )
;;   )

(defun wgsw-read-desc-from-completion ()
  (interactive)
  (let* ((desc (smex-completing-read
                (wgsw-get-wg-descs)
                ""
                ))
         (fram (car (rassoc desc (wgsw-get-frame-description-param)))))
    (message (format "desc is %s" desc))
    fram
    )
  )

(defun wgsw-goto-workgroup-from-completion ()
  (interactive)
  (let ((fram (wgsw-read-desc-from-completion)))
    (wgsw-goto-workgroup
     (replace-regexp-in-string "^\\(w[0-9][0-9]*\\)\\(.*\\)$" "\\1" (symbol-name fram)))
    )
  )

(defun wgsw-foo ()
  (interactive)
  (let ((fram (wgsw-read-desc-from-completion)))
    (message (format "frame name is %s" fram))
    (message (format "frame is %s name is %s" fram (replace-regexp-in-string "^\\(w[0-9][0-9]*\\)\\(.*\\)$" "\\1" fram)))
    )
  )

(defun wgsw-goto-workgroup (wg-name)
  ;; Seems like this should use mapc...
  (dotimes (s (length (frames-on-display-list)))
    (let ((f (nth s (frames-on-display-list))))
      (select-frame f)
      (wg-switch-to-workgroup (format "%ss%d" wg-name s) t)
      ))
  )

(defun wgsw-set-kbd (i)
  (kbd (if (fboundp 'wgsw-keybinding-func)
           (wgsw-keybinding-func i)
         (format wgsw-keybinding-template i))))

(defun wgsw-set-wg-keybindings ()
  (interactive)
  (dotimes (i wgsw-number-of-workgroups)
    (let ((w (format "w%d" (1+ i)))
          )
      (global-set-key (wgsw-set-kbd (1+ i)) `(lambda () (interactive) (wgsw-goto-workgroup ,w)))))
  )

(defun wgsw-save-wg-session ()
  (interactive)
  ;; (wg-set-session-parameter (wg-current-session) 'frame-desc wgsw-frame-names)
  (wg-write-session-file wg-default-session-file)
  )

(defun wgsw-start-workgroups ()
  "Starts workgroups from workgroups2, assuming 2 screens."
  (interactive)
  (make-frame)

  (wgsw-set-wg-keybindings)
  (setq wg-emacs-exit-save-behavior nil)
  (wg-find-session-file wg-default-session-file)
  (add-hook 'auto-save-hook #'wgsw-save-wg-session)
  (add-hook 'kill-emacs-hook #'wgsw-save-wg-session)

  ;(wg-session-parameter (wg-current-session t) 'frame-desc '())
  (wg-session-parameter 'frame-desc '())
  (wgsw-goto-workgroup "w1"))

(defun wgsw-get-name ()
  (interactive)
  (wg-workgroup-name (wg-current-workgroup t))
  )

(defun wgsw-setup-workgroups (arg)
  "Initialize workgroups from workgroups2, assuming 2 screens."
  (interactive "P")
  (if arg
        (delete-file wg-default-session-file t)
    )

  ;;(wg-set-session-parameter (wg-current-session t) 'frame-desc '(("hi" "")))

  (dotimes (i (- wgsw-number-of-frames (length (frames-on-display-list))))
    (make-frame))

  (if (not (file-exists-p wg-default-session-file))
      (progn

        (dotimes (s (length (frames-on-display-list)))
          (let ((f (nth s (frames-on-display-list))))
            (select-frame f)
            (dotimes (i wgsw-number-of-workgroups)
              (wg-create-workgroup (format "w%ds%d" (1+ i) s))
              ;; (let ((frame-name (format "w%ds%d" (1+ i) s)))
              ;;   (wg-create-workgroup frame-name)
              ;;   ;(wgsw-set-frame-description frame-name (lax-plist-get wgsw-frame-name-defaults frame-name))
              ;;   )
              )
            ))
        (mapc 'wgsw-set-frame-description-from-default (wg-workgroup-names))
        ))

  (wgsw-set-wg-keybindings)
  (if (not (file-exists-p (wg-get-session-file)))
      (progn
        (wg-save-session t)
        ;;(wg-save-frames)
        ;;(wg-write-session-file wg-default-session-file)
        ))
  (add-hook 'auto-save-hook #'wgsw-save-wg-session)
  (add-hook 'kill-emacs-hook #'wgsw-save-wg-session)
  )

(defun wgsw-get-properties (p)
  "Returns a list of the properties of p.
Assumes each property is a single item. Discards
properties that are an empty string."
  (let ((x))
    (loop while p do
          (if (not (string= (cadr p) ""))
              (setq x (cons (cadr p) x))
          )
          (setq p (cddr p))
          )
    x)
)

(defun wgsw-find-blanks ()
  "Returns a list of keys of wgsw-get-frame-description-param for
  which the property is blank."

  (loop for (key . value) in (wgsw-get-frame-description-param)
        if (not (eq (type-of value) 'string))
        collect key into retval
        else
        if (string= "" value)
        collect key into retval
        finally return retval
        )
)

(defun wgsw-get-wg-descs ()
  (mapcar 'cdr (wgsw-get-frame-description-param))
)

(defun wgsw-print ()
  (interactive)
  (mapc (lambda (x) (print x)) (wgsw-get-frame-description-param))
  )

(defun wgsw-a ()
  (interactive)
  (mapc (lambda (x) (print x)) (wgsw-find-blanks))
)



(provide 'workgroup-switcher)
