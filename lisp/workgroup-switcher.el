;;; workgroup-switcher.el -- An interface for switching workgroups between frames
;;
;; Copyright (C) 2014 Bob Forgey
;;
;; Author: Bob Forgey <sesamemucho at gmail dot com>
;; Keywords: session management window-configuration persistence
;; Homepage: https://github.com/sesamemucho/workgroup-switcher
;; Package-requires:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;;
;;; Commentary:
;;
;; This is my customizations for the excellent Emacs session manager
;; workgroups2. It provides for automatic creation (and persistance)
;; of workspaces for a desired number of frames and workspaces. It
;; also allows the user to name each workspace, and sets up
;; keybindings to switch between workspaces.
;;
;; If you find a bug or other issue, please post it here:
;; https://github.com/sesamemucho/workgroup-switcher/issues
;;
;; Usage
;; -----
;; workgroup-switcher is designed to make it convenient to switch
;; between sets of workgroups across multiple frames. If, for
;; instance, you have two monitors, you might have one frame in one
;; monitor and one frame in the other. You can then switch workgroups
;; in parallel across both monitors.
;; 
;; To use, install and configure as below. If you're using the default
;; settings, you'll have two frames and twelve workspaces, accessed by
;; s-fn, where fn is one of the twelve function keys.
;; 
;; Start the workspace switcher going with M-x wgsw-start-workgroups
;; 
;; 1. Arrange the frames as you like.
;; 
;; Switch to a workspace with s-fn. s-f3 will switch to workspace 3
;; and so on. The workspace configuration is automatically saved when
;; auto-save activates, or when you exit Emacs.
;; 
;; Use the command M-x wgsw-set-current-frame-description to set a
;; description of the current frame. This description will show up in
;; the title bar of the frame. The name of the current workspace is
;; shown in parentheses on the mode line. It has the form (wnsm) where
;; n identifies the workspace and m identifies the frame.
;;
;; Install
;; ----------------------
;; Add the lines below to your .emacs configuration.
;;
;; (require 'workgroup-switcher)
;;
;; <settings here>
;;
;;
;; Configure
;; ----------------------
;;
;; Change number of frames
;; (setq wgsw-number-of-frames 3)
;;
;; Change number of workspaces
;; (setq wgsw-number-of-workgroups 10)
;;
;; Change the key bindings to something very similar to the default:
;; C-<fn>, where n is the number of the workspace
;; (setq wgsw-keybinding-template "C-<f%d>")
;;
;; If you want something else, you can define a function
;; wgsw-keybinding-func that takes the number of the workspace (from 1
;; to wgsw-number-of-workgroups) and returns a string suitable for
;; input to kbd.
;;
;; (defun wgsw-keybinding-func (i)
;;   (if (< i 6)
;;       (format "C-<f%d>" i)
;;     (format "C-S-<f%d>" (- i 5)))
;;   )
;; This function will bind C-f1 through C-f5 to switch to workspaces 1
;; through 5, and keys C-S-f1 through C-S-f5 to switch to workspaces 6
;; through 10.
;; 
;; You may want to bind wgsw-set-current-frame-description to a key to
;; make it convenient to set a description for the current frame.

(require 'workgroups2)

  (defvar wgsw-frame-names '()
    "Property list of wg names vs descriptions.")

  (defvar wgsw-number-of-frames 2
    "Desired number of frames")

  (defvar wgsw-number-of-workgroups 12
    "Desired number of workgroups")

  (defvar wgsw-keybinding-template "s-<f%d>"
    "Template for keybinding to switch between workspaces")

  (defvar wgsw-keybinding-func)

  (defun wgsw-set-current-frame-description (desc)
    "Set the description for the current wg frame to `desc'"
    (interactive "MEnter frame description: ")
    (let ((wg-name (wg-workgroup-name (wg-current-workgroup t))))
      (wgsw-set-frame-description wg-name desc)
      )
    )

  (defun wgsw-set-frame-description (wg-name desc)
    "Set the description for wg workgroup wg-name to `desc'"
    (setq wgsw-frame-names (lax-plist-put
                               wgsw-frame-names
                               wg-name
                               desc)
          ))

  (setq frame-title-format '(multiple-frames ("%b" "     " (:eval (wgsw-show-frame-desc)) )
                                             ("" invocation-name "@" system-name)))

  (defun wgsw-show-frame-desc ()
    (interactive)
    (or (lax-plist-get wgsw-frame-names
                       (wg-workgroup-name (wg-current-workgroup t)))
        "")
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

    (wgsw-goto-workgroup "w1")
    )

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

    (dotimes (i (- wgsw-number-of-frames (length (frames-on-display-list))))
      (make-frame))

    (if (not (file-exists-p wg-default-session-file))
        (progn

        (dotimes (s (length (frames-on-display-list)))
          (let ((f (nth s (frames-on-display-list))))
            (select-frame f)
            (dotimes (i wgsw-number-of-workgroups)
              (wg-create-workgroup (format "w%ds%d" (1+ i) s)))
          ))

        ))

    (wgsw-set-wg-keybindings)
    (if (not (file-exists-p wg-default-session-file))
        (progn
          (wg-save-session-as wg-default-session-file t)
          ;;(wg-save-frames)
          ;;(wg-write-session-file wg-default-session-file)
          ))
    (add-hook 'auto-save-hook #'wgsw-save-wg-session)
    (add-hook 'kill-emacs-hook #'wgsw-save-wg-session)
    )

(provide 'workgroup-switcher)
