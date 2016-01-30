Usage
-----
workgroup-switcher is designed to make it convenient to switch
between sets of workgroups across multiple frames. If, for
instance, you have two monitors, you might have one frame in one
monitor and one frame in the other. You can then switch workgroups
in parallel across both monitors.

To use, install and configure as below. If you're using the default
settings, you'll have two frames and twelve workspaces, accessed by
s-fn, where fn is one of the twelve function keys.

1. Start the workspace switcher going with M-x wgsw-start-workgroups
   
2. Arrange the frames as you like.
   
3. Switch to a workspace with s-fn. s-f3 will switch to workspace 3
   and so on. The workspace configuration is automatically saved when
   auto-save activates, or when you exit Emacs.
   
4. Use the command M-x wgsw-set-current-frame-description to set a
   description of the current frame. This description will show up in
   the title bar of the frame. The name of the current workspace is
   shown in parentheses on the mode line. It has the form (wnsm) where
   n identifies the workspace and m identifies the frame.

Install
----------------------
Add the lines below to your .emacs configuration.

(require 'workgroup-switcher)

<settings here>


Configure
----------------------

Change number of frames
(setq wgsw-number-of-frames 3)

Change number of workspaces
(setq wgsw-number-of-workgroups 10)

Change the key bindings to something very similar to the default:
C-<fn>, where n is the number of the workspace
(setq wgsw-keybinding-template "C-<f%d>")

If you want something else, you can define a function
wgsw-keybinding-func that takes the number of the workspace (from 1
to wgsw-number-of-workgroups) and returns a string suitable for
input to kbd.

(defun wgsw-keybinding-func (i)
  (if (< i 6)
      (format "C-<f%d>" i)
    (format "C-S-<f%d>" (- i 5)))
  )
This function will bind C-f1 through C-f5 to switch to workspaces 1
through 5, and keys C-S-f1 through C-S-f5 to switch to workspaces 6
through 10.

You may want to bind wgsw-set-current-frame-description to a key to
make it convenient to set a description for the current frame.

