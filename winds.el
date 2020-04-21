;;; winds.el --- Window configuration switcher grouped by workspaces                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Javier A. Pollak

;; Author: Javier A. Pollak <javi.po.123@gmail.com>
;; Maintainer: Javier A. Pollak <javi.po.123@gmail.com>
;; Created: 17 Apr 2020
;; Keywords: convenience
;; Version: 1.0.0
;; Homepage: https://github.com/Javyre/winds.el
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Window configuration switcher grouped by workspaces
;;
;; winds.el is very similar to [[https://github.com/wasamasa/eyebrowse/][eyebrowse]] and other window config
;; switchers, but allows for having multiple "workspaces" grouping sets
;; of window config slots.
;;
;; This small package was started because I tend to have multiple
;; unrelated projects open at once, and need to keep them open.  I do
;; not want to cycle through unrelated window configs to get to what I
;; want and I want to keep only one fullscreen Emacs frame open.
;;
;; (This package has basic support for multiple frames)
;;
;; * Screenshot
;;   [[file:scrot.png]]
;;
;; * Install
;;
;;   This package has not yet been submitted to Melpa.  I have no need
;;   for it to be on Melpa as I use =straight.el= and I made this package
;;   to scratch my own itch.
;;
;;   If anyone thinks this would be useful to them please let me know and
;;   I'll be glad to submit it to Melpa.
;;
;;   #+BEGIN_SRC elisp
;;   (use-package winds
;;     :straight (winds :type git :host github :repo "Javyre/winds.el"))
;;   #+END_SRC
;;
;; * Getting Started
;;
;;   To get started, bind some keys to ~winds-goto~:
;;
;;   #+BEGIN_SRC elisp
;;   (global-set-key (kbd "M-1") (lambda () (interactive) (winds-goto :ws 1)))
;;   (global-set-key (kbd "M-2") (lambda () (interactive) (winds-goto :ws 2)))
;;   (global-set-key (kbd "M-3") (lambda () (interactive) (winds-goto :ws 3)))
;;   (global-set-key (kbd "C-c 1") (lambda () (interactive) (winds-goto :cfg 1)))
;;   (global-set-key (kbd "C-c 2") (lambda () (interactive) (winds-goto :cfg 2)))
;;   (global-set-key (kbd "C-c 3") (lambda () (interactive) (winds-goto :cfg 3)))
;;   #+END_SRC
;;
;;   You might also want to bind ~next~/~prev~ and ~close~
;;
;;   #+BEGIN_SRC elisp
;;   (global-set-key (kbd "C-c <")  'winds-next)
;;   (global-set-key (kbd "C-c >")  'winds-prev)
;;   (global-set-key (kbd "C-c \\") 'winds-close)
;;   (global-set-key (kbd "C-<")    'winds-cfg-next)
;;   (global-set-key (kbd "C->")    'winds-cfg-prev)
;;   (global-set-key (kbd "C-\\")   'winds-cfg-close)
;;   #+END_SRC
;;
;;   To disable the status message when changing window configs:
;;
;;   #+BEGIN_SRC elisp
;;   (setq winds-display-status-msg nil)
;;   #+END_SRC
;;
;;   For a simple mode-line indicator, add this to your ~mode-line-format~:
;;
;;   #+BEGIN_SRC elisp
;;     (:eval (format "%s|%s " (winds-get-cur-ws) (winds-get-cur-cfg)))
;;   #+END_SRC
;;
;;   For example (dumb example):
;;
;;   #+BEGIN_SRC elisp
;;     (setq mode-line-format
;;           `(,mode-line-format
;;             (:eval (format "%s|%s "
;;                            (winds-get-cur-ws)
;;                            (winds-get-cur-cfg)))))
;;   #+END_SRC

;;; Change Log:
;;; Code:

(require 'cl-lib)

;; Custom

(defgroup winds nil
  "A window configuration switcher for multiple workspaces"
  :group 'convenience
  :prefix "winds-")

(defcustom winds-default-ws 1
  "Default selected workspace."
  :type 'integer
  :group 'winds)
(defcustom winds-default-cfg 1
  "Default selected window config slot."
  :type 'integer
  :group 'winds)

(defcustom winds-display-status-msg t
  "Whether to display a status message upon switching window config."
  :type 'boolean
  :group 'winds)

(defcustom winds-init-cfg-hook
  (list (lambda (w c)
          (ignore w c)
          (delete-other-windows)
          (switch-to-buffer "*scratch*")))
  "Hook called to create a new layout upon opening a new window config slot.

The hook receives two parameters: the window-id and cfg-id of the new slot
Set to `nil` to not run any initialization"
  :type 'hook
  :group 'winds)


;; Vars/Decls

(cl-defstruct (winds-workspace (:type vector) :named)
  cfgs      ;; window config slots alist
  last-sel) ;; last selected slot id
(defvar winds-*workspaces* '())

(defun winds-get-cur-ws (&optional frame)
  "Get the currently selected workspace id in FRAME or the current frame."
  (or (frame-parameter frame 'winds--cur-ws) winds-default-ws))
(defun winds-get-cur-cfg (&optional frame)
  "Get the currently selected window config slot id in FRAME or the current frame."
  (or (frame-parameter frame 'winds--cur-cfg) winds-default-cfg))

(defun winds--set-cur-ws (frame value)
  "Set the currently selected workspace id in FRAME or the current frame to VALUE."
  (set-frame-parameter frame 'winds--cur-ws value))
(defun winds--set-cur-cfg (frame value)
  "Get the currently selected window config slot id in FRAME or the current frame to VALUE."
  (set-frame-parameter frame 'winds--cur-cfg value))

;; Private

(defun winds--get-or-create-ws (wsid)
  "Get or create workspace in slot WSID."
  (let ((ws (alist-get wsid winds-*workspaces*)))
    (unless ws
      (setf ws (make-winds-workspace :cfgs '()
                                     :last-sel (if (eq wsid (winds-get-cur-ws))
                                                   (winds-get-cur-cfg)
                                                 winds-default-cfg)))
      (setf (alist-get wsid winds-*workspaces*) ws))
    ws))

(defun winds--save-cfg-if-empty ()
  "Save the current window config to the current slot if slot is empty."
  (let* ((wsid   (winds-get-cur-ws))
         (cfgid  (winds-get-cur-cfg))
         (ws    (winds--get-or-create-ws wsid))
         (cfgs  (winds-workspace-cfgs ws)))
    (unless (alist-get cfgid cfgs)
      (winds-save-cfg :ws wsid :cfg cfgid))))

(defun winds--get-wsids ()
  "Get the current set of workspace ids."
  (cl-loop for assoc in winds-*workspaces*
           collect (car assoc) into keys
           finally return (progn
                            (cl-pushnew (winds-get-cur-ws) keys)
                            keys)))

(cl-defun winds--get-cfgids (&optional (wsid (winds-get-cur-ws)))
  "Get the current set of window config ids."
  (let ((ws (winds--get-or-create-ws wsid)))
    (cl-loop for assoc in (winds-workspace-cfgs ws)
             collect (car assoc) into keys
             finally return (progn
                              (when (eq wsid (winds-get-cur-ws))
                                (cl-pushnew (winds-get-cur-cfg) keys))
                              keys))))

;; Public

(defun winds-display-status-msg ()
  "Display a status message in the echo area with the current ws id and cfg id."
  (interactive)
  (let* ((wsids  (sort (winds--get-wsids) #'<))
         (cfgids (sort (winds--get-cfgids) #'<))
         (bg       (face-attribute 'mode-line-inactive :background))
         (sel-fg   (face-attribute 'mode-line :foreground))
         (unsel-fg (face-attribute 'mode-line-inactive :foreground))
         (sel-face   `(:background ,bg :foreground ,sel-fg))
         (unsel-face `(:background ,bg :foreground ,unsel-fg))
         (msg-left (mapcar
                    (lambda (id) (if (eq id (winds-get-cur-cfg))
                                     (propertize (format "%s " id) 'face sel-face)
                                   (propertize (format "%s " id) 'face unsel-face)))
                    cfgids))
         (msg-right (mapcar
                     (lambda (id) (if (eq id (winds-get-cur-ws))
                                      (propertize (format " %s" id) 'face sel-face)
                                    (propertize (format " %s" id) 'face unsel-face)))
                     wsids))
         (msg-left  (cl-reduce #'concat msg-left))
         (msg-right (cl-reduce #'concat msg-right))
         (msg-left  (concat (propertize "C " 'face unsel-face) msg-left))
         (msg-right (concat msg-right (propertize " W" 'face unsel-face))))

    ;; Don't spam *Messages*
    (let ((message-log-max nil))
      (message "%s|%s" msg-left msg-right))))

;;;###autoload
(cl-defun winds-save-cfg (&key ((:ws  wsid)  (winds-get-cur-ws))
                               ((:cfg cfgid) (winds-get-cur-cfg)))
  "Save current window configuration into workspace ws, config cfg.

Call interactively to be prompted for a workspace and window config to save to.

Call interactively with a prefix argument to save to the current window config slot
 in the current workspace."

  (interactive (unless current-prefix-arg
                 (list :ws (read-from-minibuffer
                            "Workspace to save window config to (blank for current): "
                            nil nil t nil (format "%s" (winds-get-cur-ws)))
                       :cfg (read-from-minibuffer
                             "Window config slot to save to (blank for current): "
                             nil nil t nil (format "%s" (winds-get-cur-cfg))))))

  (let ((ws (winds--get-or-create-ws wsid)))
      (setf (winds-workspace-last-sel ws) cfgid)
      (setf (alist-get cfgid (winds-workspace-cfgs ws))
            (window-state-get nil t))))

;;;###autoload
(cl-defun winds-goto (&key ((:ws wsid) (winds-get-cur-ws))
                           ((:cfg cfgid) nil)
                           (do-save t))
  "Switch to another workspace and/or window config slot.

Call interactively to be prompted for a workspace and window config to swtich to.

Call interactively with a prefix argument to go to the last selected
window config slot in the current workspace."

  (interactive (unless current-prefix-arg
                 (list :ws (read-from-minibuffer
                            "Workspace to switch to (blank for current): "
                            nil nil t nil (format "%s" (winds-get-cur-ws)))
                       :cfg (read-from-minibuffer
                             "Window config slot to switch to (blank for last selected): "
                             nil nil t nil "nil"))))

  (let ((ws (winds--get-or-create-ws wsid)))
    ;; Return to last selected cfg for selected ws
    (unless cfgid (setf cfgid (winds-workspace-last-sel ws)))

    ;; Save current cfg before leaving
    (when do-save (winds-save-cfg))

    (winds--set-cur-ws nil wsid)
    (winds--set-cur-cfg nil cfgid)

    (let* ((cfgs          (winds-workspace-cfgs ws))
           (window-config (alist-get cfgid cfgs)))
      (if window-config
          ;; Goto
          (window-state-put window-config (frame-root-window) 'safe)

        ;; Init new win config
        (run-hook-with-args 'winds-init-cfg-hook wsid cfgid)
        (winds-save-cfg :ws wsid :cfg cfgid))))
  (when winds-display-status-msg
    (winds-display-status-msg)))

;;;###autoload
(defun winds-next ()
  "Go to next workspace slot."
  (interactive)
  (winds--save-cfg-if-empty)
  (let* ((wsids    (sort (winds--get-wsids) #'<))
         (next-pos (1+ (cl-position (winds-get-cur-ws) wsids))))
    (when (= next-pos (length wsids))
      (message "Already on last workspace. Wrapping back to first.")
      (setf next-pos 0))
    (winds-goto :ws (nth next-pos wsids))))

;;;###autoload
(defun winds-prev ()
  "Go to previous workspace slot."
  (interactive)
  (winds--save-cfg-if-empty)
  (let* ((wsids    (sort (winds--get-wsids) #'>))
         (next-pos (1+ (cl-position (winds-get-cur-ws) wsids))))
    (when (= next-pos (length wsids))
      (message "Already on first workspace. Wrapping back to last.")
      (setf next-pos 0))
    (winds-goto :ws (nth next-pos wsids))))

;;;###autoload
(defun winds-cfg-next ()
  "Go to next window config slot."
  (interactive)
  (winds--save-cfg-if-empty)
  (let* ((cfgids   (sort (winds--get-cfgids) #'<))
         (next-pos (1+ (cl-position (winds-get-cur-cfg) cfgids))))
    (when (= next-pos (length cfgids))
      (message "Already on last window config. Wrapping back to first.")
      (setf next-pos 0))
    (winds-goto :cfg (nth next-pos cfgids))))

;;;###autoload
(defun winds-cfg-prev ()
  "Go to previous window config slot."
  (interactive)
  (winds--save-cfg-if-empty)
  (let* ((cfgids   (sort (winds--get-cfgids) #'>))
         (next-pos (1+ (cl-position (winds-get-cur-cfg) cfgids))))
    (when (= next-pos (length cfgids))
      (message "Already on first window config. Wrapping back to last.")
      (setf next-pos 0))
    (winds-goto :cfg (nth next-pos cfgids))))

;;;###autoload
(defun winds-close (wsid)
  "Close workspace slot WSID.

Close workspace slot WSID and switch to nearest slot or `winds-default-ws'
 if none open.  If interactive, you are prompted for an id or blank to close
 current ws."

  (interactive (list (read-from-minibuffer "Workspace to close (blank for current): "
                                           nil nil t nil
                                           (format "%s" (winds-get-cur-ws)))))
  (let ((wsids (sort (winds--get-wsids) #'<)))
    (if (memql wsid wsids)
        (progn
          (setf (alist-get wsid winds-*workspaces* nil 'remove) nil)
          (when (eq wsid (winds-get-cur-ws))
            (let ((goto (car (cl-remove-if (lambda (e) (eq wsid e)) wsids))))
              (winds-goto :ws (or goto winds-default-ws) :do-save nil))))
      (message "Workspace %s does not exist!" wsid))))

;;;###autoload
(defun winds-cfg-close (cfgid)
  "Close window config slot CFGID.

Close window config slot CFGID and switch to nearest slot or `winds-default-cfg'
 if none open.  If interactive, you are prompted for an id or blank to close
 current cfg"

  (interactive (list (read-from-minibuffer "Window config to close (blank for current): "
                                           nil nil t nil
                                           (format "%s" (winds-get-cur-cfg)))))
  (let ((cfgids (sort (winds--get-cfgids) #'<))
        (ws (winds--get-or-create-ws (winds-get-cur-ws))))
    (if (memql cfgid cfgids)
        (progn
          (setf (alist-get cfgid (winds-workspace-cfgs ws) nil 'remove) nil)
          (when (eq cfgid (winds-get-cur-cfg))
            (let ((goto (car (cl-remove-if (lambda (e) (eq cfgid e)) cfgids))))
              (winds-goto :cfg (or goto winds-default-cfg) :do-save nil))))
      (message "Window config %s does not exist!" cfgid))))

(provide 'winds)
;;; winds.el ends here
