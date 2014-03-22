;;; escr.el --- Tool for making buffers screenshots.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/escr
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:

;; Assuming that the file `escr.el' is somewhere on the load path, add the following
;; lines to your `.emacs' file:

;; (require 'escr)
;; (global-set-key "\C-cs" 'escr-shot)
;;
;; Change the key bindings to your liking.

;;; Code:

(defvar escr-column 80
  "Indicates how many columns include to make visible in the
screenshot.")

(defvar escr-exclude-fringes t
  "Exclude fringes from the screenshot.")

(defvar escr-screenshot-directory (concat
                                   user-emacs-directory
                                   "screenshots")
  "Path to directory where screenshots are going to be stored.")

(defvar escr-filename-format "%Y-%m-%d-%H%M%S.png"
  "Screenshots filename format.")



(defun escr-region-screenshot ()
  (interactive)
  (let ((window-id (frame-parameter (selected-frame) 'window-id))
        (char-height (frame-char-height))
        (char-width (frame-char-width))
        (filename (expand-file-name
                   (concat escr-screenshot-directory
                           "/"
                           (format-time-string escr-filename-format
                                               (current-time)))))
        (window-start-line nil)
        (window-region-beginning-line nil)
        (window-region-end-line nil)
        (screenshot-height 0)
        (screenshot-width 0)
        (screenshot-x 0)
        (screenshot-y 0)
        (selection-start (region-beginning))
        (selection-end (region-end))
        (current-point (point))
        (crop ""))

    (escr--check-directory)

    (setq window-start-line (line-number-at-pos (window-start)))

    (goto-char selection-start)
    (setq window-region-beginning-line (line-number-at-pos))

    (goto-char selection-end)
    (setq window-region-end-line (line-number-at-pos))

    (goto-char current-point)

    (setq screenshot-width (* char-width escr-column))
    (setq screenshot-height (* (+ (- window-region-end-line
                                     window-region-beginning-line)
                                  1)
                               char-height))

    (when escr-exclude-fringes
      (setq screenshot-x (nth 0 (window-fringes))))

    (setq screenshot-y (* (- window-region-beginning-line
                             window-start-line)
                          char-height))

    (setq crop (format "%sx%s+%s+%s"
                       screenshot-width
                       screenshot-height
                       screenshot-x
                       screenshot-y))

    (deactivate-mark t)
    (redisplay t)
    (call-process "import" nil nil nil
                  "-window" window-id
                  "-crop" crop 
                  "-quality" "100" 
                  filename)))

(defun escr--check-directory ()
  (when (not (file-exists-p escr-screenshot-directory))
    (if (y-or-n-p (format "Directory %s does not exist. Create it?"
                          escr-screenshot-directory))
        (make-directory escr-screenshot-directory)
      (error "Please create the directory first."))))

(defun escr-org-babel-after-execute-hook ()
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (mode (intern (format "%s-mode" lang)))
         (params (nth 2 info))
         (scs-param (cdr (assoc :scs params)))
         (escr-buffer nil))
    (when (string= scs-param "yes")
      (setq escr-buffer (get-buffer-create "*Escr*"))
      (with-current-buffer escr-buffer
        (switch-to-buffer escr-buffer)
        (erase-buffer)
        (insert body)
        (when (fboundp mode)
          (funcall mode))
        (jit-lock-fontify-now (point-min) (point-max))
        (mark-whole-buffer)
        (escr-shot)
        (kill-buffer)))))

(defun escr-setup ()
  (interactive)
  (add-hook 'org-babel-after-execute-hook
            'escr-org-babel-after-execute-hook))
