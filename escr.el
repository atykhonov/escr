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

(defvar escr-x-margin 0
  "X margin. If 0 then a screenshot will probably include left
Emacs border. You may want to increase that value so a screenshot
will looks like a little bit better.")

(defun escr-shot ()
  (interactive)
  (let ((window-id (frame-parameter (selected-frame) 'window-id))
        (char-height (frame-char-height))
        (char-width (frame-char-width))
        (filename "/home/demi/test2.jpg")
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
    (setq screenshot-x escr-x-margin)
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
