;;; km-window.el --- Misc window commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-window
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (transient "0.4.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Misc window commands

;;; Code:

(require 'transient)


(declare-function winner-undo "winner")

(defun km-window-buffers-visible (&optional buffer-list)
  "Return a list of visible buffers from BUFFER-LIST."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (seq-remove (lambda (b)
                      (memq b buffer-list))
                    buffers)
      (delete-dups buffers))))

(defun km-window-buffer-mode-derivered-p (modes buffer)
  "Non-nil if major mode is derived from one of MODES in BUFFER."
  (apply #'provided-mode-derived-p
         (buffer-local-value 'major-mode buffer)
         modes))

(defun km-window-buffer-mode-memq (modes buffer)
  "Return non-nil if major mode in BUFFER is an element of MODES."
  (memq (buffer-local-value 'major-mode buffer)
        (if (proper-list-p modes)
            modes
          (list modes))))

(defun km-window-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of BUFFER-LIST with `major-mode' listed in MODES.
MODES can be either list of modes, or a mode.

If DERIVED-P is non-nil, test with `derived-mode-p', otherwise use `eq'."
  (seq-filter (apply-partially
               (if derived-p #'km-window-buffer-mode-derivered-p
                 #'km-window-buffer-mode-memq)
               (if (proper-list-p modes)
                   modes
                 (list modes)))
              (or buffer-list (buffer-list))))

(defmacro km-window-with-other-window (&rest body)
  "Execute BODY in other window.
If other window doesn't exists, split selected window right."
  `(with-selected-window
       (let ((wind-target
              (if (minibuffer-window-active-p (selected-window))
                  (with-minibuffer-selected-window
                    (let ((wind (selected-window)))
                      (or
                       (window-right wind)
                       (window-left wind)
                       (progn (split-window-sensibly) wind))))
                (let ((wind (selected-window)))
                  (or
                   (window-right wind)
                   (window-left wind)
                   (progn (split-window-sensibly) wind))))))
         wind-target)
     (progn ,@body)))

;;;###autoload
(defun km-window-curr-buffer-to-other-window-and-pop-prev-buffer ()
  "Move current buffer to another window and show previous buffer."
  (interactive)
  (let ((buff (current-buffer))
        (wnd (selected-window)))
    (km-window-with-other-window
     (pop-to-buffer-same-window buff))
    (with-selected-window wnd
      (previous-buffer))))

(defun km-window-scroll-other-window (&optional arg)
  "Without ARG or ARG is positive integer scroll other window down.
If ARG is negative scroll up.
Works with xwidgets and eaf."
  (unless arg (setq arg 1))
  (let ((orig-buff (current-buffer))
        (xwidgets-buffer))
    (km-window-with-other-window
     (cond ((and (fboundp 'get-buffer-xwidgets)
                 (get-buffer-xwidgets (current-buffer)))
            (funcall-interactively (if (> arg 0)
                                       'xwidget-webkit-scroll-down
                                     'xwidget-webkit-scroll-up)))
           ((progn (setq xwidgets-buffer
                         (car
                          (delq orig-buff
                                (km-window-buffers-in-mode
                                 'xwidget-webkit-mode
                                 (km-window-buffers-visible))))))
            (with-current-buffer xwidgets-buffer
              (funcall-interactively (if (> arg 0)
                                         'xwidget-webkit-scroll-down
                                       'xwidget-webkit-scroll-up))))
           ((and (eq major-mode 'eaf-mode)
                 (fboundp 'eaf-py-proxy-scroll_up_page))
            (funcall-interactively (if (> arg 0)
                                       'eaf-py-proxy-scroll_down_page
                                     'eaf-py-proxy-scroll_up_page)))
           (t (funcall-interactively (if (> arg 0)
                                         'scroll-down-command
                                       'scroll-up-command)))))))

;;;###autoload
(defun km-window-scroll-other-window-up ()
  "Scroll other window up.
Also works with xwidgets and eaf."
  (interactive)
  (km-window-scroll-other-window 1))

;;;###autoload
(defun km-window-scroll-other-window-down ()
  "Scroll other window down.
Also works with xwidgets and eaf."
  (interactive)
  (km-window-scroll-other-window -1))

;; dedicated window

;;;###autoload
(defun km-window-select-dedicated ()
  "Select a dedicated window possibly with completions in minibuffer.
If the selected frame has only one a dedicated window, select it,
if there are more then one window - read it in minibuffer.
If the selected frame doesn't contains a dedicated windows,
do nothing."
  (interactive)
  (let* ((winds (seq-filter #'window-dedicated-p (window-list)))
         (len (length winds)))
    (cond ((> len 1)
           (let ((buff
                  (completing-read "Window: " (mapcar #'window-buffer winds))))
             (select-window (get-buffer-window buff))))
          ((= len 1)
           (select-window (car winds))))))

;;;###autoload
(defun km-window-dedicated-unset-all ()
  "Unmark all dedicated windows."
  (interactive)
  (dolist (wind (window-list))
    (when (window-dedicated-p wind)
      (set-window-dedicated-p wind nil))))

;;;###autoload
(defun km-window-toggle-window-dedicated ()
  "Mark whether selected window is dedicated to its buffer."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (status (set-window-dedicated-p window
                                         (not (window-dedicated-p window)))))
    (message
     (if
         status
         "%s: is dedicated!"
       "%s is not dedicated!")
     (current-buffer))))

(defun km-window-transient-get-dedicated-description ()
  "Return description with count or name of dedicated buffer."
  (or (when-let* ((wns (seq-filter #'window-dedicated-p
                                   (window-list)))
                  (suffix
                   (cond ((= (length wns) 1)
                          (format "(%s)"
                                  (buffer-name
                                   (window-buffer
                                    (car
                                     wns)))))
                         ((> (length wns) 1)
                          (format "(%d windows)"
                                  (length wns))))))
        (propertize suffix 'face 'success))
      ""))

;;;###autoload
(defun km-window-suffix-enlarge-horizontally ()
  "Make selected window 1 columns wider."
  (interactive)
  (enlarge-window-horizontally 1)
  (transient-setup transient-current-command))


;;;###autoload
(defun km-window-suffix-shrink-horizontally ()
  "Make selected window 1 columns narrower."
  (interactive)
  (shrink-window-horizontally 1)
  (transient-setup transient-current-command))

;;;###autoload
(defun km-window-suffix-enlarge-vertically ()
  "Enlarge current window by one line vertically."
  (interactive)
  (enlarge-window 1 nil)
  (transient-setup transient-current-command))

;;;###autoload
(defun km-window-suffix-shrink-window-vertically ()
  "Shrink the current window by one line vertically."
  (interactive)
  (shrink-window 1 nil)
  (transient-setup transient-current-command))

;;;###autoload (autoload 'km-window-transient "km-window" nil t)
(transient-define-prefix km-window-transient ()
  "Command dispatcher for window commands."
  :transient-suffix  #'transient--do-call
  :transient-non-suffix #'transient--do-exit
  [:description
   (lambda ()
     (format "%s" (selected-window)))
   ("t"  km-window-toggle-window-dedicated
    :description
    (lambda ()
      (concat "Make " (buffer-name (current-buffer)) " sticky"
              " "
              (if
                  (window-dedicated-p (selected-window))
                  (propertize "(on)" 'face 'success)
                (propertize "(off)" 'face 'error)))))
   ("s" km-window-select-dedicated
    :inapt-if
    (lambda ()
      (let ((wns (seq-filter #'window-dedicated-p
                             (window-list))))
        (or
         (= (length wns) 0)
         (and (= (length wns) 1)
              (eq (selected-window)
                  (car wns))))))
    :description (lambda ()
                   (concat "Select dedicated window "
                           (km-window-transient-get-dedicated-description))))
   ("A" km-window-dedicated-unset-all
    :description (lambda ()
                   (concat "Unset "
                           (km-window-transient-get-dedicated-description)
                           " dedicated windows")))
   ("o" "Other window" other-window)
   ("O" "Current buffer to another window and show previous buffer"
    km-window-curr-buffer-to-other-window-and-pop-prev-buffer)]
  [:description (lambda ()
                  (format "Resize (width %d) (height %d)" (window-width)
                          (window-height)))
   ("<right>" "Increase width"
    km-window-suffix-enlarge-horizontally
    :transient nil
    :inapt-if-not (lambda ()
                    (window-resizable (selected-window)
                                      1 t)))
   ("<left>" "Decrease width"
    km-window-suffix-shrink-horizontally
    :inapt-if-not (lambda ()
                    (window-resizable (selected-window)
                                      -1 t))
    :transient nil)
   ("<up>" "v+" km-window-suffix-enlarge-vertically
    :inapt-if window-full-height-p
    :transient nil)
   ("<down>" "v-" km-window-suffix-shrink-window-vertically
    :inapt-if window-full-height-p
    :transient nil)])


;;;###autoload
(defun km-window-kill-buffer-window-and-undo ()
  "Kill the current buffer and delete the selected window.
Then switch back to an earlier window configuration."
  (interactive)
  (require 'winner)
  (kill-buffer-and-window)
  (winner-undo))

(provide 'km-window)
;;; km-window.el ends here