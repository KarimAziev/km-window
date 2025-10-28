;;; km-window.el --- Misc window commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-window
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (transient "0.5.3"))
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

(defcustom km-window-split-width-thresholds '((nil 349 160) ; width <= 349  -> 160
                                              (350 nil 200)) ; width >= 350  -> 200
  "Thresholds for determining window split width based on frame width.

A list of width thresholds for determining the `split-width-threshold'
value based on the current frame width.

Each element in the list should be a list containing three items:
a minimum width, a maximum width, and the threshold value to use
when the frame width falls within this range.

The minimum and maximum width can be specified as integers or
nil. A nil value for minimum means no lower bound, and a
nil value for maximum means no upper bound.

The threshold value is an integer that will be set as the
`split-width-threshold' when the frame width is within the
specified range."
  :type '(repeat (list (choice
                        (const :tag "no min" nil) integer)
                  (choice
                   (const :tag "no max" nil) integer)
                  integer))
  :group 'km-window)

(defcustom km-window-split-threshold-delay 0.5
  "Delay in seconds before updating `split-width-threshold'.

Specifies the delay in seconds before updating the window split
threshold after a frame resize event, such as toggling fullscreen
mode.

This value determines how long to wait before applying the new
`split-width-threshold' to the frame, allowing the frame to
stabilize after resizing."
  :type 'number
  :group 'km-window)

(defcustom km-window-split-width-default nil
  "Fallback value to use for `split-width-threshold' when no range matches.
If nil, do not change `split-width-threshold'."
  :type '(choice (const :tag "leave unchanged" nil) integer)
  :group 'km-window)

(defun km-window--value-for-frame-width (width)
  "Return the threshold value for a given frame WIDTH from predefined ranges.

WIDTH is an integer representing the current frame width."
  (catch 'km-found
    (dolist (entry km-window-split-width-thresholds)
      (let ((min (nth 0 entry))
            (max (nth 1 entry))
            (val (nth 2 entry)))
        (let ((minb (if min min -1))
              (maxb (if max max most-positive-fixnum)))
          (when (and (<= minb width)
                     (<= width maxb))
            (throw 'km-found val)))))
    nil))

;;;###autoload
(defun km-window-apply-split-width-to-current-frame ()
  "Apply the `split-width-threshold' based on the current frame's width.

May be added to `after-init-hook'."
  (km-window--apply-split-width-threshold-to-frame (selected-frame)))

(defun km-window--apply-split-width-threshold-to-frame (frame)
  "Set `split-width-threshold' according to FRAME's current width."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (let* ((frame-w (frame-width frame))
             (val (km-window--value-for-frame-width frame-w)))
        (when val
          (setq split-width-threshold val))))))

(defvar km-window--pending-split-threshold-timer nil
  "Timer object for a pending `split-width-threshold' update, or nil.")

(defun km-window-update-split-width-threshold-delayed (&rest args)
  "Advice for `toggle-frame-fullscreen'.  Schedule an update after frame resize.
ARGS may contain the FRAME argument passed to `toggle-frame-fullscreen'."
  (when (and km-window--pending-split-threshold-timer
             (timerp km-window--pending-split-threshold-timer))
    (cancel-timer km-window--pending-split-threshold-timer)
    (setq km-window--pending-split-threshold-timer nil))
  (let ((f (or (car args)
               (selected-frame))))
    (setq km-window--pending-split-threshold-timer
          (run-with-idle-timer
           km-window-split-threshold-delay nil
           #'km-window--apply-split-width-threshold-to-frame
           f))))


;;;###autoload
(define-minor-mode km-window-auto-split-mode
  "Global minor mode to adjust `split-width-threshold' after fullscreen toggles.

When enabled, an :after advice is added to `toggle-frame-fullscreen' that
schedules an update (after `km-window-split-threshold-delay' seconds) so the
frame width reflects the new fullscreen state."
  :global t
  :group 'km-window
  :lighter " KM-Split"
  (if km-window-auto-split-mode
      (unless (advice-member-p #'km-window-update-split-width-threshold-delayed
                               'toggle-frame-fullscreen)
        (advice-add 'toggle-frame-fullscreen :after
                    #'km-window-update-split-width-threshold-delayed))
    (when (advice-member-p #'km-window-update-split-width-threshold-delayed
                           'toggle-frame-fullscreen)
      (advice-remove 'toggle-frame-fullscreen
                     #'km-window-update-split-width-threshold-delayed))))


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

(defun km-window--transient-setup ()
  "Set up transient for the current command if it exists."
  (when transient-current-command
    (transient-setup transient-current-command)))

;;;###autoload
(defun km-window-suffix-enlarge-horizontally ()
  "Make selected window 1 columns wider."
  (interactive)
  (enlarge-window-horizontally 1)
  (km-window--transient-setup))


;;;###autoload
(defun km-window-suffix-shrink-horizontally ()
  "Make selected window 1 columns narrower."
  (interactive)
  (shrink-window-horizontally 1)
  (km-window--transient-setup))

;;;###autoload
(defun km-window-suffix-enlarge-vertically ()
  "Enlarge current window by one line vertically."
  (interactive)
  (enlarge-window 1 nil)
  (km-window--transient-setup))

;;;###autoload
(defun km-window-suffix-shrink-window-vertically ()
  "Shrink the current window by one line vertically."
  (interactive)
  (shrink-window 1 nil)
  (km-window--transient-setup))

(defun km-window--add-frame-alpha-param (prop value &optional min-value
                                              max-value)
  "Adjust frame transparency by adding VALUE to the current alpha PROP parameter.

Argument PROP is the frame parameter to modify.

Argument VALUE is the amount to add to the current parameter value."
  (let* ((frame (selected-frame))
         (current (frame-parameter frame prop))
         (incompatible-prop (if (eq prop 'alpha-background)
                                'alpha
                              (and (eq prop 'alpha)
                                   'alpha-background)))
         (next-val (max (+ (or current 100) value)
                        (or min-value 1))))
    (when (and incompatible-prop
               (frame-parameter frame incompatible-prop))
      (set-frame-parameter frame incompatible-prop nil))
    (set-frame-parameter frame prop
                         (if (> value 0)
                             (min (or max-value 100)
                                  next-val)
                           (max (or min-value 1)
                                next-val)))))

(defun km-window--add-frame-alpha-background (value)
  "Adjust frame's alpha background by VALUE, ensuring it stays within 0-100.

Argument VALUE is the amount by which to adjust the frame's alpha background
value."
  (km-window--add-frame-alpha-param 'alpha-background value
                                    1
                                    100))

(defun km-window--add-frame-alpha (value)
  "Adjust frame transparency by adding VALUE to its alpha parameter.

Argument VALUE is the amount to add to the current alpha parameter value."
  (km-window--add-frame-alpha-param 'alpha value
                                    frame-alpha-lower-limit))

(defun km-window--frame-inc-alpha-background ()
  "Increase frame's alpha background by 1, ensuring it stays within 0-100 range."
  (interactive)
  (km-window--add-frame-alpha-background 1))

(defun km-window--frame-dec-alpha-background ()
  "Decrease frame's alpha background by 1, ensuring it stays within 0-100."
  (interactive)
  (km-window--add-frame-alpha-background -1))

(defun km-window--frame-inc-alpha ()
  "Increase frame's alpha transparency by 1."
  (interactive)
  (km-window--add-frame-alpha 1))

(defun km-window--frame-dec-alpha ()
  "Decrease frame's alpha transparency by 1."
  (interactive)
  (km-window--add-frame-alpha -1))

;;;###autoload (autoload 'km-window-frame-menu "km-window" nil t)
(transient-define-prefix km-window-frame-menu ()
  "Adjust frame transparency with \"More\" or \"Less\" options."
  :transient-suffix  #'transient--do-call
  :transient-non-suffix #'transient--do-exit
  [:description (lambda ()
                  (let ((param (frame-parameter (selected-frame)
                                                'alpha-background)))
                    (format "Alpha background (%s)" param)))
   ("<up>" "More" km-window--frame-inc-alpha-background)
   ("<down>" "Less" km-window--frame-dec-alpha-background)]
  [:description (lambda ()
                  (let ((param (frame-parameter (selected-frame)
                                                'alpha)))
                    (format "Alpha (%s)" param)))
   ("<right>" "More" km-window--frame-inc-alpha)
   ("<left>" "Less" km-window--frame-dec-alpha)])

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
    km-window-curr-buffer-to-other-window-and-pop-prev-buffer)
   ("f" "Frame settings" km-window-frame-menu)]
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


;;;###autoload
(defun km-window-split-window-sensibly (&optional window)
  "Split WINDOW only if absolutely necessary.

Only split if there is no split, and only split into left or right
windows.

If no window is specified then WINDOW defaults to output of `selected-window'.

`split-width-threshold' is observed."
  (let ((window (or window (selected-window))))
    (if (and
         (window-splittable-p window t)
         (eq (length (window-list)) 1))
        (with-selected-window window
          (split-window-right))
      nil)))

(provide 'km-window)
;;; km-window.el ends here