;;; km-window-tests.el --- Tests for km-window -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'km-window)

(ert-deftest km-window-frame-fullscreen-or-maximized-p ()
  (dolist (state '(fullscreen fullboth maximized))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) state)))
      (should (km-window--frame-fullscreen-or-maximized-p 'frame))))
  (dolist (state '(nil fullwidth fullheight))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) state)))
      (should-not (km-window--frame-fullscreen-or-maximized-p 'frame)))))

(ert-deftest km-window-frame-movable-p ()
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
            ((symbol-function 'display-graphic-p) (lambda (_frame) t))
            ((symbol-function 'frame-parameter)
             (lambda (_frame _parameter) nil)))
    (should (km-window--frame-movable-p)))
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
            ((symbol-function 'display-graphic-p) (lambda (_frame) t))
            ((symbol-function 'frame-parameter)
             (lambda (_frame _parameter) 'maximized)))
    (should-not (km-window--frame-movable-p)))
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
            ((symbol-function 'display-graphic-p) (lambda (_frame) nil)))
    (should-not (km-window--frame-movable-p))))

(ert-deftest km-window-move-frame ()
  (let (new-position)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'km-window--frame-movable-p) (lambda () t))
              ((symbol-function 'frame-position) (lambda (_frame) '(50 . 75)))
              ((symbol-function 'set-frame-position)
               (lambda (_frame left top)
                 (setq new-position (cons left top))))
              ((symbol-function 'km-window--transient-setup) #'ignore))
      (km-window--move-frame -20 10)
      (should (equal new-position '(30 . 85))))))

(ert-deftest km-window-move-frame-rejects-inapt-frame ()
  (cl-letf (((symbol-function 'km-window--frame-movable-p) (lambda () nil)))
    (should-error (km-window--move-frame 20 0) :type 'user-error)))

(ert-deftest km-window-resize-frame ()
  (let (new-size)
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'km-window--frame-movable-p) (lambda () t))
              ((symbol-function 'frame-width) (lambda (_frame) 80))
              ((symbol-function 'frame-height) (lambda (_frame) 40))
              ((symbol-function 'set-frame-size)
               (lambda (_frame width height &optional _pixelwise)
                 (setq new-size (cons width height))))
              ((symbol-function 'km-window--transient-setup) #'ignore))
      (km-window--resize-frame 2 -3)
      (should (equal new-size '(82 . 37))))))

(ert-deftest km-window-resize-frame-rejects-inapt-frame ()
  (cl-letf (((symbol-function 'km-window--frame-movable-p) (lambda () nil)))
    (should-error (km-window--resize-frame 2 -3) :type 'user-error)))

(ert-deftest km-window-frame-descriptions ()
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
            ((symbol-function 'frame-position) (lambda (_frame) '(120 . 45)))
            ((symbol-function 'frame-monitor-attributes)
             (lambda (_frame) '((name . "Test Display"))))
            ((symbol-function 'frame-parameter)
             (lambda (_frame _parameter) nil))
            ((symbol-function 'frame-width) (lambda (_frame) 100))
            ((symbol-function 'frame-height) (lambda (_frame) 60))
            ((symbol-function 'frame-pixel-width) (lambda (_frame) 1000))
            ((symbol-function 'frame-pixel-height) (lambda (_frame) 720)))
    (should (equal (substring-no-properties
                    (km-window--frame-position-description))
                   "Move  @ 120, 45 px  normal  Test Display"))
    (should (equal (substring-no-properties
                    (km-window--frame-size-description))
                   "Resize  100 x 60 chars  1000 x 720 px"))))

(provide 'km-window-tests)
;;; km-window-tests.el ends here
