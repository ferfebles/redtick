;;; redtick.el --- Smallest emacs pomodoro timer (1 char)

;; Author: F. Febles
;; URL: http://github.com/ferfebles/redtick
;; Version: 00.01.01
;; Package-Requires: ((names "20151201.0") (emacs "24"))
;; Keywords: pomodoro, timer

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; This package provides a little pomodoro timer inside the mode-line.
;;
;; After importing, it shows a little red tick (✓) in the mode-line. When
;; you click in it, it starts a pomodoro timer.
;;
;; It only shows the timer in the selected window (a moving timer
;; replicated in each window is a little bit distracting!).
;;
;; I thought about this, after seeing the spinner.el package.
;;
;; Despite my limited knowledge of elisp and emacs, I tried to make it
;; as efficient as it can be:
;;   - It uses an elisp timer to program the next modification of the
;;     pomodoro timer.
;;   - Only works when the mode-line is changed.

;;; Code:

;; pomodoro work & break intervals in seconds
(defvar redtick-work-interval (* 6 25))
(defvar redtick-break-interval (* 6 5))

;; redtick bars for every interval
(defvar redtick-workbars-interval (/ redtick-work-interval 8.0))
(defvar redtick-breakbars-interval (/ redtick-break-interval 8.0))

;; bar & colours http://www.utexas.edu/learn/html/colors.html
(defvar redtick-bars
  '((4 "█" "#ffff66") ;; 4 -> redtick-workbars-interval
    (4 "▇" "#ffcc66")
    (4 "▆" "#cc9966")
    (4 "▅" "#ff9966")
    (4 "▄" "#cc6666")
    (4 "▃" "#ff6666")
    (4 "▂" "#ff3366")
    (4 "▁" "#ff0066")
    (1 "█" "#00cc66")  ;; 1 -> redtick-breakbars-interval
    (1 "▇" "#33cc66")
    (1 "▆" "#66cc66")
    (1 "▅" "#00ff66")
    (1 "▄" "#33ff66")
    (1 "▃" "#66ff66")
    (1 "▂" "#99ff66")
    (1 "▁" "#ccff66")
    (nil "✓" "#cf6a4c")))

;; pomodoro start time
(defvar redtick-started-at (float-time))

;; seconds since pomodoro started
(defun redtick-seconds-since-started ()
  (truncate (- (float-time) redtick-started-at)))

;; seconds or minutes since started
(defun redtick-popup-message ()
  (let ( (minutes (truncate (redtick-seconds-since-started) 60)))
    (concat (cond
             ((= 0 minutes) (format "%s seconds"
                                    (redtick-seconds-since-started)))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed\nclick to restart")))

;; set colours, help echo, and click action
(defun redtick-propertize (bar bar-color)
  (propertize bar
              'face `(:inherit mode-line :foreground ,bar-color)
              'help-echo '(redtick-popup-message)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'redtick-start)))

;; getting selected window from mode-line
(defvar redtick-selected-window (selected-window))

(defun redtick-set-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq redtick-selected-window (selected-window))))

(add-function :before pre-redisplay-function #'redtick-set-selected-window)

(defun redtick-selected-window-p ()
  (eq redtick-selected-window (get-buffer-window)))

;; initializing current bar
(defvar redtick-current-bar
  (redtick-propertize "✓" "#cf6a4c"))

;; setting as risky, so it's painted with colour
(put 'redtick-current-bar 'risky-local-variable t)

;; adding to mode-line
(add-to-list 'mode-line-misc-info
             '(:eval (if (redtick-selected-window-p) redtick-current-bar)))

;; updates current bar, and programs next update.
(defun redtick-update-current-bar (redtick-current-bars)
  (progn
    (setq redtick-current-bar (apply #'redtick-propertize
                                     (cdar redtick-current-bars)))
    (if (caar redtick-current-bars)
        (run-at-time (caar redtick-current-bars)
                     nil
                     #'redtick-update-current-bar
                     (cdr redtick-current-bars)))
    (force-mode-line-update t)))

(defun redtick-start ()
  (interactive)
  (progn
    (setq redtick-started-at (float-time))
    (redtick-update-current-bar redtick-bars)))

(provide 'redtick)
;;; redtick.el ends here
