;;; redtick.el --- Smallest pomodoro timer (1 char)

;; Author: F. Febles
;; URL: http://github.com/ferfebles/redtick
;; Version: 00.01.01
;; Package-Requires: ((emacs "24"))
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

;; This package provides a little pomodoro timer in the mode-line.
;;
;; After importing, it shows a little red tick (✓) in the mode-line.
;; When you click on it, it starts a pomodoro timer.
;;
;; It only shows the timer in the selected window (a moving timer
;; replicated in each window is a little bit distracting!).
;;
;; I thought about this, after seeing the spinner.el package.
;;
;; I tried to make it efficient:
;;   - It uses an elisp timer to program the next modification of the
;;     mode line: no polling, no sleeps...
;;   - Only works when the mode-line is changed.

;;; Code:

(defgroup redtick nil
  "Little pomodoro timer in the mode-line."
  :group 'tools
  :prefix "redtick-")

;; pomodoro work & rest intervals in seconds
(defcustom redtick-work-interval (* 60 25)
  "Interval of time you will be working, in seconds."
  :type 'number)
(defcustom redtick-rest-interval (* 60 5)
  "Interval of time you will be resting, in seconds."
  :type 'number)
;; pomodoro history file
(defcustom redtick-history-file "~/redtick-history.txt"
  "File to store all the completed pomodoros."
  :type 'string)

(require 'which-func)

;; stores the information for completed pomodoros
(defvar redtick-completed-pomodoros ())

;; stores redtick timer, to be cancelled if restarted
(defvar redtick--timer nil)

;; pomodoro start time
(defvar redtick--pomodoro-started-at (current-time))

;; current pomodoro description
(defvar redtick--pomodoro-description "Start your first pomodoro now!!!")

;; redtick intervals for every bar
(defvar redtick--workbar-interval (/ redtick-work-interval 8.0))
(defvar redtick--restbar-interval (/ redtick-rest-interval 8.0))

;; intervals, bars & colours
(defvar redtick--bars
  `((,redtick--workbar-interval "█" "#ffff66")
    (,redtick--workbar-interval "▇" "#ffcc66")
    (,redtick--workbar-interval "▆" "#cc9966")
    (,redtick--workbar-interval "▅" "#ff9966")
    (,redtick--workbar-interval "▄" "#cc6666")
    (,redtick--workbar-interval "▃" "#ff6666")
    (,redtick--workbar-interval "▂" "#ff3366")
    (,redtick--workbar-interval "▁" "#ff0066")
    (,redtick--restbar-interval "█" "#00cc66")
    (,redtick--restbar-interval "▇" "#33cc66")
    (,redtick--restbar-interval "▆" "#66cc66")
    (,redtick--restbar-interval "▅" "#00ff66")
    (,redtick--restbar-interval "▄" "#33ff66")
    (,redtick--restbar-interval "▃" "#66ff66")
    (,redtick--restbar-interval "▂" "#99ff66")
    (,redtick--restbar-interval "▁" "#ccff66")
    (nil "✓" "#cf6a4c")))

(defun redtick--seconds-since (time)
  "Seconds since TIME."
  (truncate (- (float-time (current-time)) (float-time time))))

(defun redtick--format-completed-pomodoro (start-time description)
  "Formats pomodoro START-TIME and DESCRIPTION."
  (format "%s, %-s" (format-time-string "%F %T" start-time) description))

(defun redtick--popup-message (time)
  "String with pomodoro popup message: TIME since start and instructions."
  (let* ((seconds (redtick--seconds-since time))
         (minutes (truncate seconds 60)))
    (concat (redtick--format-completed-pomodoro time
                                                redtick--pomodoro-description)
            "\n"
            (cond
             ((= 0 minutes) (format "%s seconds" seconds))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed, click to (re)start")))

(defun redtick--propertize (bar bar-color)
  "Propertize BAR with BAR-COLOR, help echo, and click action."
  (propertize bar
              'face `(:inherit mode-line :foreground ,bar-color)
              'help-echo '(redtick--popup-message redtick--pomodoro-started-at)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'redtick)))

;; initializing current bar
(defvar redtick--current-bar (redtick--propertize "✓" "#cf6a4c"))
;; setting as risky, so it's painted with colour
(put 'redtick--current-bar 'risky-local-variable t)

;; storing selected window to use from mode-line
(defvar redtick--selected-window (selected-window))

;; function that updates selected window variable
(defun redtick--update-selected-window (windows)
  "WINDOWS parameter avoids error when called before 'pre-redisplay-function'."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq redtick--selected-window (selected-window))))

(add-function :before pre-redisplay-function #'redtick--update-selected-window)

(defun redtick--selected-window-p ()
  "Check if current window is the selected one."
  (eq redtick--selected-window (get-buffer-window)))

;; adding to mode-line
(add-to-list 'mode-line-misc-info
             '(:eval (if (and redtick-mode (redtick--selected-window-p))
                         redtick--current-bar))
             t)

(defun redtick--save (file data)
  "Use FILE to save DATA (wellons at nullprogram.com)."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun redtick--load (file)
  "Use FILE to load DATA ((wellons at nullprogram.com)."
  (ignore-errors (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer)))))

(defun redtick--update-current-bar (redtick--current-bars)
  "Update current bar, and program next update using REDTICK--CURRENT-BARS."
  (setq redtick--current-bar (apply #'redtick--propertize
                                    (cdar redtick--current-bars)))
  (if (caar redtick--current-bars)
      (setq redtick--timer
            (run-at-time (caar redtick--current-bars)
                         nil
                         #'redtick--update-current-bar
                         (cdr redtick--current-bars)))
    (let ((history (redtick--load redtick-history-file)))
      (add-to-list 'history
                   (redtick--format-completed-pomodoro redtick--pomodoro-started-at
                                                       redtick--pomodoro-description)
                   t)
      (redtick--save redtick-history-file history)))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode redtick-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun redtick--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (format "Working with '%s'" (current-buffer))
          (cond ((which-function)
                 (format ":'%s'" (which-function))))))

;;;###autoload
(defun redtick ()
  "Enable minor-mode, and start the pomodoro."
  (interactive)
  (redtick-with-description (redtick--default-desc)))

;;;###autoload
(defun redtick-with-description (description)
  "Ask for DESCRIPTION, enable minor-mode, and start the pomodoro."
  (interactive (list (read-string (format "Description (%s): "
                                          (redtick--default-desc))
                                  nil nil (redtick--default-desc))))
  (redtick-mode t)
  (if redtick--timer (cancel-timer redtick--timer))
  (setq redtick--pomodoro-started-at (current-time)
        redtick--pomodoro-description description)
  (redtick--update-current-bar redtick--bars))

(provide 'redtick)
;;; redtick.el ends here
