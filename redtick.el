;;; redtick.el --- Smallest pomodoro timer (1 char)

;; Author: F. Febles
;; URL: http://github.com/ferfebles/redtick
;; Version: 00.01.03
;; Package-Requires: ((emacs "24.4"))
;; Keywords: calendar

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
;;   - It uses SOX player, that supports looping wav files without gaps.
;;     Thanks to the loop, I only launch a player process when starting
;;     the work or rest interval.
;;

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
(defcustom redtick-history-file "~/redtick-history.txt"
  "File to store all the completed pomodoros."
  :type 'string)
(defcustom redtick-popup-header '(format "Working with '%s'" (current-buffer))
  "Header used in popup."
  :type 'sexp)
(defcustom redtick-play-sound nil
  "Play sounds when true."
  :type 'boolean)
(defcustom redtick-sound-volume "0.3"
  "Sound volume as numeric string (low < 1.0 < high)."
  :type 'string)
(defcustom redtick-sox-buffer nil
  "Name of the buffer used for SOX output (p.e. '*sox-debug*')."
  :type 'string)
(defcustom redtick-work-sound
  (expand-file-name "./resources/work.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Sound file to loop during the work period."
  :type 'string)
(defcustom redtick-rest-sound
  (expand-file-name "./resources/rest.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Sound file to loop during the rest period."
  :type 'string)
(defcustom redtick-end-rest-sound
  (expand-file-name "./resources/end-rest.mp3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Sound file to play at the end of the rest period."
  :type 'string)

(require 'which-func)

;; stores redtick timer, to be cancelled if restarted
(defvar redtick--timer nil)

;; stores the number of completed pomodoros
(defvar redtick--completed-pomodoros 0)

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

(defun redtick--ended-work-interval-p (redtick--current-bars)
  "Return t when ended work interval based on REDTICK--CURRENT-BARS."
  (equal `(,redtick--restbar-interval "█")
       (butlast (car redtick--current-bars))))

;; variable that stores the sound process object
(defvar redtick--sound-process nil)

(defun redtick--play-sound (file &optional args)
  "Play FILE using sox with optional ARGS."
  (if redtick-play-sound
      (if (executable-find "sox")
          (setq redtick--sound-process
                (apply 'start-process "sox" redtick-sox-buffer
                       "sox" file "-d" "vol" redtick-sound-volume args))
        (warn "SoX executable not found"))))

(defun redtick--stop-sound ()
  "Stops sound if playing."
  (if redtick--sound-process
      (delete-process redtick--sound-process)))

(defun redtick--play-sound-during (file seconds)
  "Play FILE during SECONDS, repeating or cutting if needed."
  (let ((fade (if (< seconds 8) "0" "4")))
       (redtick--play-sound file `("repeat" "-" "fade" "t" ,fade
                                   ,(number-to-string seconds)))))

(defun redtick--play-work-sound ()
  (redtick--stop-sound)
  (redtick--play-sound-during redtick-work-sound redtick-work-interval))

(add-hook 'redtick-before-work-hook #'redtick--play-work-sound)

(defun redtick--play-rest-sound ()
  (redtick--stop-sound)
  (redtick--play-sound-during redtick-rest-sound redtick-rest-interval))

(add-hook 'redtick-before-rest-hook #'redtick--play-rest-sound)

(defun redtick--play-end-of-rest-sound ()
  (redtick--stop-sound)
  (redtick--play-sound redtick-end-rest-sound))

(add-hook 'redtick-after-rest-hook #'redtick--play-end-of-rest-sound)

(defun redtick--seconds-since (time)
  "Seconds since TIME."
  (truncate (- (float-time (current-time)) (float-time time))))

(defun redtick--popup-message (time desc)
  "TIME since start, DESC(ription) and instructions."
  (let* ((seconds (redtick--seconds-since time))
         (minutes (truncate seconds 60)))
    (concat (format "%s completed pomodoro(s) in this session\n"
                    redtick--completed-pomodoros)
            (format "%s, %s\n" (format-time-string "%T" time) desc)
            (cond
             ((= 0 minutes) (format "%s seconds" seconds))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed, click to (re)start")))

(defun redtick--propertize (bar bar-color)
  "Propertize BAR with BAR-COLOR, help echo, and click action."
  (propertize bar
              'face `(:inherit mode-line :foreground ,bar-color)
              'help-echo '(redtick--popup-message redtick--pomodoro-started-at
                                                  redtick--pomodoro-description)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map 'mouse-1 'redtick)))

;; initializing current bar
(defvar redtick--current-bar (apply #'redtick--propertize
                                    (cdar (last redtick--bars))))
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
  "Use FILE to save DATA."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun redtick--load (file)
  "Use FILE to load DATA."
  (ignore-errors (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer)))))

(defun redtick--save-history ()
  "Adding current-pomodoro info to history file."
  (when redtick-history-file
    (let ((history (redtick--load redtick-history-file)))
      (redtick--save redtick-history-file
                     (add-to-list 'history
                                  (list redtick--pomodoro-started-at
                                        redtick-work-interval
                                        redtick-rest-interval
                                        redtick--pomodoro-description)
                                  t)))))

(add-hook 'redtick-after-rest-hook #'redtick--save-history)

(defun redtick--update-current-bar (redtick--current-bars)
  "Update current bar, and program next update using REDTICK--CURRENT-BARS."
  (setq redtick--current-bar (apply #'redtick--propertize
                                    (cdar redtick--current-bars)))
  (when (redtick--ended-work-interval-p redtick--current-bars)
    (run-hooks 'redtick-after-work-hook
               'redtick-before-rest-hook))
  (if (caar redtick--current-bars)
      (setq redtick--timer
            (run-at-time (caar redtick--current-bars)
                         nil
                         #'redtick--update-current-bar
                         (cdr redtick--current-bars)))
    (run-hooks 'redtick-after-rest-hook)
    (setq redtick--completed-pomodoros
          (1+ redtick--completed-pomodoros)))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode redtick-mode
  "Little pomodoro timer in the mode-line."
  :global t)

(defun redtick--default-desc ()
  "Default pomodoro description: Working with 'current-buffer'..."
  (concat (eval redtick-popup-header)
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
  (run-hooks 'redtick-before-work-hook)
  (setq redtick--pomodoro-started-at (current-time)
        redtick--pomodoro-description description)
  (redtick--update-current-bar redtick--bars))

(provide 'redtick)
;;; redtick.el ends here
