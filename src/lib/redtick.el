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
    (nil "✓" "SkyBlue2")))

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
  (redtick-propertize "✓" "SkyBlue2"))

;; setting as risky, so it's painted with colour
(put 'redtick-current-bar 'risky-local-variable t)

;; adding to mode-line
;;(add-to-list 'mode-line-misc-info
;;             '(:eval (if (redtick-selected-window-p) redtick-current-bar)))

(add-to-list 'mode-line-front-space
             '(" " (:eval (if (redtick-selected-window-p) redtick-current-bar))))

;; (setq sml/pre-modes-separator '(" "
;;                                (:eval (if (redtick-selected-window-p) redtick-current-bar))
;;                                " "))

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
