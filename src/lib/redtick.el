;; pomodoro work & break intervals
(defvar redtick-work-interval (* 60 25))
(defvar redtick-break-interval (* 60 5))
(defun redtick-workbar-interval (i)
  (/ (* i redtick-work-interval) 8))
(defun redtick-breakbar-interval (i)
  (+ redtick-work-interval (/ (* i redtick-break-interval) 8)))

;; pomodoro start time
(defvar redtick-started-at (float-time))

;; seconds since pomodoro started
(defun redtick-seconds-since-started ()
  (truncate (- (float-time) redtick-started-at)))

(defun redtick-popup-message ()
  (let ((seconds (redtick-seconds-since-started))
        (minutes (truncate (redtick-seconds-since-started) 60)))
    (concat (cond
             ((= 0 minutes) (format "%s seconds" seconds))
             ((= 1 minutes) (format "%s minute" minutes))
             (t (format "%s minutes" minutes)))
            " elapsed\nclick to restart")))

;; variable for the timer object
(defvar redtick-timer nil)

(defun redtick-propertize (bar bar-color)
  (propertize bar
              'face `(:inherit mode-line :foreground ,bar-color)
              'help-echo '(redtick-popup-message)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map
                          'mouse-1 (lambda () (interactive)
                                     (progn (redtick-stop) (redtick-start) )
                                     ))))()

;; (setq old-mode-line-format mode-line-format)
;; (add-to-list 'mode-line-format
;;              '(:eval (redtick-bar (redtick-seconds-since-started))) t)

;; (setq redtick-started-at (float-time))
;; (force-mode-line-update)

;; (setq mode-line-format old-mode-line-format)

;; (redtick-bar (redtick-seconds-since-started))
;; (delete '(:eval (redtick-bar (redtick-seconds-since-started))) mode-line-format)



;; draws bar http://www.utexas.edu/learn/html/colors.html
(defun redtick-bar (seconds)
  (cond
   ((< seconds (redtick-workbar-interval 1)) (redtick-propertize "█" "#ffff66"))
   ((< seconds (redtick-workbar-interval 2)) (redtick-propertize "▇" "#ffcc66"))
   ((< seconds (redtick-workbar-interval 3)) (redtick-propertize "▆" "#cc9966"))
   ((< seconds (redtick-workbar-interval 4)) (redtick-propertize "▅" "#ff9966"))
   ((< seconds (redtick-workbar-interval 5)) (redtick-propertize "▄" "#cc6666"))
   ((< seconds (redtick-workbar-interval 6)) (redtick-propertize "▃" "#ff6666"))
   ((< seconds (redtick-workbar-interval 7)) (redtick-propertize "▂" "#ff3366"))
   ((< seconds (redtick-workbar-interval 8)) (redtick-propertize "▁" "#ff0066"))
   ((< seconds (redtick-breakbar-interval 1)) (redtick-propertize "█" "#00cc66"))
   ((< seconds (redtick-breakbar-interval 2)) (redtick-propertize "▇" "#33cc66"))
   ((< seconds (redtick-breakbar-interval 3)) (redtick-propertize "▆" "#66cc66"))
   ((< seconds (redtick-breakbar-interval 4)) (redtick-propertize "▅" "#00ff66"))
   ((< seconds (redtick-breakbar-interval 5)) (redtick-propertize "▄" "#33ff66"))
   ((< seconds (redtick-breakbar-interval 6)) (redtick-propertize "▃" "#66ff66"))
   ((< seconds (redtick-breakbar-interval 7)) (redtick-propertize "▂" "#99ff66"))
   ((< seconds (redtick-breakbar-interval 8)) (redtick-propertize "▁" "#ccff66"))
   (t (progn
        (cancel-timer redtick-timer)
        (redtick-propertize "∞" "SkyBlue2")))))

;; Callback function 
(defun redtick-callback ()
  (setq global-mode-string
        (redtick-bar (redtick-seconds-since-started)))
  (force-mode-line-update t))

(defun redtick-start ()
  (interactive)
  (setq redtick-started-at (float-time))
  (when (timerp redtick-timer)
    (cancel-timer redtick-timer))
  (setq redtick-timer
          (run-with-timer 0 1 #'redtick-callback)))

;; stop function
(defun redtick-stop ()
  (interactive)
  (when (timerp redtick-timer)
    (cancel-timer redtick-timer))
  (setq redtick-timer nil)
  (setq global-mode-string nil))




