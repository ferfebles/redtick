;; pomodoro work & break intervals
(defvar redtick-work-interval (* 6 25))
(defvar redtick-break-interval (* 6 5))
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
  (let ( (minutes (truncate (redtick-seconds-since-started) 60)))
    (concat (cond
             ((= 0 minutes) (format "%s seconds"
                                    (redtick-seconds-since-started)))
             ((= 1 minutes) "1 minute")
             (t (format "%s minutes" minutes)))
            " elapsed\nclick to restart")))

(defun redtick-propertize (bar bar-color)
  (propertize bar
              'face `(:inherit mode-line :foreground ,bar-color)
              'help-echo '(redtick-popup-message)
              'pointer 'hand
              'local-map (make-mode-line-mouse-map
                          'mouse-1 (lambda () (interactive)
                                       (setq redtick-started-at (float-time))
                                     ))))

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
   (t (redtick-propertize "∞" "SkyBlue2"))))

(defun redtick-redraw-mode-line ()
  (progn
    (force-mode-line-update)
    (run-at-time 5 nil 'redtick-redraw-mode-line)))

(defun redtick-set-selected-window ()
  "sets the variable `redtick-selected-window` appropriately"
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq redtick-selected-window (frame-selected-window))))

(add-hook 'window-configuration-change-hook 'redtick-set-selected-window)
(add-hook 'focus-in-hook 'redtick-set-selected-window)
(add-hook 'focus-out-hook 'redtick-set-selected-window)

(defadvice select-window (after redtick-select-window activate)
  "makes redtick aware of window changes"
  (redtick-set-selected-window))

(defun redtick-selected-window-active ()
  "Return whether the current window is active."
  (eq redtick-selected-window (selected-window)))

(defun redtick-bar-in-selected-window ()
  (if (redtick-selected-window-active)
      (redtick-bar (redtick-seconds-since-started))))

(defun redtick-start ()
  (interactive)
  (progn
    (add-to-list 'mode-line-misc-info
                 '(:eval (redtick-bar-in-selected-window)))
    (redtick-set-selected-window)
    (setq redtick-started-at (float-time))
    (force-mode-line-update t))
  )
