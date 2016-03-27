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
                                     (redtick-start)
                                     ))))

(defvar redtick-selwin (selected-window))

(defun redtick-set-selwin (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq redtick-selwin (selected-window))))

(add-function :before pre-redisplay-function #'redtick-set-selwin)


;; (setq redtick-timer nil)

(defvar redtick-current-bar
  (redtick-propertize "✓" "SkyBlue2"))
(put 'redtick-current-bar 'risky-local-variable t)
(add-to-list 'mode-line-misc-info
             '(:eval (if (eq redtick-selwin (get-buffer-window))
                         redtick-current-bar)))

(defun redtick-update-current-bar (redtick-current-bars)
  (progn
    (setq redtick-current-bar (apply #'redtick-propertize
                                     (cdar redtick-current-bars)))
    (if (caar redtick-current-bars)
        (run-at-time (caar redtick-current-bars)
                     nil
                     #'redtick-update-current-bar
                     (cdr redtick-current-bars))
      )
    (force-mode-line-update t)
    (message redtick-current-bar)
    )
  )

(defun redtick-start ()
  (interactive)
  (progn
    (setq redtick-started-at (float-time))
    (redtick-update-current-bar redtick-bars)
    ;; (add-to-list 'mode-line-misc-info
    ;;              '(redtick-current-bar redtick-current-bar))
    ;; (redtick-set-selected-window)
    (force-mode-line-update t))
  )

;; (defun redtick-redraw-mode-line ()
;;   (progn
;;     (force-mode-line-update)
;;     (run-at-time 1 nil 'redtick-redraw-mode-line)))

;; (defun redtick-set-selected-window ()
;;   "sets the variable `redtick-selected-window` appropriately"
;;   (when (not (minibuffer-window-active-p (frame-selected-window)))
;;     (setq redtick-selected-window (frame-selected-window))))

;; (add-hook 'window-configuration-change-hook 'redtick-set-selected-window)
;; (add-hook 'focus-in-hook 'redtick-set-selected-window)
;; (add-hook 'focus-out-hook 'redtick-set-selected-window)

;; (defadvice select-window (after redtick-select-window activate)
;;   "makes redtick aware of window changes"
;;   (redtick-set-selected-window))

;; (defun redtick-current-window-active-p ()
;;   "Return whether the current window is active."
;;   (eq redtick-selected-window (selected-window)))

;; (defun redtick-bar-in-selected-window ()
;;   (if (redtick-current-window-active-p)
;;       redtick-current-bar))

