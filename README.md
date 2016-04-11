RedTick
=======

## What

This package provides a little pomodoro timer in the mode-line.

![Redtick](https://raw.githubusercontent.com/ferfebles/redtick/master/redtick.gif)

## How

* Install from melpa (http://melpa.org/#/getting-started)

* Use `(require 'redtick)`. After requiring, it shows a little red tick (✓) in the mode-line. When
you click on it, it starts a pomodoro timer. The pomodoro description is set using current-buffer and which-function.

* You can launch a pomodoro by M-x redtick, or M-x redtick-with-description (manual description).

It only shows the timer in the selected window (a moving timer
replicated in each window is a little bit distracting!).

## Why

I thought about this, after seeing the spinner.el package.

## Elisp

I tried to make it efficient:
  - It uses an elisp timer to program the next modification of the
    pomodoro timer.
  - Only works when the mode-line is changed.

## Thanks to

* Malabarba, for the spinner and smart-mode-line packages.
* abo-abo, for asking how to check if the current window is selected from
inside the mode-line, and Drew for answering it.
* purcell at melpa, for reviewing the code. 
* wellons at nullprogram.com, for the code used to save and restore lisp data.
