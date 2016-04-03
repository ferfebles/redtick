RedTick
=======

# What #

This package provides a little pomodoro timer inside the mode-line.

# How #

After requiring, it shows a little red tick (✓) in the mode-line. When
you click in it, it starts a pomodoro timer.

It only shows the timer in the selected window (a moving timer
replicated in each window is a little bit distracting!).

# Why #

I thought about this, after seeing the spinner.el package.

# Elisp #

I tried to make it efficient:
  - It uses an elisp timer to program the next modification of the
    pomodoro timer.
  - Only works when the mode-line is changed.

# Thanks to #

* Malabarba, for the spinner and smart-mode-line packages.

* abo-abo, for asking how to check if the current window is selected from
inside the mode-line, and Drew for answering it.

* purcell, for reviewing the code. 
