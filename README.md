# rcl-mode

(Incomplete) Emacs mode for [RCL Configuration Language](https://github.com/ruuda/rcl)

Currently provides syntax highlighting and indentation.

## Install

From master:

```elisp
(use-package rcl-mode
  :straight (rcl-mode :type git :host github :repo "qezz/rcl-mode"))
```

Specific version:

```elisp
(use-package rcl-mode
  :straight (rcl-mode :type git :host github :repo "qezz/rcl-mode" :branch "v0.1.0"))
```
