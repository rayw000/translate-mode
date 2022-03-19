Translate Mode
==============================
A paragraph-oriented Emacs minor mode for doing article translation jobs. Your original article buffer will be scrolled automatically along with your translation buffer scrolling. Never get lost!

## Screenshot

![screen record](./screen-record.gif)

## Installation

You can install `translate-mode` from MELPA by

```emacs-lisp
M-x package-install RET translate-mode RET
```

If you don't have ELPA package in your Emacs, clone this repository and load file.
```shell
git clone https://github.com/rayw000/translate-mode.git
```

```emacs-lisp
(load-file "/path/to/translate-mode.el")
(require 'translate-mode)
```

## Basic Usage

* Open the translating file you are working with, and run command
```emacs-lisp
(translate-init)
``` 

* You can also enable `translate-mode` in your working buffer first, and then use command
```emacs-lisp
(translate-open-original-file)
```
or
```emacs-lisp
(translate-select-original-buffer)
```
to setup a buffer for referring the original article.

## Customization

### Interac

You can customize keybindings by setting `translate-mode-map`. Currently these 

| Keystroke      | Command                   | Description                                                                |
|----------------|---------------------------|----------------------------------------------------------------------------|
| <kbd>C-n</kbd> | `translate-next-line`     | Move cursor to next ARG lines like `next-line`. ARG defaults to 1.         |
| <kbd>C-p</kbd> | `translate-previous-line` | Move cursor to previous ARG lines like `previous-line`. ARG defaults to 1. |

### Variables

| Name                                  | Default Value | Description                                |
|---------------------------------------|---------------|--------------------------------------------|
| `translate-enable-highlight`          | `t`           | Enable highlighting if non-nil.            |
| `translate-original-buffer-read-only` | `t`           | Make original buffer read-only if non-nil. |

### Face

```emacs-lisp
(defface translate-paragraph-highlight
  '((t :background "grey15" :extend t))
  "Default face for highlighting the current paragraph in `translate-mode'."
  :group 'translate)
```

### Cursor Moving Functions

You can customize the cursor moving functions to make `translate-mode` work better with many major modes. For example in `markdown-mode`, moving across paragraphs could be achieved by

```emacs-lisp
(setq translate-forward-paragraph-function 'markdown-forward-paragraph)
(setq translate-backward-paragraph-function 'markdown-backward-paragraph)
```

Here are available cusor moving function variables.

* `translate-previous-line-function`
* `translate-next-line-function`
* `translate-scroll-up-function`
* `translate-scroll-down-function`
* `translate-forward-paragraph-function`
* `translate-backward-paragraph-function`
* `translate-beginning-of-buffer-function`
* `translate-end-of-buffer-function`
* `translated-newline-function`
* `translate-recenter-function`
