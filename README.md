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

## Quick start

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

### Variables

#### `translate-enable-highlight`

Enable highlighting if set to non-nil.

#### `translate-original-buffer-read-only`

The original buffer will be read-only if set to non-nil.

### Face

### `translate-paragraph-highlight`
The paragraph highlighting face.

### Cursor moving functions

You can custom the cursor moving functions to make `translate-mode` work better with many major modes. For example in `markdown-mode`, moving across paragraphs could be achieved by

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

### Key maps

You can custom `translate-mode-map` to setup your own keybindings.
