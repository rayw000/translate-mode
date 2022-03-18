Translate Mode
==============================
A paragraph-based Emacs minor mode for doing article translation jobs. Your original article buffer will be scrolled automatically along with your translation buffer scrolling. Never get lost!

## Screenshot

## Installation

If you don't have ELPA package in your Emacs, clone this repository and load file.
```shell
git clone https://github.com/rayw000/translate-mode.git
```

```emacs-lisp
(load-file "/path/to/translate-mode.el")
(require 'translate-mode)
```

## Quick start

* Open the translating file you are working with, and type `M-x translate-init RET` in your minibuffer.
* You can also enable `translate-mode` in your working buffer first, and then
  * open the original article file you are referring by type `M-x translate-open-original article-file RET`,
  * or, select an existing buffer as your original article buffer by type `M-x translate-select-original-buffer RET`.

## Customization
