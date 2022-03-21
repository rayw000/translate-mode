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

### Interactive Functions

You can customize keybindings by setting `translate-mode-map`. Currently these 

| Keystroke                    | Command                         | Description                         |
|------------------------------|---------------------------------|-------------------------------------|
| <kbd>C</kbd>-<kbd>n</kbd>    | `translate-next-line`           | Remapped from `next-line`           |
| <kbd>C</kbd>-<kbd>p</kbd>    | `translate-previous-line`       | Remapped from `previous-line`       |
| <kbd>C</kbd>-<kbd>v</kbd>    | `translate-scroll-up`           | Remapped from `scroll-up-command`   |
| <kbd>M</kbd>-<kbd>v</kbd>    | `translate-scroll-down`         | Remapped from `scroll-down-command` |
| <kbd>M</kbd>-<kbd>}</kbd>    | `translate-forward-paragraph`   | Remapped from `forward-paragraph`   |
| <kbd>M</kbd>-<kbd>{</kbd>    | `translate-backward-paragraph`  | Remapped from `backward-paragraph`  |
| <kbd>C</kbd>-<kbd>&lt;</kbd> | `translate-beginning-of-buffer` | Remapped from `beginning-of-buffer` |
| <kbd>C</kbd>-<kbd>&gt;</kbd> | `translate-end-of-buffer`       | Remapped from `end-of-buffer`       |
| <kbd>RET</kbd>               | `translate-newline`             | Remapped from `newline`             |
| <kbd>C</kbd>-<kbd>l</kbd>    | `translate-recenter`            | Remapped from `recenter-top-bottom`, will also do `translate-sync-cursor-to-current-paragraph` |

There are also interactive functions not bound. You can bind them to your favorite keystrokes.

| Function                                     | Description                                                                                                  |
|----------------------------------------------|--------------------------------------------------------------------------------------------------------------|
| `translate-toggle-highlight`                 | Toggle paragraph highlighting                                                                                |
| `translate-sync-cursor-to-current-paragraph` | Move cursor in original artical buffer to the same n-th paragraph as cursor in translation buffer            |
| `translate-open-original-file`               | Prompt to open a file and set it as the original buffer for referring                                        |
| `translate-select-original-buffer`           | Prompt to select an existing buffer as the original buffer for referring                                     |
| `translate-init`                             | Enable `translate-mode` in th current buffer, and prompt to open a file as the original buffer for referring |

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

| Variable                                 | Default               |
|------------------------------------------|-----------------------|
| `translate-previous-line-function`       | `previous-line`       |
| `translate-next-line-function`           | `next-line`           |
| `translate-scroll-up-function`           | `scroll-up-command`   |
| `translate-scroll-down-function`         | `scroll-down-command` |
| `translate-forward-paragraph-function`   | `forward-paragraph`   |
| `translate-backward-paragraph-function`  | `backward-paragraph`  |
| `translate-beginning-of-buffer-function` | `beginning-of-buffer` |
| `translate-end-of-buffer-function`       | `end-of-buffer`       |
| `translated-newline-function`            | `newline`             |
| `translate-recenter-function`            | `recenter`            |

