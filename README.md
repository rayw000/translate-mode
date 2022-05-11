[![MELPA](https://melpa.org/packages/translate-mode-badge.svg)](https://melpa.org/#/translate-mode)
[![MELPA Stable](https://stable.melpa.org/packages/translate-mode-badge.svg)](https://stable.melpa.org/#/translate-mode)

Translate Mode
==============================
Paragraph-oriented minor mode for side-by-side document translation workflow. Your reference buffer will be scrolled automatically along with your translation buffer scrolling. Never get lost!

## Screen Recording

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

## Usage

Open the translation file you are working with, and run command
```emacs-lisp
(translate-select-reference-buffer)
``` 
to select an existed reference buffer or
```emacs-lisp
(translate-open-reference-file)
```
to open a reference file.

## Customization

### Interactive Functions

You can customize keybindings by setting `translate-mode-map`. Here are the default keybindings.

| Keystroke                    | Command                         | Description                                                                                    |
|------------------------------|---------------------------------|------------------------------------------------------------------------------------------------|
| <kbd>C</kbd>-<kbd>n</kbd>    | `translate-next-line`           | Remapped from `next-line`                                                                      |
| <kbd>C</kbd>-<kbd>p</kbd>    | `translate-previous-line`       | Remapped from `previous-line`                                                                  |
| <kbd>C</kbd>-<kbd>v</kbd>    | `translate-scroll-up`           | Remapped from `scroll-up-command`                                                              |
| <kbd>M</kbd>-<kbd>v</kbd>    | `translate-scroll-down`         | Remapped from `scroll-down-command`                                                            |
| <kbd>M</kbd>-<kbd>}</kbd>    | `translate-forward-paragraph`   | Remapped from `forward-paragraph`                                                              |
| <kbd>M</kbd>-<kbd>{</kbd>    | `translate-backward-paragraph`  | Remapped from `backward-paragraph`                                                             |
| <kbd>C</kbd>-<kbd>&lt;</kbd> | `translate-beginning-of-buffer` | Remapped from `beginning-of-buffer`                                                            |
| <kbd>C</kbd>-<kbd>&gt;</kbd> | `translate-end-of-buffer`       | Remapped from `end-of-buffer`                                                                  |
| <kbd>RET</kbd>               | `translate-newline`             | Remapped from `newline`                                                                        |
| <kbd>C</kbd>-<kbd>l</kbd>    | `translate-recenter-top-bottom` | Remapped from `recenter-top-bottom`, will also do `translate-sync-cursor-to-current-paragraph` |

There are also interactive functions not bound. You can bind them to your favorite keystrokes.

| Function                                     | Description                                                                                                |
|----------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `translate-toggle-highlight`                 | Toggle paragraph highlighting                                                                              |
| `translate-sync-cursor-to-current-paragraph` | Move the cursor in the reference buffer to the same n-th paragraph as the cursor in the translation buffer |
| `translate-open-reference-file`              | Prompt to open a file and set it as the reference buffer                                                   |
| `translate-select-reference-buffer`          | Prompt to select an existing buffer as the reference buffer                                                |

### Variables

| Name                                   | Default Value | Description                                 |
|----------------------------------------|---------------|---------------------------------------------|
| `translate-enable-highlight`           | `t`           | Enable highlighting if non-nil.             |
| `translate-reference-buffer-read-only` | `t`           | Make reference buffer read-only if non-nil. |

### Face

```emacs-lisp
(defface translate-paragraph-highlight-face
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
| `translate-recenter-top-bottom-function` | `recenter-top-bottom` |
