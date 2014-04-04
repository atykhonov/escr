# escr

### About

This tool allows to take a screen shot of the Emacs' frames, windows and regions
directly from the GNU Emacs.

### Usage

- Use `escr-frame-screenshot` command to take a screen shot of the current frame.

- Use `escr-window-screenshot` command to take a screen shot of the current window.

- Use `escr-region-screenshot` command to take a screen shot of the current region.

### Customization

Set `escr-exclude-fringes` to `t` if you want to exclude fringes from a screen
shot. This variable does work only for `escr-region-screenshot`.

Set `escr-screenshot-width` to nil if you want to take a screen shot of current
window width. And set to integer value if you want limit screen shot to the number of
maximum column. This variable does work only for `escr-region-screenshot`.

Set `escr-screenshot-directory` to any directory where you would like screen shots to
be stored. Default directory is "~/.emacs.d/screenshots/". If directory doesn't exist
you'll be prompted to create that directory.

Change `escr-screenshot-format` to the desired filename format. Default is
`%Y-%m-%d-%H%M%S.png` (Read `format-time-string` documentation for details).

Set `escr-screenshot-quality` to desired image quality. Default is 100 (maximum
image quality).

### Installation

Assuming that the file `escr.el` is somewhere on the load path, add the following
lines to your `.emacs` file:

```
(require 'escr)
(global-set-key (kbd "C-c C-e r") 'escr-region-screenshot)
(global-set-key (kbd "C-c C-e f") 'escr-frame-screenshot)
(global-set-key (kbd "C-c C-e w") 'escr-window-screenshot)
```

Change the key bindings to your liking.

### Contribution

Contribution is much welcome! When adding new features, please write tests for them!

Thank you! And Enjoy!
