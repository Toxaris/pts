#PTS

[![Build Status](https://travis-ci.org/Toxaris/pts.svg?branch=master)](https://travis-ci.org/Toxaris/pts)

## Description

A Haskell implementation of the [Pure Type
System](http://ncatlab.org/nlab/show/pure+type+system)

##Installation

Run the following commands at the top:

```
cabal install
```

To proactively avoid dependency hell, consider using
[cabal-dev](https://github.com/creswick/cabal-dev) or some other
tool to sandbox builds.

## Emacs mode

We bundle a very simple emacs mode, see
[emacs/pts-mode.el](emacs/pts-mode.el). To see where the emacs
mode has been installed to, run:

```
pts --locate-emacs-mode
```

To install the emacs mode, you can add something like this to
your `.emacs`:

```
(let ((pts-mode-path (shell-command-to-string "pts --locate-emacs-mode")))
  (when (file-exists-p pts-mode-path)
    (add-to-list 'load-path pts-mode-path)
    (require 'pts-mode)))
```

This assumes that `pts` is on your path and calls it to find the
emacs mode. Keybindings in pts-mode:

 - <kbd>C-c</kbd> <kbd>C-l</kbd> check types and assertions in current buffer

## Contributing

I am happy to receive pull requests. Note that I assume that I
can publish the code in pull requests under a three-clause BSD
license.
