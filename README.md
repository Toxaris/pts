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

## Known uses

This repository contains some example programs in the
[examples](examples) folder.

Known public developments using this pts interpreter:

 * [Toxaris/lift.pts](https://github.com/Toxaris/lift.pts)
   (Constructive lift monad in System Fω)

If you use the pts interpreter and want your use to be mentioned
here, sent a pull request for [README.md](README.md) or contact
the authors.

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
  (if (file-exists-p pts-mode-path)
      (progn
	(add-to-list 'load-path pts-mode-path)
	(require 'pts-mode))
    (message "Loading PTS returned an invalid path: %s" pts-mode-path)))
```

This assumes that `pts` is on your path and calls it to find the
emacs mode. Keybindings in pts-mode:

 - <kbd>C-c</kbd> <kbd>C-l</kbd> check types and assertions in current buffer

## Ctags support

If you use exuberant ctags, you can add to your `.ctags` the following block:

```
--langdef=LPTS
--langmap=LPTS:.lpts
--regex-LPTS=/^>[ \t]*([^][ \t.:=;()$]+)[ \t]*(\([^()]*\)[ \t]*)*([:=]|$)/\1/v,values/
--regex-LPTS=/^>[ \t]*export ([^][ \t.:=;()$]+)[ \t]*;/\1/e,exports/
--regex-LPTS=/^>[ \t]*module ([^][ \t.:=;()$]+)[ \t]*;/\1/m,modules/
--langdef=PTS
--langmap=PTS:.pts
--regex-PTS=/^[ \t]*([^][ \t.:=;()$]+)[ \t]*(\([^()]*\)[ \t]*)*([:=]|$)/\1/v,values/
--regex-PTS=/^[ \t]*export ([^][ \t.:=;()$]+)[ \t]*;/\1/e,exports/
--regex-PTS=/^[ \t]*module ([^][ \t.:=;()$]+)[ \t]*;/\1/m,modules/
```

You can then run ctags to produce Emacs tags with a command line similar to the
following:

```
ctags -e $(find . -name '*.pts' -o -name '*.lpts')
```

## Contributing

I am happy to receive pull requests. Note that I assume that I
can publish the code in pull requests under a three-clause BSD
license.
