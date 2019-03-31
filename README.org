#+TITLE: relint.el

Relint (regular expression lint) scans elisp files for mistakes in
regexps, including deprecated syntax and bad practice. It also checks
the regexp-like arguments to ~skip-chars-forward~ and
~skip-chars-backward~.

* Usage

Check a single file:

: M-x relint-file

Check all .el files in a directory tree:

: M-x relint-directory

Check current buffer:

: M-x relint-current-buffer

From batch mode:

: emacs -batch -l relint.el -f relint-batch FILES-AND-DIRS...

where directories are scanned recursively.
(Options for finding relint and xr need to be added after
~-batch~, either ~-f package-initialize~ or ~-L DIR~.)

In the ~*relint*~ buffer, pressing "g" will re-run the same check.

* Installation

From [[https://elpa.gnu.org/packages/relint.html][GNU ELPA]]:

: M-x package-install RET relint RET

Relint requires the package [[https://elpa.gnu.org/packages/xr.html][xr]]; install it from GNU ELPA.

* Bugs

The recognition of regexps is done by ad-hoc rules; the simplistic
method employed means that many errors will go undetected.

Still, if you believe that a flawed regexp could have been discovered
but wasn't, please report it as a bug. Reports of false positives and
crashes are of course equally welcome.
