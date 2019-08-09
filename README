                relint -- Emacs regexp mistake finder
                =====================================

Relint scans elisp files for mistakes in regexps, including deprecated
syntax and bad practice. It also checks the regexp-like arguments to
skip-chars-forward, skip-chars-backward, skip-syntax-forward and
skip-syntax-backward.

* Usage

  Check a single file:

    M-x relint-file

  Check all .el files in a directory tree:

    M-x relint-directory

  Check current buffer:

    M-x relint-current-buffer

  From batch mode:

    emacs -batch -l relint.el -f relint-batch FILES-AND-DIRS...

  where directories are scanned recursively.
  (Options for finding relint and xr need to be added after
  -batch, either -f package-initialize or -L DIR.)

  In the *relint* buffer, pressing "g" will re-run the same check.

* Installation

  From GNU ELPA (https://elpa.gnu.org/packages/relint.html):

    M-x package-install RET relint RET

  Relint requires the package xr (https://elpa.gnu.org/packages/xr.html);
  it will be installed automatically.

* What the diagnostics mean

  - Unescaped literal 'X'

    A special character is taken literally because it occurs in a
    position where it does not need to be backslash-escaped. It is
    good style to do so anyway (assuming that it should occur as a
    literal character).

  - Escaped non-special character 'X'
  
    A character is backslash-escaped even though this is not necessary
    and does not turn it into a special sequence. Maybe the backslash
    was in error, or should be doubled if a literal backslash was
    expected.
  
  - Duplicated 'X' inside character alternative
  
    A character occurs twice inside [...]; this is obviously
    pointless. In particular, backslashes are not special inside
    [...]; they have no escaping power, and do not need to be escaped
    in order to include a literal backslash.
  
  - Repetition of repetition
  
    A repetition construct is applied to an expression that is already
    repeated, such as a*+ (? counts as repetition here). Such
    expressions can be written with a single repetition and often
    indicate a different mistake, such as missing backslashes.

  - Reversed range 'Y-X' matches nothing

    The last character of a range precedes the first and therefore
    includes no characters at all (not even the endpoints). Most such
    ranges are caused by a misplaced hyphen.

  - Character 'B' included in range 'A-C'

    A range includes a character that also occurs individually. This
    is often caused by a misplaced hyphen.

  - Ranges 'A-M' and 'D-Z' overlap

    Two ranges have at least one character in common. This is often
    caused by a misplaced hyphen.

  - Two-character range 'A-B'

    A range only consists of its two endpoints, since they have
    consecutive character codes. This is often caused by a misplaced
    hyphen.

  - Duplicated character class '[:class:]'

    A character class occurs twice in a single character alternative
    or skip set.

  - Duplicated alternative branch

    The same expression occurs in two different branches, like in
    A\|A. This has the effect of only including it once.

  - Branch matches superset/subset of a previous branch

    A branch in an or-expression matches a superset or subset of what
    another branch matches, like in [ab]\|a. This means that one of
    the branches can be eliminated without changing the meaning of the
    regexp.

  - Uncounted repetition

    The construct A\{,\} repeats A zero or more times which was
    probably not intended.

  - Implicit zero repetition

    The construct A\{\} only matches the empty string, which was
    probably not intended.

  - Suspect '[' in char alternative

    This warning indicates badly-placed square brackets in a character
    alternative, as in [A[B]C]. A literal ] must come first
    (possibly after a negating ^).

  - Literal '-' not first or last

    It is good style to put literal hyphens last in character
    alternatives and skip sets, to clearly indicate that it was not
    intended as part of a range.

  - Repetition of zero-width assertion

    A repetition operator was applied to a zero-width assertion, like
    ^ or \<, which is completely pointless. The error may be a missing
    escaping backslash.

  - Repetition of expression matching an empty string

    A repetition operator was applied to a sub-expression that could
    match the empty string; this is not necessarily wrong, but such
    constructs run very slowly on Emacs's regexp engine. Consider
    rewriting them into a form where the repeated expression cannot
    match the empty string.

    Example: \(?:a*b*\)* is equivalent to the much faster \(?:a\|b\)*.

    Another example: \(?:a?b*\)? is better written a?b*. 

    In general, A?, where A matches the empty string, can be
    simplified to just A.

  - Unnecessarily escaped 'X'

    A character is backslash-escaped in a skip set despite not being
    one of the three special characters - (hyphen), \ (backslash) and
    ^ (caret). It could be unnecessary, or a backslash that should
    have been escaped.

  - Single-element range 'X-X'

    A range in a skip set has identical first and last elements. It is
    rather pointless to have it as a range.

  - Stray '\\' at end of string

    A single backslash at the end of a skip set is always ignored;
    double it if you want a literal backslash to be included.

  - Suspect skip set framed in '[...]'

    A skip set appears to be enclosed in [...], as if it were a
    regexp. Skip sets are not regexps and do not use brackets. To
    include the brackets themselves, put them next to each other.

  - Suspect character class framed in '[...]'

    A skip set contains a character class enclosed in double pairs of
    square brackets, as if it were a regexp. Character classes in skip
    sets are written inside a single pair of square brackets, like
    [:digit:].

  - Empty set matches nothing

    The empty string is a skip set that does not match anything, and
    is therefore pointless.

  - Negated empty set matches anything

    The string "^" is a skip set that matches anything, and is therefore
    pointless.

  - 'X' cannot be used for arguments to 'F'

    An expression that looks like a regexp was given as an argument to
    a function that expects a skip-set.

  - Value from 'X' cannot be spliced into '[...]'

    An expression that looks like a regexp was used to form a string
    where it is surrounded by square brackets, as if it were part of a
    character alternative. Regexps are not valid inside character
    alternatives; they use a different syntax.

    If you are just building a string containing a regexp for display
    purposes, consider using other delimiters than square brackets;
    displaying the regexp 0-9 as [0-9] is very misleading.

  - Invalid char 'X' in syntax string A string argument to

    skip-syntax-forward or skip-syntax-backward contains a character
    that doesn't indicate a syntax class. Such a string is not a
    regexp or skip-set, but just a string of syntax codes, possibly
    with a leading ^ for negation.

  - Duplicated char 'X' in syntax string

    A string argument to skip-syntax-forward or skip-syntax-backward
    contains a duplicated character, which is pointless and may
    indicate a mistake.

  - Empty syntax string

    A string argument to skip-syntax-forward or skip-syntax-backward
    is empty or "^", neither of which makes sense.

* Suppressing diagnostics

  While relint has been designed to avoid false positives, there may
  be cases where it emits unfounded complaints. Most of the time, it
  is worth the trouble to change the code to make them go away, but
  sometimes it cannot be done in a reasonable way.

  To suppress such diagnostics, add a comment on the form

    ;; relint suppression: MESSAGE

  on the line before the code where the error occurred. MESSAGE is a
  substring of the message to be suppressed. Multiple suppression
  comment lines can precede a line of code to eliminate several
  complaints on the same line.

* Bugs

  The recognition of regexps is done by ad-hoc rules; the simplistic
  method employed means that many errors will go undetected.

  Still, if you believe that a flawed regexp could have been
  discovered but wasn't, please report it as a bug. Reports of false
  positives and crashes are of course equally welcome.
