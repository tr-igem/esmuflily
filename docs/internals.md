Internals
=========

Description of some internals in [Esmuflily](https://github.com/tr-igem/esmuflily).

The musical symbols supported by Esmuflily are SMuFL-compliant glyphs
given with their Unicode code point.

All symbols are assembled in a single internal table `ekm:types`
arranged according to type (usage), where some types correspond to
LilyPond's graphical objects like note heads, flags, rests, and clefs.

The predefined internal table contains SMuFL recommended characters.
Further (external) tables can be merged into the internal table,
i.e. new styles, names, and tokens are added, and already existing ones
are replaced.

An external table can be:

-   a `types` sub-table in a font-specific cache file "ekmd-FNAME.scm".
-   in a font-specific file "types-FNAME.scm".
-   in the file "types-template.scm".
-   specified for a single type with the command `\ekmMergeType`.


Table structues
---------------

The table `ekm:types` is an alist of type tables, each containing
an alist of style tables, each defining related musical symbols,
possibly of two different forms (DOWN/UP, LEFT/RIGHT, main/variant,
slower/faster).

```scheme
(
  (TYPE
    (STYLE
      SYMBOL-ENTRY
      ...
    )
    (STYLE . INDEX-TABLE)
    ...
  )
  ...
)
```

-   TYPE (symbol): \
    Type of musical symbols. Many types correspond to grobs,
    eg. notehead, flag, rest.

-   STYLE (symbol or #t): \
    Style of musical symbols. At least one style is predefined.
    The first STYLE of a type is the default.

    - `#t` names an identifier table or a token table.
    - `ekm` is used internally (eg. arrow, beater).

-   SYMBOL-ENTRY (pair): \
    The last SYMBOL-ENTRY of a style is the default.

    - If TYPE does not distinguish two forms (DIR = #f)
      SYMBOL-ENTRY has a single SYMBOL:

      | SYMBOL-ENTRY        | DIR = #f  |
      |---------------------|-----------|
      | (KEY . SIMPLE)      | SIMPLE
      | (KEY SIMPLE)        | (SIMPLE)
      | (KEY . LIST)        | LIST
      | (KEY LIST-ELEM ...) | LIST

    - If TYPE distinguishes two forms (DIR < 0 or DIR ≥ 0),
      SYMBOL-ENTRY has either one SYMBOL for both forms or two SYMBOLs:

      | SYMBOL-ENTRY                  | DIR < 0   | DIR ≥ 0   |
      |-------------------------------|-----------|-----------|
      | (KEY . SIMPLE)                | SIMPLE    | SIMPLE
      | (KEY SIMPLE)                  | SIMPLE    | SIMPLE
      | (KEY . (LIST))                | LIST      | LIST
      | (KEY LIST)                    | LIST      | LIST
      | (KEY SIMPLE-1 . SIMPLE-2)     | SIMPLE-1  | SIMPLE-2
      | (KEY LIST-1 . LIST-2)         | LIST-1    | LIST-2
      | (KEY LIST-1 LIST-2-ELEM ...)  | LIST-1    | LIST-2

-   KEY (number, string, symbol): \
    The key depends on TYPE, usually:

    - NAME (string, symbol): Identifier
    - TOKEN (string): Token for definition strings
    - LOG (integer): Duration log
    - LIMIT (number): Maximum size

-   INDEX-TABLE: \
    Sequence of SYMBOLs accessed by an index ≥ 0.
    A single CP means consecutive code points CP + index.

    - #(EXTEXT ...)
    - CP

-   SYMBOL: \
    Data to draw a musical symbol.

    - SIMPLE (CP, string): Simple value
    - LIST: One or more EXTEXT, or an EXTEXT with additional data

-   EXTEXT (integer, string, list, #f):

    - CP
    - "..."
    - (CP FEATURE ...)
    - (CP-1 CP-2 ...)
    - (markup ...)
    - #f

-   CP (integer): \
    Code point of the glyph to draw. 0 draws a point-stencil.


### Style table

A type table can have one or more style tables.

```scheme
(TYPE
  (STYLE
    SYMBOL-ENTRY
    ...
  )
  ...
)
```


### Identifier table

Special style table with the name `#t`.
A type table with an identifier table has usually no other style tables.

```scheme
(TYPE
  (#t
    SYMBOL-ENTRY
    ...
  )
)
```


### Token table

Special identifier table with the first entry `#t`.
This entry is only crucial for the correct merger of the predefined table
with a font-specific table.

A DEFINITION string, i.e. a sequence of one or more tokens,
draws the corresponding SYMBOLs stacked in a line.

```scheme
(TYPE
  (#t #t
    (TOKEN . SYMBOL)
    ...
  )
)
```

-   TOKEN (string): \
    A token which is a prefix of other tokens in the same table
    must be arranged after them, i.e. the correct order is "abc", "ab", "a".
    Else the other keys will be ignored.

-   SYMBOL (EXTEXT): \
    Musical symbol.
    `#f` ignores TOKEN in DEFINITION strings.

#### Shared token table

The type `shared` defines additional tokens that are always applicable
in DEFINITION strings.


### Limit table

Special style table whose keys are maximum numbers to select an entry.
The entries must be in ascending order of LIMIT.
The last entry should have the LIMIT `+inf.0`.

```scheme
(TYPE
  (STYLE
    (LIMIT ...)
    ...
  )
  ...
)
```


### Orientation table

Type table with index-tables.

```scheme
(TYPE
  (ekm
    (LENGTH INDEX-0 INDEX-1 ...)
    ...
    (-1 . #(ORIENT-0 ORIENT-1 ...))
  )
  (STYLE . INDEX-TABLE)
  ...
)
```

-   LENGTH (integer): \
    Length of an INDEX-TABLE. > 0 for a vector. 0 for CP.

-   INDEX-I (integer): \
    Correct index of the symbol at index I in an INDEX-TABLE.

-   ORIENT-I: \
    Value to complete the missing symbol at index I with the symbol at INDEX.

    - INDEX
    - (INDEX . XFORM)

-   XFORM (number): \
    Transformation to obtain a missing symbol.
    X,Y (0,1) flips. Any other value rotates counter-clockwise.


### Table access procedures

-   ```scheme
    (ekm:asst TYPE-OR-TABLE STYLE KEY DIR)
    ```
    Return the value according to DIR of KEY in the style table STYLE
    in the type table TYPE or in TABLE,
    or in the style table TABLE if STYLE is `#f`.
    Return the value of the last entry if KEY is not found.

-   ```scheme
    (ekm:assld TABLE GROB LOG DIR)
    ```
    Like `(ekm:asst)` but for the GROB properties `style`, `duration-log`,
    and `Stem.direction`, or for LOG or DIR if true.
    If GROB is not a grob it must be a style, and LOG and DIR must be true values.

-   ```scheme
    (ekm:asslim TYPE STYLE SIZE DIR)
    ```
    Return the value according to DIR of the first entry with a key ≥ SIZE
    in the style table STYLE in the type table TYPE.
    Return 0 if STYLE is not found or if no key ≥ SIZE found.

-   ```scheme
    (ekm:assid TYPE KEY)
    ```
    Return the value of KEY in the identifier table in the type table TYPE,
    or the whole identifier table if KEY is `#f`. In this case it skips
    the first entry `#t` (token table).

-   ```scheme
    (ekm:tokens TABLE DEF TOKENS)
    ```
    Return a list of markups of the musical symbols corresponding to
    the tokens in the string DEF, according to the token table TABLE.
    The first element of the list is a list of the tokens in DEF
    if TOKENS is true (used only by ekm-harp-pedal), else it is `()`.

-   ```scheme
    (ekm:assq TABLE KEY)
    ```
    Return the value of KEY in TABLE, or the value of the first entry
    if KEY is not found.

-   ```scheme
    (ekm:asstl TYPE STYLE)
    ```
    Return the style table STYLE in the type table TYPE,
    or `#f` if STYLE is not found.

-   ```scheme
    (ekm:sym VAL DIR)
    ```
    Return the part (symbol) of VAL according to DIR (#f, < 0, ≥ 0).

-   ```scheme
    (ekm:reverse DIR)
    ```
    Return the opposite direction, unless DIR is #f.

-   ```scheme
    (ekm:mv VAR)
    ```
    Return 1 (for variant symbol) if VAR is true, else `MAIN`.


Types
-----

| Type        | Structue | Forms | Description |
|-------------|----------|-------|-------------|
| notehead    | S   | Y   | Note heads
| cluster     | S   | Y   | Note clusters
| flag        | S   | Y   | Flags
| rest        | S   | M   | Rests (normal, ledgered)
| mmrest      | S   | X   | Multi-measure rests
| dot         | S   | N   | Augmentation dots
| script      | I   | Y   | Scripts, Expressive marks
| clef        | I   | M   | Clefs (normal, change)
| clef-mod    | I   | X,N | Clef modifiers
| time        | I   | N   | Special time/cadenza signatures
| time-sub    | I   | N   | Time signature sub-fractions
| colon       | I   | N   | Colon bar glyphs and lines
| segno       | I   | N   | Segno bar glyphs and lines
| barline     | I   | N   | Staff dividers
| separator   | L   | N   | System separator marks
| dynamic     | T   | N   | Dynamics
| systemstart | L   | X   | System start delimiter
| spanner     | S   | X,W | Multi-segment spanner
| fingering   | S   | N   | Fingering
| pedal       | T   | N   | Piano pedals
| harp        | T   | N   | Harp pedals
| ottava      | T   | N   | Ottavation
| ottavation  | S   | Y   | Ottavation styles
| tuplet      | I   | N   | Tuplet number special symbols
| tremolo     | S   | N   | Tremolo marks
| stem        | I   | N   | Stem decorations
| grace       | S   | Y   | Grace note slash
| lv          | L   | Y   | Laissez vibrer
| arpeggio    | S   | Y   | Arpeggios
| percent     | I   | N   | Percent repeats
| fret        | I   | M   | Fret diagrams
| accordion   | S   | N   | Accordion registers
| brass       | S   | Y   | Falls and doits
| fbass       | I   | N   | Figured bass
| fbass-acc   | I   | N   | Figured bass accidentals
| lyric       | I   | N   | Lyrics
| analytics   | T   | N   | Analytics
| func        | T   | N   | Function theory
| arrow       | O   | N   | Arrows and arrow heads
| beater      | O   | N   | Percussion beaters
| level       | S,L | N,M | Level display (empty, thumb)
| misc        | I   | X,N | Miscellaneous symbols
| number      | S   | N   | Numbers, Digits
| parens      | S   | X   | Parentheses
| shared      | T   | N   | Shared token table

### Type table structue

-   S = Style tables
-   I = Identifier table
-   T = Token table
-   L = Limit tables
-   O = Orientation table

### Forms of symbols in the table entries

-   X = LEFT and RIGHT
-   Y = DOWN and UP
-   M = main and variant
-   W = smaller and wider (slower and faster)
-   N = do not distinguish forms


Note heads
----------

```scheme
(notehead
  (STYLE
    NOTEHEAD-ENTRY
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, harmonic, diamond, cross, triangle, ...

-   NOTEHEAD-ENTRY:

    - (LOG . NH)
    - (LOG SYMBOL)
    - (LOG SYMBOL-DOWN . SYMBOL-UP)

-   LOG (integer):
    Duration log, usually -1 to 2.

-   SYMBOL:

    - NH
    - (NH . NH-EMPTY)

-   NH (CP):
    Note head symbol.

-   NH-EMPTY (CP or #f):
    Symbol to whiteout the background. Intended for note name note heads.


### Note head metadata

```scheme
ekmd:glyphs (
  ...
  (NH CONVERTED STEM-ATTACH-DOWN STEM-ATTACH-UP)
  ...
)
```

-   CONVERTED (boolean):
    `#t` if STEM-ATTACH is converted for the `stem-attachment` property.

-   STEM-ATTACH (pair):
    Either the original metadata from "stemUpSE" and "stemDownNW",
    or the converted values for the `stem-attachment` property.


#### Convert note head metadata

```
    bBoxNE        --------------   o - - - - -
                 /      :       \
                /       :        \           y-up
  stemUpSE     /        :         o  - - -
              /         :          |    uy
         0   | - - - - -:- - - - - | - - - - -
             |          :         /     dy
stemDownNW    o         :        /   - - -
               \        :       /            y-down
                \       :      /
    bBoxSW   o   --------------      - - - - -
             ::         :         ::
             ::    dx   :   ux    ::
             :                     :
             :       x-right       :
```

```
x-ext = (0 . x-right)
y-ext = (y-down . y-up)
w = x-right / 2

ux = (stemUpSE.x - w) / w = stemUpSE.x / w - 1
uy = stemUpSE.y / y-up
dx = (stemDownNW.x - w) / w = stemDownNW.x / w - 1
dy = stemDownNW.y / abs(y-down)

stem-attach-up   = (ux . uy)
stem-attach-down = (dx . dy)
```


Note clusters
-------------

Implements Henry Cowell's clusters.

```scheme
(cluster
  (STYLE
    CLUSTER-ENTRY
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, harmonic, diamond, square

-   CLUSTER-ENTRY:

    - (LOG SYMBOL)
    - (LOG SYMBOL-DOWN . SYMBOL-UP)

-   LOG (integer):
    Duration log, usually -1 to 2.

-   SYMBOL:

    - (NH-1 NH-2 NH-3 NH-TOP NH-MID NH-BOTTOM STEM-POS)

-   NH-1, NH-2, NH-3 (CP or #f):
    Note head symbols for unison, second, and third.

-   NH-TOP, NH-MID, NH-BOTTOM (CP):
    Top, middle, and bottom segments for larger intervals.

-   STEM-POS (number):
    Added to the `stem-begin-position` property of stem down.


Flags
-----

```scheme
(flag
  (STYLE
    (LOG SYMBOL-DOWN . SYMBOL-UP)
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, short, straight, ...

-   LOG (integer):
    Duration log, usually 3 to 10.

-   SYMBOL (EXTEXT):
    Flag symbol.


### Flag stem length

```scheme
ekm-stemlength-tab (
  (STYLE
    (XLENGTH-DOWN . XLENGTH-UP)
    (LENGTH-DOWN . LENGTH-UP)
  )
  ...
)
```

-   XLENGTH (list):

    - (NOMINAL-LENGTH EXTRA-LENGTH-3 ... EXTRA-LENGTH-10)

-   LENGTH (list):

    - (NOMINAL-LENGTH STEM-LENGTH-3 ... STEM-LENGTH-10)

-   NOMINAL-LENGTH (number):
    Nominal unmodified stem length, usually 3.5.

-   EXTRA-LENGTH (number):
    Amount to lengthen stem for duration log 3 to 10.

-   STEM-LENGTH (number):
    Stem length for duration log 3 to 10.


### Flag metadata

Used to initialize `ekm-stemlength-tab`.

```scheme
ekmd:glyphs (
  (FLAG-DOWN (0 . EXTRA-LENGTH-DOWN) (#f . #f))
  (FLAG-UP   (#f . #f) (0 . EXTRA-LENGTH-UP))
  ...
)
```


Rests
-----

```scheme
(rest
  (STYLE
    REST-ENTRY
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, classical, z

-   REST-ENTRY:

    - (LOG . REST)
    - (LOG REST REST-LEDGERED)

-   LOG (integer):
    Duration log, usually -3 to 10.

-   REST (EXTEXT):
    Normal rest symbol.

-   REST-LEDGERED:

    - REST-PRECOMPOSED
    - (REST-BARE LEDGER-LINE DIR ...)

-   REST-PRECOMPOSED (EXTEXT):
    Precomposed ledgered rest symbol.
    Note: If itself is a list it must be enclosed in a list.

-   REST-BARE (EXTEXT):
    Rest symbol to be combined with LEDGER-LINE, usually equal REST.

-   LEDGER-LINE (EXTEXT or #t):
    Ledger line symbol.
    `#t` draws a ledger line according to the metadata `legerLineThickness`
    and `legerLineExtension`.

-   DIR (integer):
    Position on REST-BARE where to place LEDGER-LINE, usually 1 or -1.
    Several values can be specified.


Multi-measure rests
-------------------

```scheme
(mmrest
  (STYLE HBAR-MID DRAW HBAR-LEFT . HBAR-RIGHT)
  (STYLE HBAR-MID DRAW . #f)
  ...
)
```

-   STYLE (symbol):
    default, ...

-   HBAR-MID (EXTEXT or #f):
    Middle symbol of horizontal bar.

-   DRAW (boolean):

    - `#t` draws HBAR-MID stretched, i.e. scales the stencil horizontally.
      The height remains unchanged. This is the default.

    - `#f` draws a horizontal bar with the Y extent of HBAR-MID,
      or with metadata `hBarThickness` if HBAR-MID is `#f`.

-   HBAR-LEFT/RIGHT (EXTEXT):
    Edge symbol of horizontal bar.


Augmentation dots
-----------------

```scheme
(dot
  (STYLE DOT PAD-3 PAD-4 PAD-5)
  ...
)
```

-   STYLE (symbol):
    default, note, metronome, ...

-   DOT (EXTEXT):
    Augmentation dot symbol.

-   PAD-3, PAD-4, PAD-5 (number):
    Padding between a note of duration log 3, 4, 5, and the dot
    in units of the dot width. PAD-5 applies also to higher logs.

PAD and styles other than `default` and `note` are used only by `\ekm-note-by-number`.


Scripts, Expressive marks
-------------------------

```scheme
(script (#t
  SCRIPT-ENTRY
  ...
))
```

-   SCRIPT-ENTRY:

    - (NAME SYMBOL)
    - (NAME SYMBOL-DOWN . SYMBOL-UP)

-   NAME (string):
    "sforzato", "dmarcato", "trill", "dfermata", ...
    This is the car of the `script-stencil` property.

-   SYMBOL (EXTEXT):
    Expressive mark.

Note: The command `\ekmScript` takes a pair of symbols in the reverse
order (SYMBOL-UP . SYMBOL-DOWN) which is more intuitive, and compatible
with former Esmuflily.


Clefs
-----

```scheme
(clef (#t
  CLEF-ENTRY
  ...
))
```

-   CLEF-ENTRY:

    - (NAME CLEF . CLEF-CHANGE)
    - (NAME (CLEF CLEF-POSITION TRANSPOSITION C0-POSITION) . CLEF-CHANGE)

-   NAME (string):
    "clefs.G", ...

-   CLEF (CP):
    Normal clef symbol.

-   CLEF-CHANGE (CP or #f):
    Change clef symbol.
    `#f` draws CLEF with a 2 steps smaller font size.

-   CLEF-POSITION, TRANSPOSITION, C0-POSITION (integer):
    Position data for a new clef. Default is 0, 0, 0.

A new font-specific clef can be added to LilyPond's list of supported clefs.

-   Its NAME must not have the prefix "clefs."
-   It requires position data. Default is 0, 0, 0.


### Clef modifiers

```scheme
(clef-mod (#t
  CLEF-MOD-ENTRY
  ...
))
```

-   CLEF-MOD-ENTRY:

    - (NAME SYMBOL)
    - (PARENS SYMBOL-LEFT . SYMBOL-RIGHT)

-   NAME (string):
    "8", "15", ...

-   PARENS (symbol):
    parenthesized, bracketed

-   SYMBOL (EXTEXT):
    Clef modifier or modifier parenthesis symbol.


Time / Cadenza signatures
-------------------------

### Digits

Number table with style `time`.


### Special symbols

```scheme
(time (#t
  (NAME SYMBOL)
  ...
))
```

-   NAME (string):
    "C", "+", "/+", "X", ...

-   SYMBOL (EXTEXT):
    Special time or cadenza signature symbol.


### Sub-fractions

```scheme
(time-sub (#t
  (FRACTION SYMBOL)
  ...
))
```

-   FRACTION (rational):
    Value of a sub-fraction to be part of a numerator.

-   SYMBOL (EXTEXT):
    Precomposed fraction symbol.


### Procedures

-   ```scheme
    (ekm-time-num LIST NUM)
    ```
    Return the first element of LIST.
    If NUM is true, the element is converted to a markup number.

-   ```scheme
    (ekm-time-join LIST SEP DENOM)
    ```
    Return (RLIST . RD) where RLIST is a list with the elements from LIST
    and SEP inserted between them (infix), and RD is #f.
    If DENOM is true, RD is the last element of LIST (denominator)
    and all elements are converted to markup.

-   ```scheme
    (ekm-time-fraction FRACTION STYLE)
    ```
    Return FRACTION as markup. FRACTION must be a number or a pair or list
    of numbers. The last number is the denominator unless only one number
    is given, or if STYLE is 'single-digit.

-   ```scheme
    (ekm-time-plain LIST)
    ```
    Return LIST with sub-fractions replaced by simple fractions required
    for Timing properties.

See scm\time-signature-settings.scm


Bar glyphs and Bar lines
------------------------

```scheme
(colon (#t
  BAR-ENTRY
  ...
))
```

```scheme
(segno (#t
  BAR-ENTRY
  ...
))
```

-   BAR-ENTRY:

    - (NAME BAR SHOW SCALE)
    - (BAR-NAME EOL-NAME BOL-NAME SPAN-NAME)

-   NAME (char)
    Name of a bar glyph. Must be an ASCII character.

-   BAR (EXTEXT)
    Bar glyph.

-   SHOW (boolean):
    `#f` draws an empty glyph with the X-extent of BAR.

-   SCALE (number or #f):
    Scaling factor for the property `segno-kern`.
    `#f` draws BAR without bar lines (usually colon).

-   BAR-NAME, EOL-NAME, BOL-NAME, SPAN-NAME (string):
    Name of bar line normal, eol, bol, and span bar.
    `#t` draws BAR-NAME. `#f` draws nothing.

See scm/bar-line.scm


Staff dividers
--------------

```scheme
(barline (#t
  (DIR SYMBOL)
  ...
))
```

-   DIR (integer):
    CENTER, DOWN, UP

-   SYMBOL (EXTEXT):
    Staff divider symbol.


System separator marks
----------------------

```scheme
(separator
  (STYLE
    (LIMIT SYMBOL)
    ...
  )
)
```

-   STYLE (symbol):
    default (usually the only style)

-   LIMIT (number):
    The first entry with LIMIT ≥ the specified size is selected.

-   SYMBOL (EXTEXT):
    System separator mark.


Dynamics
--------

```scheme
(dynamic (#t
  (TOKEN . SYMBOL)
  ...
)
```

-   TOKEN (string):
    "p", "m", "f", "r", "s", "z", "n", "mp", ...

-   SYMBOL (CP):
    Absolute dynamic symbol.

This is actually an identifier table, not a token table.
`\ekm-dynamic` draws either a single symbol for a TOKEN or a concatenation
of symbols for each letter.
This is different from the usual interpretation of DEFINITION strings.

See [LSR/Item?id=771](https://lsr.di.unimi.it/LSR/Item?id=771) for `\ekmParensHairpin`.


System start delimiter
----------------------

The requested height is determined from a `bar-line` delimiter.
Then, one or more of the following steps are performed to obtain
the requested height for the actual delimiter symbol.

1.  Select
2.  Scale
3.  Stretch
4.  Lengthen


### Select

Select the symbol that is intended for the requested height.
This enables optical sizing.
The available symbols are mapped onto height limits.

Typical (default) heights of a staff group, i.e. of a `bar-line` delimiter
in LilyPond and the appropriate limits:

| Staves  | Height  | Limit |
|---------|---------|-------|
| 1       | 4.1     | 9
| 2       | 13.1    | 18
| 3       | 22.1    | 27
| 4       | 31.1    | 36
| 5       | 40.1    | 45
| 6       | 49.1    | 54


### Scale

Set the font size to scale the delimiter proportionally.
The original symbol has either the height for a five-line staff (1em)
or a height close to (or equal to) the requested height.


### Stretch

Scale the stencil of the delimiter vertically.
The width remains unchanged.


### Lengthen

This is a variant of the "large-item hack" proposed by
[Daniel Benjamin Miller](https://github.com/dbenjaminmiller).

Draw a top, middle, and bottom segment of the delimiter,
as well as intermediate vertical bars (fitting segments).
It assumes that the delimiter is vertically symmetric.

-   H = Original height, after scale and stretch
-   RH = Requested height
-   ADD = Additional height = RH - H
-   END = Height of the end (bottom, top) segments
-   MID = Height of the middle segment
-   REM = Height of the remaining parts = (H - MID)/2 - END
-   FIT = Height of the fitting segments = (RH - MID)/2 - END

The parts which shall not be drawn are masked with a white rectangle.
This works under the condition RH ≥ H + 2 * END .

Note: Clipping is not supported in LilyPond. There is a potential
[solution](https://lists.gnu.org/archive/html/lilypond-user/2020-12/msg00112.html)
but it basically works only for postscript.

The following components are drawn in this order:

1.  Delimiter for bottom segment

2.  Delimiter translated by ADD for top ssegment

3.  Mask from END to (RH - END)

4.  Delimiter translated by ADD/2 for middle segment

5.  Bottom mask from END to (RH - MID)/2,
    and top mask from (RH + MID)/2 to (RH - END)

6.  Bottom fitting from END to (RH - MID)/2,
    and top fitting from (RH + MID)/2 to (RH - END)

```
          Bottom     Top     Mask   Middle    Mask   Fitting   Result
---                     --                                         --   --- ---
                       //                                         //    END
                      --    ------           ------    --        --     ---
ADD                   ||    |    |       --  |    |    ||        ||
                      --    |    |      //   |    |    ||        ||     FIT
                     //     |    |     --    |    |    ||        ||
--- ---        --   //      |    |     ||    |    |    ||        ||
    END       //   <<       |    |     --    ------    --        --     ---
    ---      --     \\      |    |    //                        //
    REM      ||      \\     |    |   //                        //
    ---      --       --    |    |  <<                        <<        MID  RH
            //        ||    |    |   \\                        \\
           //         --    |    |    \\                        \\
H   MID   <<           \\   |    |     --    ------    --        --     ---
           \\           --  |    |     ||    |    |    ||        ||
            \\              |    |     --    |    |    ||        ||
    ---      --             |    |      \\   |    |    ||        ||     FIT
    REM      ||             |    |       --  |    |    ||        ||
    ---      --             ------           ------    --        --     ---
    END       \\                                                  \\    END
--- ---        --                                                  --   --- ---
```


### Table

```scheme
(systemstart
  (STYLE
    SYSTEMSTART-ENTRY
    ...
  )
  ...
)
```

-   STYLE (symbol):
    brace, bracket

-   SYSTEMSTART-ENTRY:

    - (LIMIT SYMBOL)
    - (LIMIT SYMBOL-LEFT . SYMBOL-RIGHT)

-   LIMIT (number):
    The first entry with LIMIT ≥ RH is selected.

-   SYMBOL:

    - (DELIMITER)
    - (DELIMITER SCALE)
    - (DELIMITER SCALE STRETCH)
    - (DELIMITER SCALE STRETCH END MID LEFT RIGHT)
    - (#f SCALE STRETCH BOTTOM TOP LEFT RIGHT)

-   DELIMITER (EXTEXT):
    Delimiter symbol.
    `#f` draws the delimiter with BOTTOM and TOP without masks and
    without a middle segment. This is intended for brackets.

-   SCALE (number):
    Scale factor from EM to staff spaces. Default is 255/1000.

-   STRETCH (number or #f):
    Stretch factor.
    `#f` does not stretch (= 1). This is the default.

-   END (number):
    Relative height of the end segments of DELIMITER.

-   MID (number):
    Relative height of the middle segment of DELIMITER.

-   BOTTOM, TOP (EXTEXT):
    Symbols to draw as end segments if DELIMITER is `#f`.

-   LEFT, RIGHT (number or #f):
    X extent of the fitting segment.
    `#f` uses the `thickness` property whose default is 0.45 for brackets.


Multi-segment spanner
---------------------

```scheme
(spanner
  (STYLE
    SPANNER-ENTRY
    ...
  )
  ...
)
```

-   STYLE (symbol):
    trill, vibrato, wavy, ...

-   SPANNER-ENTRY:

    - (text TEXT-LEFT . TEXT-RIGHT)
    - (0 . MAIN)
    - (TEMPO FASTER . SLOWER)

-   TEXT-LEFT, TEXT-RIGHT (CP):
    Left and right symbol to be placed on each spanner piece.
    No entry equals `(text 0 . 0)`.

-   TEMPO (index):
    Absolute tempo.

-   MAIN (CP):
    Main (medium) extender line segment for tempo 0.

-   FASTER, SLOWER (CP):
    Faster (narrower) and slower (wider) extender line segment for TEMPO.


Fingering
---------

```scheme
(fingering
  (STYLE
    (NAME SYMBOL)
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, italic

-   NAME (string):
    "1", "2", "th", ...

-   SYMBOL (EXTEXT):
    Fingering symbol.

The fingering symbols p, i, m, a, x are used by the `StrokeFinger` grob.


Piano pedals
------------

```scheme
(pedal (#t #t
  (TOKEN SYMBOL)
))
```

-   TOKEN (string):
    "Ped.", "Sost.", "*", ...

-   SYMBOL (EXTEXT):
    Piano pedal symbol.


Harp pedals
-----------

```scheme
(harp (#t #t
  (TOKEN SYMBOL)
))
```

-   TOKEN (string):
    "^", "-", "v", ...

-   SYMBOL (EXTEXT):
    Harp pedal symbol.


Ottavation
----------

```scheme
(ottava (#t #t
  (TOKEN SYMBOL)
  ...
))
```

-   TOKEN (string):
    "8", "8va", "15", "15ma", ...

-   SYMBOL (EXTEXT):
    Ottavation symbol.


### Ottavation styles

```scheme
(ottavation
  (STYLE
    OTTAVATION-ENTRY
    ...
  )
  ...
)
```

-   OTTAVATION-ENTRY:

    - (OCTAVE . DEFINITION)
    - (OCTAVE DEFINITION-DOWN . DEFINITION-UP)

-   OCTAVE (index):
    Absolute octave number.

-   DEFINITION (string):
    Ottavation symbol defined as ottava tokens.


Tuplet number
-------------

### Digits

Number table with style `tuplet`.


### Notes

`\note-by-number` with style `metronome`.


### Special symbols

```scheme
(tuplet (#t
  (NAME SYMBOL)
  ...
))
```

-   NAME (string):
    ":"

-   SYMBOL (EXTEXT):
    Special tuplet symbol.

See output-lib.scm


Tremolo marks
-------------

```scheme
(tremolo
  (STYLE
    (FLAGS SYMBOL)
    ...
  )
)
```

-   STYLE (symbol):
    beam-like, fingered

-   FLAGS (integer):
    Number of flags for subdivision, usually 1 to 5.

-   SYMBOL (EXTEXT):
    Tremolo symbol.


Stem decorations
----------------

```scheme
(stem (#t
  (NAME SYMBOL)
  ...
))
```

-   NAME (string):
    "buzzroll", "sprechgesang", "sussurando", "ricochet2", ...

-   SYMBOL (EXTEXT):
    Stem decoration symbol (tremolo mark, accordion ricochet).


Grace note slashes
------------------

```scheme
(grace
  (STYLE
    (LOG SYMBOL-DOWN . SYMBOL-UP)
    ...
  )
)
```

-   STYLE (symbol):
    default
    This is the flag style.

-   LOG (integer):
    Duration log, usually 3 to 10.

-   SYMBOL:

    - (GRACE OFFSET-X . OFFSET-Y)

-   OFFSET (number):
    Offset for the grace note slash. Is scaled by font-size.

-   GRACE (EXTEXT):
    Grace note slash.


Laissez vibrer
--------------

```scheme
(lv
  (STYLE
    (LIMIT SYMBOL-DOWN . SYMBOL-UP)
    ...
  )
)
```

-   STYLE (symbol):
    default (usually the only style)

-   LIMIT (number):
    The first entry with LIMIT ≥ the specified size is selected.

-   SYMBOL (EXTEXT):
    Laissez vibrer symbol.


Arpeggios
---------

```scheme
(arpeggio
  (STYLE
    (DIR SYMBOL-DOWN . SYMBOL-UP)
    ...
  )
)
```

-   STYLE (symbol):
    default, swash

-   DIR (integer):

    - 0 (no direction): Simple arpeggio SYMBOL-UP (DOWN is not used).
    - 1 (or any other value): Arpeggio with arrow.

-   SYMBOL:

    - (BOTTOM MID TOP)

-   BOTTOM, MID, TOP (EXTEXT):
    Arpeggio line segments. MID is drawn zero or more times.
    BOTTOM and TOP are always drawn.


Percent repeats
---------------

```scheme
(percent (#t
  (NAME SYMBOL)
  ...
))
```

-   NAME (string):
    "/", "//", "%", "%%"

-   SYMBOL (EXTEXT):
    Percent repeat symbol.

See measure-counter-stencil in output-lib.scm


Fret diagrams
-------------

```scheme
(fret (#t
  FRET-ENTRY
  ...
))
```

-   FRET-ENTRY:

    - (NAME SYMBOL)
    - (FRETS FRETBOARD . FRETBOARD-NUT)

-   NAME (string):
    ".", "x", "o"

-   SYMBOL (EXTEXT):
    Mark for normal, muted, or open string.

-   FRETS (integer):
    Number of frets, usually 3 to 6.

-   FRETBOARD (EXTEXT):
    Fret board symbol with normal or thick top fret line.


### Markup properties

-   fret-diagram-details (alist):
    The following keys are supportet:

    - top-fret-thickness (number):
      A value > 1 draws a fret board with thick top fret line. Default is 3.

    - finger-code (symbol):
      Type (position) of fingering indications.
      `below-string` (default), `none`.

    - finger-style (symbol):
      Number style of fingering indications. Default is `sans`.


Accordion registers
-------------------

```scheme
(accordion
  (ekm
    (#f . 0))
  (dot . ACCREG-DOT)
  (STYLE
    ACCORDION-ENTRY
    ...
  )
)
```

-   ACCREG-DOT (EXTEXT):
    Combining accordion dot symbol.

-   STYLE (symbol):
    d (discant), sb (standard bass),
    sb4 (standard bass, four reed), sb5 (standard bass, five reed),
    sb6 (standard bass, six reed), fb (free bass), sq (square).

-   ACCORDION-ENTRY:

    - (NAME ACCREG)
    - (NAME ACCREG-EMPTY (DOT-X . DOT-Y) ...)

-   NAME (string):
    "1", "10", "1+0", "Soprano", "Alto", ...

-   ACCREG (EXTEXT):
    Accordion register symbol.

-   ACCREG-EMPTY (EXTEXT):
    Combining empty accordion register symbol.

-   DOT-X, DOT-Y (integer):
    Position of ACCREG-DOT in percent of the extent of ACCREG-EMPTY.

Note: The module `(scm accreg)` is not required.


Falls and doits
---------------

```scheme
(brass
  (STYLE
    (LOG SYMBOL-DOWN . SYMBOL-UP)
    ...
  )
)
```

-   STYLE (symbol):
    bend, rough, smooth, scoop
    `scoop` is used by `\ekm-scoop`.

-   LOG (integer):
    Duration log, usually 0 to 2.

-   SYMBOL:

    - BRASS
    - (BRASS . ALIGN)

-   BRASS (EXTEXT):
    Fall/doit (lift) or scoop/plop symbol.
    A single BRASS must be CP.

-   ALIGN (boolean):
    `#t` aligns the symbol up. Usually for rough and smooth down.


Figured bass
------------

### Digits

Number table with style `fbass`.


### Combining and precomposed symbols

```scheme
(fbass (#t
  (NAME SYMBOL)
  ...
))
```

-   NAME (string):
    "\\+", "\\\\", "/", "2\\+", ...

-   SYMBOL (EXTEXT):
    Combining symbol or precomposed bass figure for augmented/diminished.


### Accidentals

```scheme
(fbass-acc (#t
  (ALTERATION SYMBOL)
  ...
))
```

-   ALTERATION (rational):
    0, 1/2, 1, 3/2, -1/2, ...

-   SYMBOL (EXTEXT):
    Bass figure accidental.


Lyrics
------

```scheme
(lyric (#t
  (TOKEN SYMBOL)
  ...
))
```

-   TOKEN (char):
    `~`, `_`, `%` ...

-   SYMBOL (EXTEXT):
    Special lyric symbol.


Analytics
---------

```scheme
(analytics (#t #t
  (TOKEN SYMBOL)
  ...
))
```

-   TOKEN (string):
    "H", "CH", "N", ...

-   SYMBOL (EXTEXT):
    Analytics symbol.


Function theory
---------------

```scheme
(func (#t #t
  (TOKEN SYMBOL)
  ...
))
```

-   TOKEN (string):
    "D", "DD", "/D", "S", ...

-   SYMBOL (EXTEXT):
    Function theory symbol.

See [LSR/Item?id=967](https://lsr.di.unimi.it/LSR/Item?id=967) for `\ekmFunc`.


Arrows and arrow heads
----------------------

```scheme
(arrow
  (ekm
    (0 0 1 2 3 4 5 6 7)
    (1 0)
    (2 0 4)
    (4 0 2 4 6)
    (-1 . #(0 (0 . -45) (0 . -90) (1 . ,Y) (0 . ,Y) (7 . ,Y) (2 . ,X) (1 . ,X)
            0 (8 . -45) (8 . -90) (9 . ,Y))))
  (STYLE . INDEX-TABLE)
  ...
)
```

-   STYLE (symbol):
    `black`, `white`, `open`, ...

-   INDEX-TABLE:

    - AR
    - #(AR-N)
    - #(AR-N AR-S)
    - #(AR-N AR-E AR-S AR-W)
    - #(AR-N AR-NE AR-E AR-SE AR-S AR-SW AR-W AR-NW)
    - #(AR-N AR-NE AR-E AR-SE AR-S AR-SW AR-W AR-NW AR-NS AR-NESW AR-EW AR-SENW)

-   AR (CP):
    Arrow or arrow head.
    For single AR: CP + 0 to 7  =>  AR-N AR-NE AR-E ... AR-NW


Percussion beaters
------------------

```scheme
(beater
  (ekm
    (0 0 4 1 7)
    (1 0)
    (2 0 4)
    (4 0 4 1 7)
    (-1 . #(0 (0 . -30) (0 . -90) (1 . ,Y) (0 . ,Y) (7 . ,Y) (2 . ,X) (1 . ,X)
            0 (8 . -30) (8 . -90) (9 . ,Y))))
  (STYLE . INDEX-TABLE)
  ...
)
```

-   STYLE (symbol):
    `xyl-soft`, `timpani-soft`, `stick`, ...

-   INDEX-TABLE:

    - BTR
    - #(BTR-N)
    - #(BTR-N BTR-S)
    - #(BTR-N BTR-S BTR-NE BTR-NW)

-   BTR (CP):
    Percussion beater.
    For single BTR: CP + 0 to 3  =>  BTR-N BTR-S BTR-NE BTR-NW


Level display
-------------

Electronic music symbols: Volume level, MIDI controller

```scheme
(level
  (STYLE
    SUBSTITUTE-ENTRY
    (LEVEL SYMBOL)
    ...
    (+inf.0 . #f)
  )
  ...
)
```

-   STYLE (symbol):
    `fader`, `midi`

-   SUBSTITUTE-ENTRY:

    - (-1 . PRECISION)
    - (-1 EMPTY . THUMB)

-   PRECISION (number):
    Rounding factor for the specified level, usually 20.
    Select the first entry with LEVEL ≥ the specified level rounded
    to the nearest mutliple of PRECISION.

-   EMPTY, THUMB (EXTEXT):
    Combining empty display symbol and thumb symbol.
    Select the entry with LEVEL = the specifed level, else use EMPTY and THUMB.

-   LEVEL (number):
    0 (first entry), 100 (last entry).

-   SYMBOL (EXTEXT):
    Level display symbol.


Miscellaneous symbols
---------------------

```scheme
(misc (#t
  MISC-ENTRY
  ...
))
```

-   MISC-ENTRY:

    - (KEY SYMBOL)
    - (KEY SYMBOL-1 . SYMBOL-2)

-   KEY (symbol, string)
    `eyeglasses`, `metronome`


Numbers, Digits
---------------

Defines digits with index-tables and complete numbers with normal style tables.

```scheme
(number
  (STYLE . DIGIT)
  (STYLE . #(DIG0 DIG1 DIG2 DIG3 DIG4 DIG5 DIG6 DIG7 DIG8 DIG9))
  (STYLE . PROC)
  (STYLE
    (NUMBER . SYMBOL)
    ...
  )
  ...
  (ekm
    (STYLE . DEFAULT-PROC)
    ...
  )
)
```

-   STYLE (symbol):
    time, tuplet, fingering, fbass, func, string, ...

-   DIGIT (CP):
    Symbol for digit 0.

-   DIG0 - DIG9 (EXTEXT):
    Symbol for digit 0 to 9.

-   SYMBOL (EXTEXT):
    Symbol for NUMBER (not digit).

-   PROC (procedure):
    Markup procedure applied on the decimal digit string of the specified number.

-   DEFAULT-PROC (procedure):
    Markup procedure used if STYLE has no NUMBER = the specified number.


Parentheses
-----------

```scheme
(parens
  (STYLE
    (NAME SYMBOL-LEFT . SYMBOL-RIGHT)
    ...
  )
  ...
)
```

-   STYLE (symbol):
    default, bracket, brace, angle

-   NAME (symbol):
    accidental, dynamic, hairpin, func, text

-   SYMBOL:

    - PARENS-SIMPLE
    - (PARENS Y-ALIGN SIZE)

-   PARENS (EXTEXT):
    Parenthesis symbol.

-   Y-ALIGN, SIZE (number):
    Transformation values applied to PARENS.


Orientation positions
---------------------

```scheme
ekm-orient-pos '#(
  OFFSET-0
  ...
)
```

-   OFFSET-I (pair):
    Offset for the orientation index I = 0 to 7.

Used by `\ekm-label` for the label position.


Standard staff line positions
-----------------------------

```scheme
ekm-linepos-tab (
  (LINES POSITION ...)
  ...
)
```

-   LINES (integer):
    Number of staff lines.

-   POSITION (integer):
    Standard line position in the range LINES - 1 to - (LINES - 1).

New entries are added as necessary by `ekm-linepos`.
Used by multi-measure rests.
