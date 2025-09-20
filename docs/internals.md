Internals
=========

Descriptions and comments on some internals of [Esmuflily](https://github.com/tr-igem/esmuflily).

The musical symbols supported by Esmuflily are SMuFL-compliant glyphs
given with their Unicode code point.
The symbols are assembled in a single table `ekm-types`
arranged according to ther usage for a specific type and style.

The predefined table contains SMuFL recommended characters.
It can be merged with a font-specific table provided in the `types`
sub-table of the metadata cache file.
The font-specific characters and styles either extend or replace
existing characters/styles.
A few symbols are not yet in the table but hard-coded in some commands.

The following markup commands still use [Ekmelos](https://github.com/tr-igem/ekmelos)
specific glyphs:

    \ekm-midi
    \ekm-eyeglasses
    \ekm-metronome



Table structues
---------------

The table `ekm-types` is an alist of type tables, each containing
an alist of style tables, each defining related musical symbols,
possibly of different directions.

    ekm-types (
      (TYPE
        (STYLE
          SYM-ENTRY
          ...
        )
        (STYLE . INDEX-TABLE)
        ...
      )
      ...
    )

*   TYPE (symbol):
    Type of musical symbols. Many types correspond to grobs,
    eg. notehead, flag, rest.

*   STYLE (symbol or #t):
    Style of musical symbols. At least one style is predefined.
    The first style of a type is the default.

    `#t` names an identifier table or a token table.
    `ekm` is used internally (eg. arrow, beater).

*   SYM-ENTRY (pair):

        (KEY . SYM)
        (KEY SYM)
        (KEY SYM-DOWN . SYM-UP)
        (KEY SYM ...)

    The first form is used if SYM is a single CP,
    or if TYPE does not distinguish directions.
    Else the second or third form is used.
    The last form is not used.

    The last entry of a style is the default.

*   KEY (number, string, symbol):
    The key depends on TYPE, usually:

    +   LOG (integer): Duration log
    +   NAME (string, symbol): Identifier
    +   TOKEN (string): Token for definition strings
    +   LIMIT (number): Maximum size

*   INDEX-TABLE:

        #(EXTEXT ...)
        CP

    Sequence of symbols accessed by an index >= 0.
    A single CP means consecutive code points CP + index.
    The order of the symbols depends on TYPE.

*   SYM:
    Data to draw a musical symbol.
    The structure depends on TYPE, usually CP or EXTEXT.

*   EXTEXT (integer, string, list):

        CP
        (CP FEATURE ...)
        (CP1 CP2 ...)
        "STRING"
        (markup ...)

*   CP (integer >= 0):
    Code point of the glyph to draw. 0 draws a point-stencil.


### Style table

A type table can have one or more style tables.

    (TYPE
      (STYLE
        SYM-ENTRY
        ...
      )
      ...
    )

*   TYPE: notehead, flag, rest, delimiter, spanner, finger, ottavation,
    tremolo, laissezvibrer, cluster, accordion, brass, number, parens,
    arrow, beater.


### Identifier table

Special style table with the name `#t`.
A type table that does not differentiate styles has a single
identifier table.

    (TYPE
      (#t
        SYM-ENTRY
        ...
      )
    )

*   TYPE: script, clef, clef-mod, dynamic, stem, fbass, fbass-acc, lyric.


### Token table

Special identifier table with the first entry `#t`.
This entry is only crucial for the correct merger of the predefined table
with a font-specific table.

A DEFINITION string which is a sequence of one or more tokens
draws the corresponding musical symbols stacked in a line.

    (TYPE
      (#t #t
        (TOKEN . SYM)
        ...
      )
    )

*   TYPE: shared, pedal, ottava, harp, analytics, func.

*   TOKEN (string):
    A token which is a prefix of other tokens in the same table
    must be arranged after them, ie. the correct order is "abc", "ab", "a".
    Else the other keys will be ignored.

*   SYM (EXTEXT or #f):
    Musical symbol.
    `#f` ignores TOKEN in DEFINITION strings.

#### Shared token table

The type `shared` defines additional tokens that are always applicable
in DEFINITION strings.
The predefined shared token table defines spaces:

    " "       ,(markup #:hspace 1)      SP
    "____"    ,(markup #:hspace 4)      EMSP
    "___"     ,(markup #:hspace 2)      ENSP
    "__"      ,(markup #:hspace 0.78)   THSP
    "_"       ,(markup #:hspace 0.17)   HSP


### Number table

Type table `number` with index-tables.

    (number
      (STYLE . DIG0)
      (STYLE . #(DIG0 DIG1 DIG2 DIG3 DIG4 DIG5 DIG6 DIG7 DIG8 DIG9))
      (STYLE
        (NUMBER . SYM-NUM)
        ...
      )
      ...
    )

*   DIG0 - DIG9 (CP):
    Digit symbol.

*   SYM-NUM (EXTEXT):
    Symbol for NUMBER (not a digit).
    Used in style `string` and `scale`.


### Orientation table

Type table with index-tables.

    (TYPE
      (ekm
        (LENGTH INDEX-0 INDEX-1 ...)
        ...
        (-1 . #(ORIENT-0 ORIENT-1 ...))
      )
      (STYLE . INDEX-TABLE)
      ...
    )

*   LENGTH (integer):
    Length of an INDEX-TABLE. > 0 for a vector. 0 for CP.

*   INDEX-I (integer):
    Correct index of the symbol at index I in an INDEX-TABLE.

*   ORIENT-I:

        INDEX
        (INDEX . XFORM)

    Value to complete the missing symbol at index I with the symbol at INDEX.

*   XFORM (number):
    Transformation to obtain a missing symbol.
    X,Y (0,1) flips. Any other value rotates counter-clockwise.


### Table access procedures

*   (ekm-asst TABLE STYLE KEY DIR)

    Return the value according to DIR of KEY in the style table STYLE in TABLE,
    or in the style table TABLE if STYLE is `#f`.
    Return the value of the last entry if KEY is not found.

    Types: flag, parens, script, spanner, accordion, brass, rest markup.

*   (ekm-assld TABLE GROB LOG DIR)

    Like `(ekm-asst)` but for the GROB properties `style`, `duration-log`,
    and `Stem.direction`, or for LOG or DIR if true.
    If GROB is not a grob it must be a style, and LOG and DIR must be true.

    Types: notehead, cluster, rest.

*   (ekm-asslim TYPE STYLE SIZE DIR)

    Return the value according to DIR of the first entry with a key > SIZE
    in the style table STYLE in the type table TYPE.
    Return 0 if STYLE is not found or if no key > SIZE found.
    The last entry in the style table should have the key `+inf.0`.

*   (ekm-assid TYPE KEY)

    Return the value of KEY in the identifier table in the type table TYPE,
    or the whole identifier or token table if KEY is `#f`.

*   (ekm-token-list TABLE DEF TOKENS)

    Return a markup list of the musical symbols corresponding to
    the tokens in the string DEF, according to the token table TABLE.
    The first element of the list is a list of the tokens in DEF
    if TOKENS is true (used only by ekm-harp-pedal), else it is '().

*   (ekm-number->list STYLE NUMBER)

    Return a list with the digit symbols or with SYM-NUM for NUMBER
    according to the number table STYLE in the type table `number`.

*   (ekm-assq TABLE KEY)

    Return the value of KEY in TABLE, or the value of the first entry
    if KEY is not found.

*   (ekm-assns TYPE STYLE)

    Return the style table STYLE in the type table TYPE,
    or `#f` if STYLE is not found.

*   (ekm-sym VAL DIR)

    Return the part of VAL according to DIR (default is DOWN):

        SYM-ENTRY             < 0     >= 0    #f
        ---------------------------------------------------
        (KEY . SYM)           SYM     SYM     SYM
        (KEY SYM)             SYM     SYM     (SYM)
        (KEY SYM1 . SYM2)     SYM1    SYM2    (SYM1 . SYM2)
        (KEY SYM ...)         SYM     (...)   (SYM ...)



Note heads
----------

    (notehead
      (STYLE
        NOTEHEAD-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, harmonic, diamond, cross, triangle, ...

*   NOTEHEAD-ENTRY:

        (LOG . NH)
        (LOG SYM)
        (LOG SYM-DOWN . SYM-UP)

*   LOG (integer):
    Duration log, usually in the range -1 to 2.

*   SYM:

        NH
        (NH . NH-EMPTY)

*   NH (CP):
    Note head symbol.

*   NH-EMPTY (CP or #f):
    Symbol to whiteout the background. Intended for note name note heads.


### Note head metadata

    ekmd:glyphs (
      ...
      (NH CONVERTED STEM-ATTACH-DOWN STEM-ATTACH-UP)
      ...
    )

*   CONVERTED (boolean):
    `#t` if STEM-ATTACH is converted for the `stem-attachment` property.

*   STEM-ATTACH (pair):
    Either the original metadata from "stemUpSE" and "stemDownNW",
    or the converted values for the `stem-attachment` property.


#### Convert note head metadata

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

    x-ext = (0 . x-right)
    y-ext = (y-down . y-up)
    w = x-right / 2

    ux = (stemUpSE.x - w) / w = stemUpSE.x / w - 1
    uy = stemUpSE.y / y-up
    dx = (stemDownNW.x - w) / w = stemDownNW.x / w - 1
    dy = stemDownNW.y / abs(y-down)

    stem-attach-up   = (ux . uy)
    stem-attach-down = (dx . dy)



Note clusters
-------------

Implements Henry Cowell's clusters.
See lilypond.1069038.n5.nabble.com/Cowell-clusters-td237881.html

    (cluster
      (STYLE
        CLUSTER-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, harmonic, diamond, square

*   CLUSTER-ENTRY:

        (LOG SYM)
        (LOG SYM-DOWN . SYM-UP)

*   LOG (integer):
    Duration log, usually in the range -1 to 2.

*   SYM:

        (NH-1 NH-2 NH-3 NH-TOP NH-MID NH-BOTTOM STEM-POS)

*   NH-1, NH-2, NH-3 (CP or #f):
    Note head symbols for unison, second, and third.

*   NH-TOP, NH-MID, NH-BOTTOM (CP):
    Top, middle, and bottom segments for larger intervals.

*   STEM-POS (number):
    Added to the `stem-begin-position` property of stem down.



Flags
-----

    (flag
      (STYLE
        FLAG-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, short, straight, ...

*   FLAG-ENTRY:

        (LOG FLAG-DOWN . FLAG-UP)

*   LOG (integer):
    Duration log in the range 3 to 10.

*   FLAG (CP):
    Flag symbol.


### Flag stem length

    ekm-stemlength-tab (
      (STYLE
        STEM-LENGTH EXTRA-LENGTH-3 ... EXTRA-LENGTH-10
      )
      ...
    )

*   STEM-LENGTH (number):
    Nominal unmodified stem length, usually 3.5.

*   EXTRA-LENGTH:

        (EXTRA-LENGTH-DOWN . EXTRA-LENGTH-UP)

    Amount to lengthen stem for duration log 3 to 10.
    Currently, only EXTRA-LENGTH-UP is used.


### Flag metadata

Used to initialize `ekm-stemlength-tab`.

    ekmd:glyphs (
      ...
      (FLAG-DOWN (0 . EXTRA-LENGTH-DOWN) (#f . #f))
      (FLAG-UP   (#f . #f) (0 . EXTRA-LENGTH-UP))
      ...
    )



Rests
-----

    (rest
      (STYLE
        REST-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, classical, z

*   REST-ENTRY:

        (LOG . REST)
        (LOG REST . REST-LEDGERED)

*   LOG (integer):
    Duration log in the range -3 to 10.

*   REST (CP):
    Normal rest symbol.

*   REST-LEDGERED (CP):
    Rest symbol with ledger line.
    Note: This is SYM-2 which is the symbol for UP in other tables.
    REST-ENTRY could be changed to (LOG (REST . REST-LEDGERED)).



Scripts - Expressive marks
--------------------------

    (script (#t
      SCRIPT-ENTRY
      ...
    ))

*   SCRIPT-ENTRY:

        (NAME SYM)
        (NAME SYM-DOWN . SYM-UP)

*   NAME (string):
    "sforzato", "dmarcato", "trill", "dfermata", ...
    This is the car of the `script-stencil` property.

*   SYM (EXTEXT):
    Expressive mark.



Clefs
-----

    (clef (#t
      CLEF-ENTRY
      ...
    ))

*   CLEF-ENTRY:

        (NAME CLEF . CLEF-CHANGE)
        (NAME (CLEF CLEF-POSITION TRANSPOSITION C0-POSITION) . CLEF-CHANGE)

*   NAME (string):
    "clefs.G", ...

*   CLEF (CP):
    Normal clef symbol.

*   CLEF-CHANGE (CP or #f):
    Change clef symbol.
    `#f` draws CLEF with a 2 steps smaller font size.

*   CLEF-POSITION, TRANSPOSITION, C0-POSITION (integer):
    Position data for a new clef. Default is 0, 0, 0.

A new font-specific clef can be added to the list of supported clefs.

*   Its NAME must not have the prefix "clefs."
*   It requires position data. Default is 0, 0, 0.


### Clef modifiers

    (clef-mod (#t
      CLEF-MOD-ENTRY
      ...
    ))

*   CLEF-MOD-ENTRY:

        (NAME . SYM)
        (PARENS SYM-LEFT . SYM-RIGHT)

*   NAME (string):
    "8", "15", ...

*   PARENS (symbol):

        parenthesized
        bracketed

*   SYM (EXTEXT):
    Clef modifier/parens symbol.



Time / Cadenza signatures
-------------------------

### Digits

Number table with style `time`.


### Special symbols

    (time (#t
      TIME-ENTRY
      ...
    ))

*   TIME-ENTRY:

        (NAME . SYM)

*   NAME (string):
    "C", "+", "/+", "X", ...

*   SYM (EXTEXT):
    Special time or cadenza signature symbol.


### Sub-fractions

    (time-sub (#t
      TIME-SUB-ENTRY
      ...
    ))

*   TIME-SUB-ENTRY:

        (FRACTION . SYM)

*   FRACTION (rational):
    Value of a sub-fraction to be part of a numerator.

*   SYM (EXTEXT):
    Precomposed fraction symbol.


### Procedures

*   (ekm-time-num L NUM)

    Return the first element of list L.
    If NUM is true, the element is converted to a markup number.

*   (ekm-time-join LS SEP DENOM)

    Return (RLS . RD) where RLS is a list with the elements from list LS
    and SEP inserted between them (infix) and RD is #f.
    If DENOM is true, RD is the last element of LS (denominator)
    and all elements are converted to markup.

*   (ekm-time-fraction FR ST)

    Return fraction FR as markup. FR must be a number or a pair or list
    of numbers. The last number is the denominator unless only one number
    or ST is 'single-digit.

*   (ekm-time-plain SIG)

    Return list SIG with sub-fractions replaced by simple fractions
    required for Timing properties.

See scm\time-signature-settings.scm



Dynamics
--------

    (dynamic (#t
      DYNAMIC-ENTRY
      ...
    )

*   DYNAMIC-ENTRY:

        (NAME . DYNAMIC)

*   NAME (string):
    "p", "m", "f", "r", "s", "z", "n", "mp", ...

*   DYNAMIC (CP):
    Absolute dynamic symbol.

Note: This identifier table is also used as a token table, but only
for the single-letter names.
`\ekm-dynamic` draws either a single symbol for the whole name
or a concatenation of symbols for each letter.
This is different from the usual interpretation of DEFINITION strings.

`\ekmParensHairpin` after lsr.di.unimi.it/LSR/Item?id=771.



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

    Staves  Height  Limit
    1       4.1     9
    2       13.1    18
    3       22.1    27
    4       31.1    36
    5       40.1    45
    6       49.1    54


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

Draw a top, bottom, and middle segment of the delimiter,
as well as intermediate vertical bars (fitting segments).
It assumes that the delimiter is vertically symmetric.

    H       Original height, after scale and stretch
    RH      Requested height
    ADD     Additional height = RH - H
    END     Height of the end (bottom, top) segments
    MID     Height of the middle segment
    REM     Height of the remaining parts = (H - MID)/2 - END
    FIT     Height of the fitting segments = (RH - MID)/2 - END

The parts which shall not be drawn are masked with a white rectangle.
This works under the condition RH >= H + 2 * END .

Note: Clipping is not supported in LilyPond. There is a potential
[solution](https://lists.gnu.org/archive/html/lilypond-user/2020-12/msg00112.html)
but it basically works only for postscript.

The following components are drawn:

1.  Delimiter for bottom segment

2.  Delimiter translated by ADD for top ssegment

3.  Mask from END to (RH - END)

4.  Delimiter translated by ADD/2 for middle segment

5.  Bottom mask from END to (RH - MID)/2,
    and top mask from (RH + MID)/2 to (RH - END)

6.  Bottom fitting from END to (RH - MID)/2,
    and top fitting from (RH + MID)/2 to (RH - END)

#### Figure

    |         Bottom    Top     Mask    Middle    Mask    Fitting   Result
    ---                    --                                           --   --- ---
    |                     //                                           //    END   |
    |                    --    -------           -------    --        --     ---   |
    |                    ||    |     |           |     |    ||        ||           |
    ADD                  ||    |     |       --  |     |    ||        ||           |
    |                    ||    |     |      //   |     |    ||        ||           |
    |                    --    |     |     --    |     |    ||        ||     FIT   |
    |                   //     |     |     ||    |     |    ||        ||           |
    --- ---        --  //      |     |     ||    |     |    ||        ||           |
    |   END       //  <<       |     |     ||    |     |    ||        ||           |
    |   ---      --    \\      |     |     --    -------    --        --     ---   |
    |            ||     \\     |     |    //                         //            |
    |   REM      ||      --    |     |   //                         //             |
    |            ||      ||    |     |  <<                         <<        MID  RH
    |   ---      --      ||    |     |   \\                         \\             |
    |           //       ||    |     |    \\                         \\            |
    |          //        --    |     |     --    -------    --        --     ---   |
    H   MID   <<          \\   |     |     ||    |     |    ||        ||           |
    |          \\          --  |     |     ||    |     |    ||        ||           |
    |           \\             |     |     ||    |     |    ||        ||           |
    |   ---      --            |     |     --    |     |    ||        ||     FIT   |
    |            ||            |     |      \\   |     |    ||        ||           |
    |   REM      ||            |     |       --  |     |    ||        ||           |
    |            ||            |     |           |     |    ||        ||           |
    |   ---      --            -------           -------    --        --     ---   |
    |   END       \\                                                   \\    END   |
    --- ---        --                                                   --   --- ---


### Table

    (delimiter
      (STYLE
        DELIMITER-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    brace, bracket

*   DELIMITER-ENTRY:

        (LIMIT SYM)
        (LIMIT SYM-LEFT . SYM-RIGHT)

*   LIMIT (number):
    The first entry with RH < LIMIT is selected.

*   SYM:

        (DELIMITER)
        (DELIMITER SCALE)
        (DELIMITER SCALE STRETCH)
        (DELIMITER SCALE STRETCH END MID LEFT RIGHT)
        (#f SCALE STRETCH BOTTOM TOP LEFT RIGHT)

*   DELIMITER (EXTEXT or #f):
    Delimiter symbol.
    `#f` draws the delimiter with BOTTOM and TOP without masks and
    without a middle segment. This is intended for brackets.

*   SCALE (number):
    Scale factor from EM to staff spaces. Default is 255/1000.

*   STRETCH (number or #f):
    Stretch factor.
    `#f` does not stretch (= 1). This is the default.

*   END (number):
    Relative height of the end segments of DELIMITER.

*   MID (number):
    Relative height of the middle segment of DELIMITER.

*   BOTTOM, TOP (EXTEXT):
    Symbols to draw as end segments if DELIMITER is `#f`.

*   LEFT, RIGHT (number or #f):
    X extent of the fitting segment.
    `#f` uses the `thickness` property whose default is 0.45 for brackets.



Multi-segment spanner
---------------------

    (spanner
      (STYLE
        SPANNER-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    trill, vibrato, wavy, ...

*   SPANNER-ENTRY:

        (text TEXT-LEFT . TEXT-RIGHT)
        (0 . SEGMENT-MAIN)
        (TEMPO SEGMENT-FASTER . SEGMENT-SLOWER)

*   TEXT-LEFT, TEXT-RIGHT (CP):
    Left and right symbol to be placed on each spanner piece.
    No entry equals `(text 0 . 0)`.

*   TEMPO (index):
    Absolute tempo.

*   SEGMENT-MAIN (CP):
    Main (medium) extender line segment for tempo 0.

*   SEGMENT-FASTER, SEGMENT-SLOWER (CP):
    Faster (narrower) and slower (wider) extender line segment for TEMPO.



Fingering
---------

    (finger
      (STYLE
        (NAME . SYM)
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, italic

*   NAME (string):
    "1", "2", "th", ...

*   SYM (EXTEXT):
    Fingering symbol.

The fingering symbols p, i, m, a, x are used by the `StrokeFinger` grob.



Piano pedals
------------

    (pedal (#t #t
      (TOKEN . SYM)
    ))

*   TOKEN (string):
    "Ped.", "Sost.", "*", ...

*   SYM (EXTEXT):
    Piano pedal symbol.



Harp pedals
-----------

    (harp (#t #t
      (TOKEN . SYM)
    ))

*   TOKEN (string):
    "^", "-", "v", ...

*   SYM (EXTEXT):
    Harp pedal symbol.



Ottavation
----------

    (ottava (#t #t
      (TOKEN . SYM)
      ...
    ))

*   TOKEN (string):
    "8", "8va", "15", "15ma", ...

*   SYM (EXTEXT):
    Ottavation symbol.


### Ottavation styles

    (ottavation
      (STYLE
        OTTAVATION-ENTRY
        ...
      )
      ...
    )

*   OTTAVATION-ENTRY:

        (OCTAVE . DEFINITION)
        (OCTAVE DEFINITION-DOWN . DEFINITION-UP)

*   OCTAVE (integer):
    Absolute octave number.

*   DEFINITION (string):
    Ottavation symbol defined as ottava tokens.



Tremolo marks
-------------

    (tremolo
      (STYLE
        TREMOLO-ENTRY
        ...
      )
    )

*   STYLE (symbol):
    beam-like, fingered

*   TREMOLO-ENTRY:

        (FLAGS . TREMOLO)

*   FLAGS (integer):
    Number of flags for subdivision, usually 1 to 5.

*   TREMOLO (CP):
    Tremolo symbol.



Stem decoration
---------------

    (stem (#t
      STEM-ENTRY
      ...
    )

*   STEM-ENTRY:

        (NAME . SYM)

*   NAME (string):
    "sprechgesang", "sussurando", "buzzroll", "unmeasured", ...

*   SYM (EXTEXT):
    Stem decoration symbol or tremolo mark.



Laissez vibrer
--------------

    (laissezvibrer
      (STYLE
        LV-ENTRY
        ...
      )
    )

*   STYLE (symbol):
    default (usually the only style)

*   LV-ENTRY:

        (LIMIT LV-DOWN . LV-UP)

*   LIMIT (number):
    The first entry with the specified size < LIMIT is selected.

*   LV (CP):
    Laissez vibrer symbol.



Accordion registers
-------------------

    (accordion
      (STYLE
        ACCORDION-ENTRY
        ...
      )
    )

*   STYLE (symbol):
    d (discant), sb (standard bass),
    sb4 (standard bass, four reed), sb5 (standard bass, five reed),
    sb6 (standard bass, six reed), fb (free bass), sq (square).

*   ACCORDION-ENTRY:

        (NAME . ACCREG)
        (NAME (ACCREG-EMPTY (DOT-X . DOT-Y) ...))

*   NAME (string):
    "1", "10", "1+0", "Soprano", "Alto", ...

*   ACCREG (CP):
    Accordion register symbol.

*   ACCREG-EMPTY (CP):
    Empty accordion register symbol.

*   DOT-X, DOT-Y (integer):
    Position of a dot in percent of the extent of ACCREG-EMPTY.

Note: The module `(scm accreg)` is not required.



Falls and doits
---------------

    (brass
      (STYLE
        BRASS-ENTRY
        ...
      )
    )

*   STYLE (symbol):
    bend, rough, smooth

*   BRASS-ENTRY:

        (LOG (BRASS-DOWN ALIGN) . (BRASS-UP ALIGN))

*   LOG (integer):
    Duration log in the range 0 to 2.

*   ALIGN (boolean):
    `#t` aligns the symbol up. Usually for rough and smooth down.

*   BRASS (EXTEXT):
    Fall or doit (lift) symbol.



Parentheses
-----------

    (parens
      (STYLE
        PARENS-ENTRY
        ...
      )
      ...
    )

*   STYLE (symbol):
    default, bracket, ...

*   PARENS-ENTRY:

        (NAME SYM-LEFT . SYM-RIGHT)

*   NAME (symbol):
    Intended scope of the parentheses.
    a (accidental), h (dynamics hairpin), t (normal text), ...

*   SYM:

        PARENS
        (#t Y-ALIGN SIZE PARENS)

*   PARENS (EXTEXT):
    Parenthesis symbol.

*   Y-ALIGN, SIZE (number):
    Transformation values applied to PARENS.



Figured bass
------------

### Digits

Number table with style `fbass`.


### Combining and precomposed symbols

    (fbass (#t
      (NAME . COMBINE)
      ...
    ))

*   NAME (string):
    "\\+", "\\\\", "/", "2\\+", ...

*   COMBINE (EXTEXT):
    Combining symbol or precomposed bass figure for augmented/diminished.


### Accidentals

    (fbass-acc (#t
      (ALTERATION . ACCIDENTAL)
      ...
    ))

*   ALTERATION (rational):
    0, 1/2, 1, 3/2, -1/2, ...

*   ACCIDENTAL (EXTEXT):
    Bass figure accidental.



Lyrics
------

    (lyric (#t
      (TOKEN . LYRIC)
      ...
    ))

*   TOKEN (char):
    ~ _ % ...

*   LYRIC (EXTEXT):
    Special lyric symbol.



Analytics
---------

    (analytics (#t #t
      (TOKEN . ANALYTICS)
      ...
    ))

*   TOKEN (string):
    "H", "CH", "N", ...

*   ANALYTICS (EXTEXT):
    Analytics symbol.



Function theory
---------------

    (func (#t #t
      (TOKEN . FUNC)
      ...
    ))

*   TOKEN (string):
    "D", "DD", "/D", "S", ...

*   FUNC (EXTEXT):
    Function theory symbol.

See [LSR/Item?id=967](https://lsr.di.unimi.it/LSR/Item?id=967)
for `\\ekmFunc`.



Arrows and arrow heads
----------------------

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

*   STYLE (symbol):
    black, white, open, ...

*   INDEX-TABLE:

        AR
        #(AR-N)
        #(AR-N AR-S)
        #(AR-N AR-E AR-S AR-W)
        #(AR-N AR-NE AR-E AR-SE AR-S AR-SW AR-W AR-NW)
        #(AR-N AR-NE AR-E AR-SE AR-S AR-SW AR-W AR-NW AR-NS AR-NESW AR-EW AR-SENW)

*   AR (CP):
    Arrow or arrow head.
    Single AR: CP + 0 to 7  ->  AR-N AR-NE AR-E ... AR-NW



Percussion Beaters
------------------

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

*   STYLE (symbol):
    xyl-soft, xyl-medium, timpani-soft, stick, ...

*   INDEX-TABLE:

        BTR
        #(BTR-N)
        #(BTR-N BTR-S)
        #(BTR-N BTR-S BTR-NE BTR-NW)

*   BTR (CP):
    Percussion beater.
    Single BTR: CP + 0 to 3  ->  BTR-N BTR-S BTR-NE BTR-NW



Augmentation dots
-----------------

    (dots
      (STYLE DOT PAD-3 PAD-4 PAD-5)
      ...
    )

*   STYLE (symbol):
    default, note, metronome, straight, short, beamed

*   DOT (EXTEXT):
    Augmentation dot symbol.

*   PAD-3, PAD-4, PAD-5 (number):
    Padding between a note of duration log 3, 4, 5, and the dot
    in units of the dot width. PAD-5 applies also to higher logs.

Used by `\note-by-number`.



Segno bar lines
---------------

    ekm-segno-tab '#(
      SYM-0
      ...
    )

*   SYM-I:

        (KERN-SCALE SEGNO)

    Segno bar line for the type I = 0 to 2.

*   KERN-SCALE (number):
    Scaling factor for the property `segno-kern`.

*   SEGNO (EXTEXT):
    Segno serpent symbol.

See scm/bar-line.scm



Orientation positions
---------------------

    ekm-orient-pos '#(
      OFFSET-0
      ...
    )

*   OFFSET-I (pair):
    Offset for the orientation index I = 0 to 7.

Used by `\ekm-label` for the label position.



Standard staff line positions
-----------------------------

    ekm-linepos-tab (
      (LINES POSITION ...)
      ...
    )

*   LINES (integer):
    Number of staff lines.

*   POSITION (integer):
    Standard line position in the range LINES - 1 to - (LINES - 1).

New entries are added as necessary by `(ekm-linepos)`.
Used by multi-measure rests.
