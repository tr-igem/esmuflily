Internals
=========

Descriptions and comments on some internals of [Esmuflily](https://github.com/tr-igem/esmuflily).
All font specific data in the tables are currently hard coded and
largely specific to [Ekmelos](https://github.com/tr-igem/ekmelos).



Table structues
---------------

### Style table

An alist of alists with musical symbols of several styles
and possibly of different directions (up/down or right/left).
Used by noteheads, note clusters, flags, rests, accordion registers,
falls and doits.

    ((STYLE
       ENTRY
       ...
     )
     ...
    )

*   STYLE (symbol): The style of one or more musical symbols.
    The first style is the default.

*   ENTRY: Specifed are the values returned by `ekm-asst` for Dir:

        _                   Dir:    #f                  >= 0    < 0
        (KEY . VALUE)               VALUE               VALUE   VALUE
        (KEY VALUE)                 (VALUE)             VALUE   VALUE
        (KEY VALUE-1 . VALUE-2)     (VALUE-1 . VALUE-2) VALUE-1 VALUE-2
        (KEY VALUE ...)             (VALUE ...)         VALUE   (...)       ;not used

*   KEY (symbol, number, string): Usually the duration log.
    The last key is the default.

*   VALUE: Musical symbol, possibly followed by metadata, e.g. the
    stem-attachment for a note head.

        EXTEXT
        (EXTEXT ...)

#### Access procedures

    (ekm-assq TABLE KEY)

Return the value in TABLE for KEY (symbol).

    (ekm-asst TABLE STYLE KEY DIR)

Return the value in TABLE for STYLE (symbol), KEY (symbol, number, string),
and DIR (number).
Expect TABLE being a single alist if STYLE is `#f`.
Return the entire TABLE if KEY is `#f`.
Return the entire value of the entry if DIR is `#f`.

    (ekm-assld TABLE GROB LOG DIR)

Return the value in TABLE for the GROB properties `style`, `duration-log`,
and `Stem.direction`, or for LOG or DIR if they have true values.
If GROB is not a grob it must itself be a style, and LOG and DIR must be
specific (true) values.


### Definition Table

An alist of musical symbols mapped onto definition keys.
One or more keys concatenated to a DEFINITION string draw
the corresponding musical symbols stacked in a line.
Used by symbols for dynamics, ottavation, fingering, piano pedals,
harp pedals, analytics, and function theory, and by `\\ekm-def`.

    ((DEF-KEY . VALUE) ...)

*   DEF-KEY (string):
    A key which is a prefix of other keys in the table must be arranged
    after them, i.e. the correct order is "abc", "ab", "a".
    Else the other keys will be ignored.

*   VALUE (EXTEXT or `#f`): Musical symbol.
    `#f` ignores DEF-KEY in DEFINITION strings.

#### Common keys

These keys are always applicable but can be overridden in a definition table.

    <space>     U+0020  space
    _           U+200A  hairspace
    __          U+2009  thinspace
    ___         U+2002  enspace
    ____        U+2003  emspace



Note heads
----------

    ekm-notehead-tab (
      (NOTEHEAD-STYLE NOTEHEAD-ENTRY ...)
      ...
    )

*   NOTEHEAD-STYLE (symbol): `default`, `harmonic`, `diamond`, `cross`,
    `triangle`, `square`, ...

*   NOTEHEAD-ENTRY:

        (LOG . CP)
        (LOG NOTEHEAD-DATA)
        (LOG NOTEHEAD-DATA-UP . NOTEHEAD-DATA-DOWN)

*   LOG (integer): Duration log, usually in the range -1 to 2.

*   NOTEHEAD-DATA:

        CP
        (CP CP-EMPTY STEM-ATTACH-X . STEM-ATTACH-Y)

*   CP (integer): Code point of the note head glyph.

*   CP-EMPTY (integer or `#f`): Code point of the glyph to whiteout
    the background. This is intended for note name note heads.

*   STEM-ATTACH-X, STEM-ATTACH-Y (number): Values for the
    `stem-attachment` property.



Note clusters
-------------

Implements Henry Cowell's clusters
(see lilypond.1069038.n5.nabble.com/Cowell-clusters-td237881.html)

    ekm-cluster-tab (
      (CLUSTER-STYLE CLUSTER-ENTRY ...)
      ...
    )

*   CLUSTER-STYLE (symbol): `default`, `harmonic`, `diamond`, `square`.

*   CLUSTER-ENTRY:

        (LOG CLUSTER-DATA)
        (LOG CLUSTER-DATA-UP . CLUSTER-DATA-DOWN)

*   LOG (integer): Duration log, usually in the range -1 to 2.

*   CLUSTER-DATA:

        (CP-1 CP-2 CP-3 CP-TOP CP-MID CP-BOTTOM)
        (CP-1 CP-2 CP-3 CP-TOP CP-MID CP-BOTTOM STEM-POS STEM-ATTACH-X . STEM-ATTACH-Y)

*   CP-1, CP-2, CP-3 (integer or `#f`): Code points of the note head glyphs
    for unison, second, and third.

*   CP-TOP, CP-MID, CP-BOTTOM (integer): Code points of the top, middle,
    and bottom segments for larger intervals.

*   STEM-POS (number): Added to the `stem-begin-position` property
    of stem down.

*   STEM-ATTACH-X, STEM-ATTACH-Y (number): Values for the
    `stem-attachment` property.



Flags
-----

    ekm-flag-tab (
      (FLAG-STYLE FLAG-ENTRY ...)
      ...
    )

*   FLAG-STYLE (symbol): `default`, `short`, `straight`.

*   FLAG-ENTRY:

        (LOG CP-UP . CP-DOWN)

*   LOG (integer): Duration log in the range 3 to 10.

*   CP (integer): Code point of the flag glyph.

###

    ekm-stemlength-tab (
      (FLAG-STYLE STEM-LENGTH EXTRA-LENGTH-3 ... EXTRA-LENGTH-10)
      ...
    )

*   STEM-LENGTH (number): Nominal unmodified stem length, usually 3.5.

*   EXTRA-LENGTH (number): Amount to lengthen stem for duration log 3 to 10.



Rests
-----

    ekm-rest-tab (
      (REST-STYLE REST-ENTRY ...)
      ...
    )

*   REST-STYLE (symbol): `default`, `classical`, `z`.

*   REST-ENTRY:

        (LOG . CP)
        (LOG CP . CP-LEDGERED)

*   LOG (integer): Duration log in the range -3 to 10.

*   CP (integer): Code point of the rest glyph.

*   CP-LEDGERED (integer): Code point of the rest glyph with ledger line.



System start delimiter
----------------------

The `brace` and `bracket` delimiters are currently supported.
See [SMuFL glyphs](https://w3c.github.io/smufl/latest/tables/staff-brackets-and-dividers.html).

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

####

              Bottom    Top     Mask    Middle    Mask    Fitting   Result
    ---                    --                                           --   --- ---
     |                    //                                           //    END  |
     |                   --    -------           -------    --        --     ---  |
     |                   ||    |     |           |     |    ||        ||      |   |
    ADD                  ||    |     |       --  |     |    ||        ||      |   |
     |                   ||    |     |      //   |     |    ||        ||      |   |
     |                   --    |     |     --    |     |    ||        ||     FIT  |
     |                  //     |     |     ||    |     |    ||        ||      |   |
    --- ---        --  //      |     |     ||    |     |    ||        ||      |   |
     |  END       //  <<       |     |     ||    |     |    ||        ||      |   |
     |  ---      --    \\      |     |     --    -------    --        --     ---  |
     |   |       ||     \\     |     |    //                         //       |   |
     |  REM      ||      --    |     |   //                         //        |   |
     |   |       ||      ||    |     |  <<                         <<        MID  RH
     |  ---      --      ||    |     |   \\                         \\        |   |
     |   |      //       ||    |     |    \\                         \\       |   |
     |   |     //        --    |     |     --    -------    --        --     ---  |
     H  MID   <<          \\   |     |     ||    |     |    ||        ||      |   |
     |   |     \\          --  |     |     ||    |     |    ||        ||      |   |
     |   |      \\             |     |     ||    |     |    ||        ||      |   |
     |  ---      --            |     |     --    |     |    ||        ||     FIT  |
     |   |       ||            |     |      \\   |     |    ||        ||      |   |
     |  REM      ||            |     |       --  |     |    ||        ||      |   |
     |   |       ||            |     |           |     |    ||        ||      |   |
     |  ---      --            -------           -------    --        --     ---  |
     |  END       \\                                                   \\    END  |
    --- ---        --                                                   --   --- ---


The table contains font specific values,
either for [Bravura](https://github.com/steinbergmedia/bravura)
or for [Ekmelos](https://github.com/tr-igem/ekmelos).

    ekm-system-start-tab (
      (DELIMITER-STYLE DELIMITER-ENTRY ...)
      ...
    )

*   DELIMITER-STYLE (symbol): `brace`, `bracket`.

*   DELIMITER-ENTRY:

        (LIMIT DELIMITER)
        (LIMIT DELIMITER SCALE)
        (LIMIT DELIMITER SCALE STRETCH)
        (LIMIT DELIMITER SCALE STRETCH END MID LEFT RIGHT)
        (LIMIT #f SCALE STRETCH BOTTOM TOP LEFT RIGHT)

*   LIMIT (number): The first entry with RH < LIMIT is selected.

*   DELIMITER (EXTEXT or `#f`): Symbol to draw.
    `#f` draws the delimiter with BOTTOM and TOP without masks and
    without a middle segment. This is intended for brackets.

*   SCALE (number): Scale factor from EM to staff spaces.
    Default is 255/1000.

*   STRETCH (number or `#f`): Stretch factor.
    `#f` does not stretch (= 1). This is the default.

*   END (number): Relative height of the end segments of DELIMITER.

*   MID (number): Relative height of the middle segment of DELIMITER.

*   BOTTOM, TOP (EXTEXT): Symbols to draw as end segments if DELIMITER is `#f`.

*   LEFT, RIGHT (number or `#f`): X extent of the fitting segment.
    `#f` uses the `thickness` property whose default is 0.45 for brackets.



Accordion registers
-------------------

Note: The module `(scm accreg)` is not required.

    ekm-accordion-tab (
      (REGISTER-TYPE REGISTER-ENTRY ...)
      ...
    )

*   REGISTER-TYPE (symbol): `d` (discant), `sb` (standard bass),
    `sb4` (standard bass, four reed), `sb5` (standard bass, five reed),
    `sb6` (standard bass, six reed), `fb` (free bass), `sq` (square).

*   REGISTER-ENTRY:

        (REGISTER-NAME . CP)
        (REGISTER-NAME CP-EMPTY (DOT-X . DOT-Y) ...)

*   REGISTER-NAME (string): `"1"`, `"10"`, `"11"`, `"1+0"`, ...
    `"Soprano"`, `"Alto"`, `"Tenor"`, `"Master"`, ...

*   CP (integer): Code point of the accordion register glyph.

*   CP-EMPTY (integer): Code point of the empty accordion register glyph.

*   DOT-X, DOT-Y (integer): Position of a dot in percent of the extent
    of CP-EMPTY.



Falls and doits
---------------

    ekm-brass-tab (
      (BRASS-STYLE BRASS-ENTRY ...)
      ...
    )

*   BRASS-STYLE (symbol): `bend`, `rough`, `smooth`.

*   BRASS-ENTRY:

        (LOG MINIMUM-LENGTH ALIGN-UP CP-DOWN CP-UP)

*   LOG (integer): Duration log in the range 0 to 2.

*   MINIMUM-LENGTH (number): Value for the `minimum-length` property.

*   ALIGN-UP (boolean): `#t` aligns up if the direction is upward.

*   CP (integer): Code point of the fall or doit (lift) glyph.



Parentheses
-----------

    ekm-parens-tab (
      (PARENS-STYLE PARENS-ENTRY ...)
      ...
    )

*   PARENS-STYLE (symbol): `default`, `bracket`, `brace`, `angle`.

*   PARENS-ENTRY:

        (TYPE LEFT . RIGHT)

*   TYPE (symbol): `a` (accidental), `h` (dynamics hairpin),
    `f` (function theory symbol), `t` (normal text).

*   LEFT, RIGHT (EXTEXT): Left and right parentheses.



Clefs
-----

    ekm-clef-tab (
      (CLEF-NAME CP . CP-CHANGE)
      ...
    )

*   CLEF-NAME (string): `"clefs.G"`, ... `"semipitched"`, ...
    Additional SMuFL clefs have no prefix `"clefs."`.

*   CP (integer): Code point of the clef glyph.

*   CP-CHANGE (integer or `#f`): Code point of the change clef glyph.
    `#f` draws CP with a 2 steps smaller font size.



Augmentation dots
-----------------

    ekm-dots-tab (
      (DOT-STYLE CP PAD-3 PAD-4 PAD-5)
      ...
    )

*   DOT-STYLE (symbol): `default`, `note`, `metronome`, `straight`,
    `short`, `beamed`.

*   CP (integer): Code point of the dot glyph.

*   PAD-3, PAD-4, PAD-5 (number): Padding between a note of duration log 3,4,5
    and the dot in units of the dot width. PAD-5 applies also to higher logs.

This is used only by `\\note-by-number`.



Dynamics
--------

    ekm-dynamic-tab (
      (DYNAMIC-NAME . CP)
      ...
    )

*   DYNAMIC-NAME (string): `"p"`, `"m"`, `"f"`, `"r"`, `"s"`, `"z"`, `"n"`,
    `"pp"`, `"mp"`, ...

*   CP (integer): Code point of the absolute dynamic glyph.

Note: The interpretation of names for `\\ekm-dynamic` is slightly
different from other DEFINITION strings.
A dynamics symbol is either a single glyph or a sequence of glyphs
for each letter, but not for keys of two or more letters.



Scripts - Expressive marks
--------------------------

    ekm-script-tab (
      SCRIPT-ENTRY
      ...
    )

*   SCRIPT-ENTRY:

        (SCRIPT-NAME SCRIPT)
        (SCRIPT-NAME SCRIPT-UP . SCRIPT-DOWN)

*   SCRIPT-NAME (string): `"sforzato"`, `"dmarcato"`, `"trill"`, `"turn"`, `"dfermata"`, ...
    This is the car of the `script-stencil` property.

*   SCRIPT (EXTEXT): Expressive mark.



Multi-segment spanner
---------------------

    ekm-spanner-tab (
      (SPANNER-STYLE SPANNER-ENTRY ...)
      ...
    )

*   SPANNER-STYLE (symbol): `trill`, `vibrato`, ..., `circular`, `circular-constant`,
    `wavy`, `square`, `sawtooth`, `beam`.

*   SPANNER-ENTRY:

        (text CP-LEFT . CP-RIGHT)
        (0 . CP-MAIN)
        (N CP-FASTER-N . CP-SLOWER-N)

*   CP-LEFT, CP-RIGHT (integer): Code point of the left and right symbol
    to be placed on each spanner piece. 0 draws a point-stencil.

*   CP-MAIN (integer): Code point of the main (medium) extender line segment.

*   CP-FASTER-N, CP-SLOWER-N (integer): Code point of the N-th faster (narrower)
    and slower (wider) extender line segment.



Stem symbols and Tremolos
-------------------------

    ekm-stem-tab (
      (STEMSYMBOL-NAME . STEMSYMBOL)
      ...
    )

    ekm-tremolo-tab (
      (STEMSYMBOL-NAME . STEMSYMBOL)
      ...
    )

*   STEMSYMBOL-NAME (string): `"sprechgesang"`, `"sussurando"`, ...
    `"buzzroll"`, `"penderecki"`, ...

*   STEMSYMBOL (EXTEXT): Stem symbol or tremolo mark.



Ottavation
----------

    ekm-ottavation-tab (
      (OTTAVATION-KEY . CP)
      ...
    )

*   OTTAVATION-KEY (string): `"8"`, `"8va"`, `"15"`, `"15ma"`, ...

*   CP (integer): Code point of the ottavation glyph.



Fingering
---------

    ekm-finger-tab (
      (FINGERING-KEY . CP)
      ...
    )

*   FINGERING-KEY (string): `"1"`, `"2"`, ... `"th"`, `"p"`, ...

*   CP (integer): Code point of the fingering glyph.

Note: The fingering symbols p, i, m, a, and x are used by the
`StrokeFinger` grob.



Piano pedals
------------

    ekm-pedal-tab (
      (PEDAL-KEY . PEDAL)
      ...
    )

*   PEDAL-KEY (string): `"Ped."`, `"Sost."`, `"*"`, ...

*   PEDAL (EXTEXT): Piano pedal.



Harp pedals
-----------

    ekm-harp-pedal-tab (
      (HARPPEDAL-KEY . HARPPEDAL)
      ...
    )

*   HARPPEDAL-KEY (string): `"^"`, `"-"`, `"v"`, ...

*   HARPPEDAL (EXTEXT): Harp pedal.



Figured bass
------------

    ekm-fbass-acc (
      (ALTERATION . CP)
      ...
    )

*   ALTERATION (rational): 0, ±1/2, ±1, ±3/2.

*   CP (integer): Code point of the bass figure accidental.

###

    ekm-fbass-pre (
      (PRECOMP-FLAG . CP)
      ...
    )

*   PRECOMP-FLAG (integer): Value of the bass figure digit + flag for the
    combining glyph: #x100 (plus `+`), #x200 (raising `/`), #x400 (lowering `\\`).

*   CP (integer): Code point of the precomposed bass figure glyph.



Analytics
---------

    ekm-analytics-tab (
      (ANALYTICS-KEY . ANALYTICS)
      ...
    )

*   ANALYTICS-KEY (string): `"H"`, `"CH"`, `"N"`, ...

*   ANALYTICS (EXTEXT): Analytics symbol.



Function theory
---------------

See [LSR/Item?id=967](https://lsr.di.unimi.it/LSR/Item?id=967) for `\\ekmFunc`.

    ekm-func-tab (
      (FUNC-KEY . FUNC)
      ...
    )

*   FUNC-KEY (string): `"0"`, `"1"`, ... `"D"`, `"DD"`, `"/D"`, `"S"`, ...

*   FUNC (EXTEXT): Function theory symbol.



Arrows and arrow heads
----------------------

    ekm-arrow-tab (
      (ARROW-STYLE . ARROW-DATA)
      ...
    )

*   ARROW-STYLE (symbol): `black`, `white`, `open`, `simple`, `double`, ...

*   ARROW-DATA (vector):

        #(CP-N CP-E CP-S CP-W)
        #(CP-N CP-NE CP-E CP-SE CP-S CP-SW CP-W CP-NW)
        #(CP-N CP-NE CP-E CP-SE CP-S CP-SW CP-W CP-NW CP-NS CP-NESW CP-EW CP-SENW)

*   CP (integer): Code point of an arrow glyph.
    Except for `simple`, CP-NESW and CP-SENW are either = CP-N or not present.



Percussion symbols - Beaters
----------------------------

    ekm-beater-tab (
      (BEATER-STYLE BEATER-PREDEF BEATER-ENTRY ...)
      ...
    )

*   BEATER-STYLE (symbol): `xyl`, `glsp`, `timpani`, `yarn`, ...

*   BEATER-PREDEF (boolean): Flag for predefined orientations:
    `#t`: N, S, NE, NW. `#f`: N, S.
    The remaining orientations are achieved by flipping or by rotating
    through 90 or 30 degrees.

*   BEATER-ENTRY:

        (BEATER-TYPE . CP-N)
        (BEATER-TYPE . #(CP-N CP-S))
        (BEATER-TYPE . #(CP-N CP-S CP-NE CP-NW))

*   BEATER-TYPE (symbol): `soft`, `medium`, `hard`, `wood`, ...

*   CP (integer): Code point of a beater glyph.
    CP < 0 draws (abs CP) flipped.
    For a single CP-N, the other predefined glyphs have consecutive code points.


### Beater orientation

A vector with eight entries to obtain the beater orientation for the
orientation index 0 to 7.

    ekm-beater-dir #(
      (CP-OFFSET-4 XFORM-4 CP-OFFSET-2 XFORM-2)
      ...
    )

*   CP-OFFSET-4, CP-OFFSET-2 (integer): Offset to the code point of a beater glyph
    where 4 (N, S, NE, NW) or 2 (N, S) predefined orientations exist.

*   XFORM-4, XFORM-2 (boolean or number):
    `#f` does not transform. `#t` flips. Number is a rotation angle.



Standard staff line positions
-----------------------------

    ekm-linepos-tab (
      (LINE-COUNT LINE-POSITION ...)
      ...
    )

*   LINE-COUNT (integer): Number of staff lines.

*   LINE-POSITION (integer): Standard line position in the range
    LINE-COUNT - 1, LINE-COUNT - 3,..., - (LINE-COUNT - 1)

New entries are added if needed.
The table is currently used only for multi-measure rests.
