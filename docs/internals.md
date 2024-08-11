Internals
=========

Descriptions and comments on some internals of [Esmuflily](https://github.com/tr-igem/esmuflily).



Table structues
---------------

Several musical symbols (noteheads, note clusters, flags, rests,
falls and doits, etc.) are defined in tables which are accessed by
`(ekm-asst Table Style Key Dir)`.
The table is either an alist of alists for different styles

    ((STYLE
       ENTRY
       ...
     )
     ...
    )

or a single alist if there are no styles (Style = #f).

*   STYLE (symbol): The style of one or more musical symbols.
    The first style is the default.

*   ENTRY (pair): May contain separate values for directed musical symbols.
    `(ekm-asst)` returns the value corresponding to Dir.

        Dir                     0                   UP      DOWN
        (KEY . V)               V                   V       V
        (KEY V)                 (V)                 V       V
        (KEY V-UP . V-DOWN)     (V-UP . V-DOWN)     V-UP    V-DOWN
        (KEY V ...)             (V ...)             V       (...)       (not used)

*   KEY (symbol, number, or string): Usually the duration log.
    The last key is the default.

*   V (EXTEXT): Definition of the musical symbols.



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

Select the glyph that is intended for the requested height.
This enables optical sizing.
The available glyphs are mapped onto height limits.
GLYPH-1 is selected for heights less than LIMMIT-1, etc.

    (LIMIT-1 GLYPH-1)
    (LIMIT-2 GLYPH-2)
    ...
    (+inf.0 GLYPH-N)

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

Set the font size to scale the glyph proportionally.
The original glyph has either the height for a five-line staff (1em)
or a height close to (or equal to) the requested height.


### Stretch

Scale the stencil of the glyph vertically.
The width remains unchanged.


### Lengthen

This is a variant of the "large-item hack" proposed by
[Daniel Benjamin Miller](https://github.com/dbenjaminmiller).

Draw a top, bottom, and middle segment of the glyph,
as well as intermediate vertical bars (fitting segments).
It assumes that the glyph is vertically symmetric.

    H       Height of the original glyph
    RH      Requested height
    ADD     Additional height = RH - H
    END     Height of the end (bottom, top) segments
    MID     Height of the middle segment
    REM     Height of the remaining parts = (H - MID)/2 - END
    FIT     Height of the fitting segments = (RH - MID)/2 - END

The parts of the glyph which shall not be drawn are masked
with a white rectangle.
This works under the condition RH >= H + 2 * END .

Note: Clipping of a glyph (stencil) is not supported in LilyPond.
There is a potential [solution](https://lists.gnu.org/archive/html/lilypond-user/2020-12/msg00112.html)
but it basically works only for postscript.

The following parts are drawn:

1.  Glyph for bottom segment

2.  Glyph translated by ADD for top ssegment

3.  Mask from END to (RH - END)

4.  Glyph translated by ADD/2 for middle segment

5.  Bottom mask from END to (RH - MID)/2,
    and top mask from (RH + MID)/2 to (RH - END)

6.  Bottom fitting from END to (RH - MID)/2,
    and top fitting from (RH + MID)/2 to (RH - END)

####

              Bottom    Top     Mask    Middle    Mask   Fitting   Result
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

### Table

The table `ekm-system-start-tab` currently contains hard coded
font specific values for [Bravura](https://github.com/steinbergmedia/bravura)
or [Ekmelos](https://github.com/tr-igem/ekmelos).

    ((STYLE
       DELIMITER-ENTRY
       ...
     )
     ...
    )

*   STYLE (symbol):

        brace
        bracket

*   DELIMITER-ENTRY (list):

        (LIMIT GLYPH)
        (LIMIT GLYPH SCALE)
        (LIMIT GLYPH SCALE STRETCH)
        (LIMIT GLYPH SCALE STRETCH END MID LEFT RIGHT)
        (LIMIT #f SCALE STRETCH BOTTOM TOP LEFT RIGHT)

*   LIMIT (number): The first entry with RH < LIMIT is selected.

*   GLYPH (EXTEXT or #f): Glyph to draw.
    `#f` draws the delimiter with BOTTOM and TOP without masks and
    without a middle segment. This is intended for brackets.

*   SCALE (number): Scale factor from EM to staff spaces.
    Default is 255/1000.

*   STRETCH (number or #f): Stretch factor.
    `#f` performs no stretching (= 1). This is the default.

*   END (number): Relative height of the end segments of GLYPH.

*   MID (number): Relative height of the middle segment of GLYPH.

*   BOTTOM, TOP (EXTEXT): Glyphs to draw as end segments if GLYPH is `#f`.

*   LEFT, RIGHT (number): X extent of the fitting segment.
