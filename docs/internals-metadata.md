Internals SMuFL metadata
========================

Provide font-specific metadata for use in Esmuflily.
This should include:

*   font name and version
*   engravingDefaults
*   stem attachment of noteheads
*   stem length of notes with flag



Tables
------

### Metadata table

A tree with all metadata for Esmuflily.

    (
      (fontName . "FONTNAME")
      (fontVersion . "FONTVERSION")
      (defaults
        (DEFKEY . DEFVALUE)
        ...
      )
      (glyphs
        (CP ...)
        ...
      )
    )

Created from the font-specific metadata JSON file.
Assumes that glyphs are given with their canonical glyph name,
not with the Unicode code point.

    {
      "fontName": "FONTNAME",
      "fontVersion": "FONTVERSION",
      "engravingDefaults": {
        "DEFKEY": DEFVALUE,
        ...
      },
      "glyphsWithAnchors": {
        "note...": {
          "stemDownNW": [ X, Y ],
          "stemUpSE": [ X, Y ]
        },
        ...
        "flag...": {
          "stemDownSW|stemUpNW": [ X, Y ]
        },
        ...
      },
      "optionalGlyphs": {
        "GLYPHNAME": {
          "codepoint": "U+HEX_CP",
          ...
        },
        ...
      },
      ...
    }


### Template metadata table

A tree similar to the final metadata table, but
without `fontName` and `fontVersion` (actually they are ignored),
and with GLYPHNAME instead of CP.

    (
      (defaults
        (DEFKEY . DEFVALUE)
        ...
      )
      (glyphs
        (GLYPHNAME ...)
        ...
      )
    )


### Glyphnames table

An alist of code points mapped onto glyph names (symbols).

    (
      (GLYPHNAME . CP)
      ...
    )

Created from "glyphnames.json" as it is provided by
[SMuFL](https://github.com/w3c/smufl).

    {
      "GLYPHNAME": {
        "codepoint": "U+HEX_CP",
        ...
      },
      ...
    }



Metadata location
-----------------

### JSON file

See SMuFL 3.11. Font-specific metadata locations
and "bravura-installer.iss".

    MD_DIR/FNAME.json

    MD_DIR        1.  PRIVATE_DIR
                  2.  USER_DIR/SMUFL_DIR
                  3.  SYS_DIR/SMUFL_DIR

    PRIVATE_DIR   from variable `ekmMetadata`

    USER_DIR      Linux:    $XDG_DATA_HOME
                  Windows:  %LOCALAPPDATA%
                  macOS:    ~/Library/Application Support

    SYS_DIR       Linux:    $XDG_DATA_DIRS
                  Windows:  %CommonProgramFiles%
                            %CommonProgramFiles(x86)%
                  macOS:    /Library/Application Support

    SMUFL_DIR     SMuFL/Fonts/FONTNAME

    FONTNAME      font name in normal spelling, eg. "Bravura"

    FNAME         font name in all lowercase, eg. "bravura",
                  though Bravura provides the file "bravura_metadata" (?)


### Scheme file

    EKMD_DIR/metadata-FNAME.scm

    EKMD_DIR    a LilyPond include directory

    FNAME       font name in all lowercase, eg. "bravura"



Process
-------

*   Read the command-line option `ekmfont`
    or the variable `ekmFont` and initialize `ekm:font-name`.
    The default is "Bravura" (currently "Ekmelos").

*   Read the command-line option `ekmmetadata`
    or the variable `ekmMetadata` for the metadata location PRIVATE_DIR.
    The default is #f.

*   If the metadata file "EKMD_DIR/metadata-FNAME.scm"
    for the selected font exists,
    load the metadata table from the file
    and set the variables `ekmd:defaults` and `ekmd:glyphs`
    to the values (sub-tables) of the keys `defaults` and `glyphs`.

*   Else create a new metadata table with the following steps.

*   Load the glyphnames table from the file
    "EKMD_DIR/glyphnames.scm".

*   Load the template metadata table from the file
    "EKMD_DIR/metadata-template.scm"
    and set the variables `ekmd:defaults` and `ekmd:glyphs` as above.

*   Parse the file "MD_DIR/FNAME.json".
    Select and store members according to the mask, in particular:

        fontName
        fontVersion
        engravingDefaults
        glyphsWithAnchors/note*
        optionalGlyphs/codepoint

*   Replace the values in `ekmd:defaults` and `ekmd:glyphs`,
    and add new elements according to the template
    for keys not defined in `glyphs` of the template metadata table.

*   Replace the glyph names in `ekmd:glyphs` with their code points
    defined in the glyphnames table and in `optionalGlyphs`.

*   Create a new metadata table from `ekm:font-name`, `ekm:font-version`,
    `ekmd:defaults`, and `ekmd:glyphs`.
    Save the table in the file "EKMD_DIR/metadata-FNAME.scm".
    EKMD_DIR is the same as for "metadata-template.scm".



JSON parser
-----------

See [JSON format](https://www.json.org/json-en.html).

See [guile-json](https://github.com/cthom06/guile-json),
Copyright (c) 2010, Corey Thomasson.

The parsed data are returned as a Scheme tree with the values:

    { "KEY" : VALUE ,... }    alist ( (KEY . VALUE) ... )
    [ VALUE ,... ]            list ( VALUE ... )
    "KEY"                     symbol KEY of unescaped key
    "STRING"                  unescaped string
    NUMBER                    integer or real number
    true                      #t
    false                     #f
    null                      '()

A `,` after the last object member or array element is silently ignored.

Note: The JSON files for SMuFL are simpler in some respect:

*   Key strings never have escapes.

*   The only escapes in value strings are `\uHHHH` and `\"`
    which appear in "description" values of "glyphnames.json".
    These values are ignored for esmuflily.

*   The values `true`, `false`, and `null` do not appear.



Mask
----

A tree with (KEY . VALUE) elements that specify
how to process object members of a JSON file.

KEY selects members:

*   `#t`: every member
*   SYMBOL: member with key SYMBOL
*   "STRING"  each member with a key with prefix STRING

VALUE specifies how to store members:

*   `#t`

        {... k: v ...}   ->   (... (k . v) ...)

    Stores the member as key-value pair.
    Used for "fontName" and "fontVersion".

*   `#\d`

        {... k: v ...}   ->   (... (k . v) ...)

    Like #t but stores the key-value pair in `ekmd:defaults`
    replacing the value for k or adding a new entry.
    Used for members of "engravingDefaults".

*   `#\c`

        {... k: v ...}   ->   v

    Stores v without its containing alist.
    A string with prefix "U+" is converted into a number.
    Assumes that the remaining string consists of hex-digits.
    Used for "codepoint" members of "optionalGlyphs"
    and in "glyphnames.json".

*   `#\01`, `#\02`, ...

        pk: {... k: v ...}        ->   (pk ... v ...)
        pk: {... k: [x, y] ...}   ->   (pk ... (x . y) ...)

    Stores v at index 1,2,... in the parent alist in `ekmd:glyphs`
    replacing the value for pk.
    Stores the first two elements of an array v in a pair.
    Used for "stemDown*" and "stemUp*" members of note heads and flags.



Template
--------

An alist with (KEY . VALUE) elements for keys not defined in `glyphs`
of the template metadata table.

KEY selects a template:

*   `#t`: for every key
*   SYMBOL: for key SYMBOL
*   "STRING"  for each key with prefix STRING

VALUE is the template for the new element.



Note heads
----------

    (glyphs
      (CP CONVERTED STEM_UP_ATTACH STEM_DOWN_ATTACH)
      ...
    )

*   CP (integer): Code point of the note head glyph.

*   CONVERTED (boolean): `#t` if the original metadata are converted.

*   STEM_UP_ATTACH, STEM_DOWN_ATTACH (pair):
    Either the original metadata from "stemUpSE" and "stemDownNW",
    or the converted values for the `stem-attachment` property.


### Convert metadata

        bBoxNE        --------------   o - - - - -
                     /      :       \
                    /       :        \           h-up
      stemUpSE     /        :         o  - - -
                  /         :          |    uy
             0   | - - - - -:- - - - - | - - - - -
                 |          :         /     dy
    stemDownNW    o         :        /   - - -
                   \        :       /            h-down
                    \       :      /
        bBoxSW   o   --------------      - - - - -
                 ::         :         ::
                 ::    dx   :   ux    ::
                 :                     :
                 :       w-roght       :

    x-ext = (0 . w-right)
    y-ext = (h-down . h-up)
    w = w-right / 2

    ux = (stemUpSE.x - w) / w = stemUpSE.x / w - 1
    uy = stemUpSE.y / h-up

    dx = (stemDownNW.x - w) / w = stemDownNW.x / w - 1
    dy = stemDownNW.y / abs(h-down)
