Internals Metadata
==================

Description of the maintenance of metadata in [Esmuflily](https://github.com/tr-igem/esmuflily)
provided by a [SMuFL](https://github.com/w3c/smufl) compliant,
font-specific [JSON](https://www.json.org/json-en.html) file.



Metadata location
-----------------

### JSON file

See SMuFL 3.11. Font-specific metadata locations
and "bravura-installer.iss".

    MD_DIR/MD_NAME

    MD_DIR        1.  PRIVATE_DIR
                  2.  USER_DIR/SMUFL_DIR
                  3.  SYSTEM_DIR/SMUFL_DIR

    PRIVATE_DIR   from variable `ekmMetadata`

    USER_DIR      Linux:    $XDG_DATA_HOME
                  Windows:  %LOCALAPPDATA%
                  macOS:    ~/Library/Application Support

    SYSTEM_DIR    Linux:    $XDG_DATA_DIRS
                  Windows:  %CommonProgramFiles%
                            %CommonProgramFiles(x86)%
                  macOS:    /Library/Application Support

    SMUFL_DIR     SMuFL/Fonts/FONTNAME

    MD_NAME       1.  FNAME_metadata.json
                  2.  FNAME.json
                  3.  metadata.json

    FONTNAME      font name in normal spelling, eg. "Bravura"

    FNAME         font name in all lowercase, eg. "bravura"


### Cache file

    EKMD_DIR/ekmd-FNAME.scm
    EKMD_DIR/ekmd-template.scm

    EKMD_DIR      a LilyPond include directory



Metadata tables
---------------

### JSON file

Glyphs must be given with their canonical glyph name, not with
the Unicode code point.

Metadata of interest for Esmuflily:

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


### Metadata table

A tree with all metadata for Esmuflily, created from the JSON file
and stored in a font-specific cache file "ekmd-FNAME.scm".

The optional sub-table `types` is merged into the table `ekm:types`.
It must be added manually to the metadata. It's data cannot be derived
from the JSON file. See "internals.md".

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
      (types
        TYPE-ENTRY
        ...
      )
    )


### Template metadata table

A metadata table, with the sub-tables `defaults` and `glyphs`
(other sub-tables are actually ignored), and with glyph names
instead of code points, stored in the file "ekmd-template.scm".

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

An alist of code points mapped onto glyph names (symbols),
created from "glyphnames.json" provided by SMuFL, and defined as
`ekmd:glyphnames` in the file "ekmd.scm".

    (
      (GLYPHNAME . CP)
      ...
    )



Process
-------

*   Read the command-line option `ekmfont`
    or the variable `ekmFont` and initialize `ekm:font-name`.
    The default is "Ekmelos" (should be changed to "Bravura").

*   Read the command-line option `ekmmetadata`
    or the variable `ekmMetadata` for the metadata location PRIVATE_DIR.
    The default is #f.

*   If the cache file "ekmd-FNAME.scm" exists, load it and set the
    variables `ekmd:defaults` and `ekmd:glyphs` to the sub-tables
    of the keys `defaults` and `glyphs`, and merge the sub-table `types`
    into the table `ekm:types`.

*   Else create a new metadata table with the following steps.

*   Load the template metadata table from the file "ekmd-template.scm"
    and set the variables `ekmd:defaults` and `ekmd:glyphs` as above.

*   Parse the JSON file. Select and store members of interest for Esmuflily.

*   Replace the values in `ekmd:defaults` and `ekmd:glyphs`,
    and add new elements for keys not defined in `glyphs` in the
    template metadata table.

*   Replace the glyph names in `ekmd:glyphs` with their code points
    defined in `ekmd:glyphnames` and in `optionalGlyphs` in the JSON file.

*   Create a new metadata table from `ekm:font-name`, `ekm:font-version`,
    `ekmd:defaults`, and `ekmd:glyphs`.
    Save the table in the cache file "ekmd-FNAME.scm".



JSON parser
-----------

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
    These values are ignored for Esmuflily.

*   The values `true`, `false`, and `null` do not appear.


### Mask

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


### Template

An alist with (KEY . VALUE) elements for keys not defined in `glyphs`
in the template metadata table.

KEY selects a template:

*   `#t`: for every key
*   SYMBOL: for key SYMBOL
*   "STRING"  for each key with prefix STRING

VALUE is the template for the new element.
