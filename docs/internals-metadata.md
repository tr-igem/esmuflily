Internals for Metadata
======================

Description of the maintenance of metadata in [Esmuflily](https://github.com/tr-igem/esmuflily)
provided by a [SMuFL](https://github.com/w3c/smufl) compliant,
font-specific [JSON](https://www.json.org/json-en.html) file.


Locations
---------

### JSON file

See SMuFL 3.11. Font-specific metadata locations
and "bravura-installer.iss".

-   MD_LOC/MD_NAME

-   MD_LOC

    1.  PRIVATE_LOC
    2.  USER_LOC/SMuFL/Fonts/FONT
    3.  SYSTEM_LOC/SMuFL/Fonts/FONT

-   PRIVATE_LOC \
    From variable `ekmMetadata`

-   USER_LOC

    - Linux:    `$XDG_DATA_HOME`
    - Windows:  `%LOCALAPPDATA%`
    - macOS:    `~/Library/Application Support`

-   SYSTEM_LOC

    - Linux:    `$XDG_DATA_DIRS`
    - Windows:  `%CommonProgramFiles%`, `%CommonProgramFiles(x86)%`
    - macOS:    `/Library/Application Support`

-   MD_NAME

    1.  FNAME_metadata.json
    2.  FNAME.json
    3.  metadata.json

-   FONT \
    Font name in normal spelling, eg. "Bravura"

-   FNAME \
    Font name in all lowercase, eg. "bravura"


### Scheme files

-   EKMD_LOC/ekmd-FNAME.scm \
    Metadata table (cache file)

-   EKMD_LOC/ekmd-template.scm \
    Template metadata table

-   EKMD_LOC/types-FNAME.scm \
    Types table

-   EKMD_LOC/types-template.scm \
    Template types table

-   EKMD_LOC

    1.  a LilyPond include directory
    2.  MD_LOC


Tables
------

### JSON file

Glyphs must be given with their canonical glyph name, not with
the Unicode code point.

Metadata of interest for Esmuflily:

```json
{
  "fontName": "FONT",
  "fontVersion": "VERSION",
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
```


### Metadata table

A tree with all metadata for Esmuflily, created from the JSON file
and stored in a font-specific cache file "ekmd-FNAME.scm".
The types sub-table is optional.

```scheme
(
  (fontName . "FONT")
  (fontVersion . "VERSION")
  (defaults
    (DEFKEY . DEFVALUE)
    ...
  )
  (glyphs
    (CP ...)
    ...
  )
  (types
    ...
  )
)
```


### Template metadata table

A metadata table, with the sub-tables `defaults` and `glyphs`
(other sub-tables are actually ignored), and with glyph names
instead of code points, stored in the file "ekmd-template.scm".

```scheme
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
```


### Glyphnames table

An alist of code points mapped onto glyph names (symbols),
created from "glyphnames.json" provided by SMuFL, and defined as
`ekmd:glyphnames` in the file "ekmd.scm".

```scheme
(
  (GLYPHNAME . CP)
  ...
)
```


### Types table

A tree with data for Esmuflily additional to the metadata.
See [internals.md](./internals.md).

The types table in a font-specific file "types-FNAME.scm" or in
"types-template.scm" will be included into a newly created metadata table.


Process flow
------------

1.  Read the command-line option `ekmfont` or the variable `ekmFont`
    and initialize `ekm:font-name`.
    The default is "Ekmelos" (should be changed to "Bravura").

2.  Read the command-line option `ekmmetadata` or the variable `ekmMetadata`
    for the metadata location PRIVATE_LOC.
    The default is #f.

3.  If the cache file "ekmd-FNAME.scm" exists, load it and set the
    variables `ekmd:defaults` and `ekmd:glyphs` to the sub-tables
    of the keys `defaults` and `glyphs`, and merge the sub-table `types`
    into the table `ekm:types`.

    Else create a new metadata table with the following steps.

4.  Load the template metadata table from the file "ekmd-template.scm"
    and set the variables `ekmd:defaults` and `ekmd:glyphs` as above.

5.  Load the types table from the file "types-FNAME.scm" or "types-template.scm".

6.  Parse the JSON file. Select and store members of interest for Esmuflily.

7.  Replace the values in `ekmd:defaults` and `ekmd:glyphs`,
    and add new elements for keys not defined in `glyphs` in the
    template metadata table.

8.  Replace the glyph names in `ekmd:glyphs` with their code points
    defined in `ekmd:glyphnames` and in `optionalGlyphs` in the JSON file.

9.  Create a new metadata table from `ekm:font-name`, `ekm:font-version`,
    `ekmd:defaults`, `ekmd:glyphs`, and the types table.
    Save the table in the cache file "ekmd-FNAME.scm" at the same
    location as the types table or the template metadata table if
    there is no types table.


JSON parser
-----------

The parsed data are returned as a Scheme tree with the values:

| JSON                      | Scheme                        |
|---------------------------|-------------------------------|
| `{ "KEY" : VALUE ,... }`  | alist `( (KEY . VALUE) ... )`
| `[ VALUE ,... ]`          | list `( VALUE ... )`
| "KEY"                     | symbol KEY of unescaped key
| "STRING"                  | unescaped string
| NUMBER                    | integer or real number
| `true`                    | `#t`
| `false`                   | `#f`
| `null`                    | `'()`

A `,` after the last object member or array element is silently ignored.

Note: The JSON files for SMuFL are simpler in some respect:

-   Key strings never have escapes.

-   The only escapes in value strings are `\uHHHH` and `\"`
    which appear in "description" values of "glyphnames.json".
    These values are ignored for Esmuflily.

-   The values `true`, `false`, and `null` do not appear.


### Mask

A tree with (KEY . VALUE) elements that specify how to process
object members of a JSON file.

KEY selects members:

-   `#t`: Each member
-   SYMBOL: Member with key SYMBOL
-   "STRING": Each member with a key with prefix STRING

VALUE specifies how to store members:

-   `#t` \
    Stores the member as key-value pair.
    Used for "fontName" and "fontVersion".
    ```
    {... "KEY": VALUE ...}
    ⇒
    (... (KEY . VALUE) ...)
    ```

-   `#\d` \
    Like `#t` but stores the key-value pair in `ekmd:defaults`
    replacing the current value or adding a new entry.
    Used for members of "engravingDefaults".

-   `#\c` \
    Stores the value without key and without containing alist.
    A string with prefix "U+" is converted into a number.
    Assumes that the remaining string consists of hex-digits.
    Used for "codepoint" members of "optionalGlyphs"
    and in "glyphnames.json".

-   `#\01`, `#\02`, ... \
    Stores the value at index 1,2,... in the parent alist
    into `ekmd:glyphs` replacing the current value.
    Stores the first two elements of an array v in a pair.
    Used for "stemDown*" and "stemUp*" members of note heads and flags.
    ```
    "PARENT-KEY": {... "KEY": VALUE ...}
    ⇒
    (PARENT-KEY ... VALUE ...)

    "PARENT-KEY": {... "KEY": [X, Y] ...}
    ⇒
    (PARENT-KEY ... (X . Y) ...)
    ```


### Template

An alist with (KEY . VALUE) elements for keys not defined in `glyphs`
in the template metadata table.

KEY selects a template:

-   `#t`: Each key
-   SYMBOL: Key SYMBOL
-   "STRING": Each key with prefix STRING

VALUE is the template for the new element.
