%%
%% Read SMuFL metadata
%%
%% Reads a JSON file ("metadata.json"),
%% selects the data specified in RO (`ekmd-ro`),
%% and saves the selected data in a cache file as Scheme alists.
%%
%%
%% Assumptions:
%%
%% * WS is (#\space #\tab #\newline) but not #\return.
%%   The file is read in text mode.
%% * Key strings never have escapes.
%% * Value srings never have escapes in "metadata.json".
%%
%%
%% Notes:
%%
%% The only escapes in SMuFL metadata strings are `\uHHHH` and `\"`.
%% They appear in "glyphnames.json" and "glyphdata.json"
%% in "description" values.
%% `\"` appears only for one glyph:
%%  "stockhausenTremolo": {
%%    "description": "Stockhausen irregular tremolo (\"Morsen\", like Morse code)"
%%    ...
%%  }
%%
%% The values (words) `true`, `false`, and `null`
%% do not appear in SMuFL metatata.
%% The value `true` appears only in "glyphdata.json".
%%
%%
%% Basic concepts:
%%
%% RO: List of requested objects.
%%  The first element is a RM.
%%  The remainder is an alist where for each entry (k . v):
%%  k is the key of a requested object at the current level.
%%  v is one of:
%%    List whose first element is:
%%      RO of the next level (direct child objects).
%%      Number: Codepoint and stem attachment data of glyphname k.
%%    #t: Save each member of the current object in ekmd-table.
%%    #f: Ignore the object (useless since no entry for an object k
%%      means it is not requested).
%%
%% RM: List of requested members (intended for non-object members).
%%  An alist where for each entry (k . v):
%%  k is the prefix of keys of requested members of the current object
%%    (possibly also of child objects ?).
%%  v is one of:
%%    #t: Save the value of the member in ekmd-table.
%%    #f: Ignore the member.
%%      This is useful only if a member "foobar" shall be ignored,
%%      while other members with prefix "foo" are requested.
%%    Number: Index into the list value for a requested object
%%      at the current level.
%%      This is intended for stem attachment data of glyphname objects
%%      where the index is 1 or 3.
%%
%%
%% JSON format
%% https://www.json.org/json-en.html
%%

\version "2.24.0"

%% for read-delimited, read-string
#(use-modules (ice-9 rdelim))


% metadataName = "/Ekmelik/Software/Esmuflily/test/data.json"
metadataName = "/Ekmelik/Software/Ekmelos/metadata/metadata.json"

metadataCache = "/Ekmelik/Software/Esmuflily/ly/metadata.ly"


#(define ekmd-ro '(
  (("font" . #t))
  (""
    ()
    ("engravingDefaults" . #t)
    ("glyphsWithAnchors"
      (("stemDown" . 1)
       ("stemUp" . 3))
      ;
      ("noteheadBlack" #xE0A4 "downX" "downY" "upX" "upY")
      ;("noteheadHalf" #xE0A3 "downX" "downY" "upX" "upY")
      ("flag8thUp" #xE240 "downX" "downY" "upX" "upY")
      ("flag8thDown" #xE241 "downX" "downY" "upX" "upY")
      ;("flag16thUp" #xE242 0 0 0.0 -0.22)
      ;("flag16thDown" #xE243 0.0 0.196 0 0)
      ;("flag32ndUp" #xE244 0 0 0.0 0.532)
      ;("flag32ndDown" #xE245 0.0 -0.552 0 0)
      ;("flag64thUp" #xE246 0 0 0.0 1.348)
      ;("flag64thDown" #xE247 0.0 -1.304 0 0)
      ;("flag128thUp" #xE248 0 0 0.0 2.3)
      ;("flag128thDown" #xE249 0.0 -2.052 0 0)
      ;("flag256thUp" #xE24A 0 0 0.0 3.048)
      ;("flag256thDown" #xE24B 0.0 -2.804 0 0)
      ;("flag512thUp" #xE24C 0 0 0.0 3.804)
      ;("flag512thDown" #xE24D 0.0 -3.556 0 0)
      ;("flag1024thUp" #xE24E 0 0 0.0 4.56)
      ;("flag1024thDown" #xE24F 0.0 -4.316 0 0)
    )
  )))


%% empty RO if none is given for next level in ekmd-obj
#(define ekmd-noro '(()))


%% table of general metadata
%% "fontName", "fontVersion", and "engravingDefaults" members
#(define ekmd-table '())


#(define ekmd-cs:dig (string->char-set "-0123456789"))
#(define ekmd-cs:num (string->char-set "0123456789.-+Ee"))

#(define (ekmd-error t)
  (ly:error "JSON error: ~a" t)
  '())

#(define (ekmd-next p)
  (let ((c (read-char p)))
    (case c
      ((#\sp #\ht #\lf) (ekmd-next p))
      (else c))))

#(define (ekmd-word p w v)
  (let ((s (read-string p (string-length w))))
    (if (string=? w s) v (ekmd-error "illegal word"))))


%% ro: RO at current level
%% key: Key at current object
%% m: RM of ro
%% a: Value of key in ro if key is requested, else #f
%% r: RO of next level (= a) if it is a pair,
%%    else '(()) (possibly (list m) to pass m to child objects ?)
%%
#(define (ekmd-obj p ro key)
  (let* ((m (car ro))
         (a (assoc-ref (cdr ro) key))
         (r (if (and (pair? a) (not (number? (car a)))) a ekmd-noro)))
    ;(ly:warning "<~a>   ~a" key m)
    (let obj ((o #t)) ;; o unused
      (case (ekmd-next p)
        ((#\,)
          (obj o))
        ((#\x22)
          (let* ((k (read-delimited "\x22" p))
                 (v (if (char=? #\: (ekmd-next p)) (ekmd-val p r k) #f))
                 (i (if (boolean? a) a
                      (find (lambda (e) (string-prefix? (car e) k)) m)))
                 (i (if (pair? i) (cdr i) i)))
            (if (number? i)
              (if (pair? v)
                (begin
                  (list-set! a i (car v))
                  (if (pair? (cdr v)) (list-set! a (1+ i) (cadr v)))))
              (if i (set! ekmd-table (acons k v ekmd-table))))
            (obj o)))
        ((#\})
          o)
        (else
          (ekmd-error "open object") #f)))))

#(define (ekmd-val p ro key)
  (let ((c (ekmd-next p)))
    (case c
      ((#\{)
        (ekmd-obj p ro key))
      ((#\[)
        (let lst ((l '()))
          (let ((c (ekmd-next p)))
            (case c
              ((#\,)
                (lst l))
              ((#\])
                (reverse l))
              (else
                (unread-char c p)
                (lst (cons* (ekmd-val p ro key) l)))))))
      ((#\x22)
        (read-delimited "\x22\\" p))
      ((#\t)
        (ekmd-word p "rue" #t))
      ((#\f)
        (ekmd-word p "alse" #f))
      ((#\n)
        (ekmd-word p "ull" '()))
      (else
        (if (char-set-contains? ekmd-cs:dig c)
          (string->number
            (reverse-list->string
              (let num ((n (list c)) (d (read-char p)))
                (cond
                  ((eof-object? d)
                    n)
                  ((char-set-contains? ekmd-cs:num d)
                    (num (cons* d n) (read-char p)))
                  (else
                    (unread-char d p)
                    n)))))
          (ekmd-error "unknown element"))))))


#(define-public (ekmd-read fn)
  (let ((p (open-input-file fn #:encoding "utf-8")))
    (ekmd-val p ekmd-ro "")
    (close-port p)))


#(define (ekmd-write p val)
  (if (and (pair? val) (pair? (cdr val)))
    (if (number? (cadr val))
      (format p "~s~%" (cdr val))
      (for-each (lambda (v) (ekmd-write p v)) (cdr val)))))


#(define (ekmd-save fn)
  (let ((p (open-output-file fn)))
    (format p "'(~%")
    (for-each (lambda (v) (format p "~s~%" v)) ekmd-table)
    (format p ")~%")
    (format p "'(~%")
    (ekmd-write p ekmd-ro)
    (format p ")~%")
    ;(format p "~%'~s~%" ekmd-table)
    (close-port p)))


#(begin
  (ekmd-read metadataName)
  (ekmd-save metadataCache))
