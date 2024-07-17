%% Esmuflily - Support for SMuFL/Ekmelos
%% Copyright (c) 2020-2024 Thomas Richter
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%%
%% File: esmufl.ily
%% Latest revision: 2024-07-17
%%

\version "2.24.0"


#(define ekm:font-name #f)
#(define ekm:draw-paths #f)


%% Predicates

#(define (number-or-list? x)
  (or (number? x) (list? x)))

#(define ekm-cp? integer?)

#(define (ekm-cdr-cp? x)
  (or (null? (cdr x))
      (and (ekm-cp? (cadr x)) (< 31 (cadr x)))))

#(define (ekm-cp-or-string? x)
  (or (ekm-cp? x) (string? x)))

#(define (ekm-cp-or-vector? x)
  (or (ekm-cp? x) (vector? x)))

#(define (ekm-extext? x)
  (or (ekm-cp? x)
      (string? x)
      (pair? x)))


%% Markup and stencils

#(define-markup-command (ekm-str layout props str)
  (string?)
  #:properties ((font-size 0))
  (interpret-markup
    layout
    (cons
      `((font-size . ,(+ font-size 5))
        (font-name . ,ekm:font-name))
      props)
    str))

#(define-markup-command (ekm-char layout props cp)
  (ekm-cp?)
  #:properties ((font-size 0))
  (if (zero? cp)
    point-stencil
  (if ekm:draw-paths
    (ekm-path-stencil cp font-size 0 #t)
    (interpret-markup layout
      (cons
        `((font-size . ,(+ font-size 5))
          (font-name . ,ekm:font-name))
        props)
      (ly:wide-char->utf-8 cp)))))

#(define-markup-command (ekm-charf layout props cp ff)
  (ekm-cp? number-or-list?)
  #:properties ((font-size 0))
  (let ((f (if (pair? ff) (car ff) ff)))
    (if (and (number? f) (negative? f))
      (if (defined? 'ekm-path-stencil)
        (ekm-path-stencil cp font-size (if (= -1 f) 0 (- f)) (= -1 f))
        empty-stencil)
      (interpret-markup layout
        (cons
          (cons*
            `(font-size . ,(+ font-size 5))
            `(font-name . ,ekm:font-name)
            (if (null? ff)
              ff
              `((font-features .
                ,(if (number? f)
                  (list (string-append "salt " (number->string f)))
                  ff)))))
          props)
        (ly:wide-char->utf-8 cp)))))

#(define-markup-command (ekm-chars layout props cps)
  (cheap-list?)
  #:properties ((font-size 0))
  (if (null? cps)
    point-stencil
  (if ekm:draw-paths
    (stack-stencil-line 0
      (map (lambda (cp) (ekm-path-stencil cp font-size 0 #t)) cps))
    (interpret-markup layout props
      (make-ekm-str-markup
        (string-concatenate
          (map (lambda (cp) (ly:wide-char->utf-8 cp)) cps)))))))

#(define-markup-command (ekm-text layout props txt)
  (ekm-extext?)
  (interpret-markup layout props
    (if (ekm-cp? txt)
      (make-ekm-char-markup txt)
    (if (pair? txt)
      (if (ekm-cp? (car txt))
        (if (ekm-cdr-cp? txt)
          (make-ekm-chars-markup txt)
          (make-ekm-charf-markup (car txt) (cdr txt)))
        txt)
      txt))))

#(define (ekm-cdr-text p)
  (set-cdr! p (make-ekm-text-markup (cdr p))) p)

#(define-markup-command (ekm-line layout props args)
  (pair?)
  #:properties ((word-space)
                (text-direction RIGHT))
  (let* ((al (map (lambda (s)
               (if (or (ekm-cp? s) (pair? s))
                 (make-general-align-markup Y DOWN
                   (make-ekm-text-markup s))
                 s))
               args))
         (sil (interpret-markup-list layout props al)))
    (stack-stencil-line
      word-space
      (if (= LEFT text-direction) (reverse sil) sil))))

#(define (ekm:char layout props cp)
  (interpret-markup layout props (make-ekm-char-markup cp)))

#(define (ekm-center center sil)
  (let ((s (if (logtest center 1) (ly:stencil-aligned-to sil X CENTER) sil)))
    (if (logtest center 2) (ly:stencil-aligned-to s Y CENTER) s)))

#(define-markup-command (ekm-cchar layout props center cp)
  (integer? ekm-cp?)
  (ekm-center center (ekm:char layout props cp)))

#(define (ekm-cchar grob center cp)
  (ekm-center center
    (grob-interpret-markup grob
      (make-ekm-char-markup cp))))

#(define (ekm-ctext grob center txt)
  (ekm-center center
    (grob-interpret-markup grob
      (make-ekm-text-markup txt))))

#(define-markup-command (ekm-ctext layout props center txt)
  (integer? ekm-extext?)
  (ekm-center
    (logand center
      (if (ekm-cp? txt) 0
      (if (pair? txt)
        (if (ekm-cp? (car txt)) (if (ekm-cdr-cp? txt) 1 0) 3) 3)))
    (interpret-markup layout props
      (make-ekm-text-markup txt))))


#(define (ekm-number->list tail cp num)
  (let digit ((f (not num)) (n num) (l tail))
    (if f l
      (digit
        (< n 10)
        (quotient n 10)
        (cons* ((if (vector? cp) vector-ref +) cp (remainder n 10)) l)))))

#(define-markup-command (ekm-number layout props cp num)
  (ekm-cp-or-vector? integer?)
  (interpret-markup layout props
    (make-ekm-chars-markup (ekm-number->list '() cp num))))

#(define-markup-command (ekm-combine layout props cp x y scp)
  (ekm-cp? number? number? ekm-cp?)
  (interpret-markup layout props
    (markup #:combine
      #:ekm-char cp
      #:translate-scaled (cons x y) #:ekm-char scp)))


%% Common symbols (spaces) mapped onto definition key:
%%  ("definition" . cp)
#(define ekm-common-tab '(
  (" " . #x0020)
  ("____" . #x2003)
  ("___" . #x2002)
  ("__" . #x2009)
  ("_" . #x200A)
))

#(define (ekm-assd tab def)
  (if (null? tab) #f
  (if (string-prefix? (caar tab) def) (car tab)
  (ekm-assd (cdr tab) def))))

#(define (ekm-def-list tab def keys)
  ;; Return a list (k v ...).
  ;; k is a list of the keys found in string `def' if `keys' is true
  ;; (used only by ekm-harp-pedal), else k is '().
  ;; The tail list (v ...) holds the corresponding values as markup.
  (let cvt ((k '()) (v '()) (d def))
    (if (string-null? d)
      (cons* (reverse k) (reverse v))
      (let ((f (or (ekm-assd tab d)
                   (ekm-assd ekm-common-tab d))))
        (if (not f)
          (begin
            (ly:warning "Definition string has unknown characters `~a'" d)
            (cvt k v ""))
          (cvt
            (if (and keys (cdr f)) (cons* (car f) k) k)
            (cons* (make-ekm-text-markup (or (cdr f) 0)) v)
            (substring d (string-length (car f)))))))))

#(define-markup-command (ekm-def layout props tab def)
  (pair? string?)
  (stack-stencil-line 0
    (interpret-markup-list layout props
      (cdr (ekm-def-list tab def #f)))))


#(define (ekm-extent sil dir)
  (interval-length (ly:stencil-extent sil dir)))

#(define (ekm-dim grob mk dir)
  (cdr (ly:stencil-extent (grob-interpret-markup grob mk) dir)))


%% Table access

#(define (ekm-assq tab key)
  (or (assq-ref tab key) (cdar tab)))

#(define (ekm-asst tab style key dir)
  ;; Return the value in tab for style and key,
  ;; or for key if style #f, or the entire tab if style and key #f.
  ;; Default is the first style and the last key.
  (let* ((stab (if style (ekm-assq tab style) tab))
         (val (if key (or (assoc-ref stab key) (cdr (last stab))) stab)))
    (if (or (not-pair? val) (zero? dir))
      val
      (if (or (null? (cdr val)) (positive? dir))
        (car val)
        (cdr val)))))

#(define (ekm-assld tab grob log dir)
  (ekm-asst tab
    (if (ly:grob? grob) (ly:grob-property grob 'style) grob)
    (or log (ly:grob-property grob 'duration-log))
    (or dir (ly:grob-property (ly:grob-object grob 'stem) 'direction))))
%#(define (ekm-assld tab grob log dir)
%  (let* ((g (ly:grob? grob))
%         (st (if g (ly:grob-property grob 'style) grob)))
%    (ekm-asst tab
%      st ;(if (symbol? st) st 'default)
%      (if g (ly:grob-property grob 'duration-log) log)
%      (if g (ly:grob-property (ly:grob-object grob 'stem) 'direction) dir))))


%% Orientation arguments

#(define-public N 2)
#(define-public NE 1.5)
#(define-public E 1)
#(define-public SE 0.5)
#(define-public S 0)
#(define-public SW -0.5)
#(define-public W -1)
#(define-public NW -1.5)
#(define-public NS -2)
#(define-public NESW -2.5)
#(define-public EW -3)
#(define-public SENW -3.5)

%% Positions (x . y) for orientation index 0-7 (used by ekm-label)
#(define ekm-orient-pos '#(
  (0 . 1)
  (1 . 1)
  (1 . 0.5)
  (1 . 0)
  (0 . -1)
  (-1 . 0)
  (-1 . 0.5)
  (-1 . 1)))

#(define (ekm-oref vec orient)
  ;; Return the value in vec for orient or for N if orient is invalid.
  (let* ((l (vector-length vec))
         (i (max 0 (inexact->exact
              (if (< 4 l)
                (- 4 (ceiling (* orient 2)))
                (- 2 (ceiling orient)))))))
    (vector-ref vec (if (< i l) i 0))))

#(define-markup-command (ekm-label layout props orient label arg)
  (boolean-or-number? markup? markup?)
  #:properties ((font-size 0)
                (label-size -4)
                (padding 0.3))
  (let ((pos (if (number? orient) (ekm-oref ekm-orient-pos orient) #f)))
    (if pos
      (let* ((lbl (interpret-markup layout
                    (cons `((font-size . ,(+ font-size label-size))) props)
                    label))
             (sil (interpret-markup layout props arg)))
        (if (= 0 (car pos))
          (ly:stencil-combine-at-edge
            (ekm-center 1 sil) Y (cdr pos) (ekm-center 1 lbl) padding)
          (ly:stencil-combine-at-edge
            sil
            X (car pos)
            (if (= 0 (cdr pos))
              lbl
              (ly:stencil-translate lbl
                (cons 0 (* (- (ekm-extent sil Y) (ekm-extent lbl Y)) (cdr pos)))))
            padding)))
      sil)))


%% Clefs
%% see scm\parser-clef.scm

%% Add new clefs to LilyPond
%% Each has clp = trp = c0p = 0
#(for-each
  (lambda (n) (add-new-clef n n 0 0 0))
  '("semipitched"
    "varsemipitched"
    "indiandrum"
    "moderntab"
    "talltab"
    "seriftab"
    "4stringtab"
    "4stringmoderntab"
    "4stringtalltab"
    "4stringseriftab"
    "bridge"
    "accordion"))

%% Clefs mapped onto glyph name:
%%  ("glyph-name" cp . cp-change)
%% glyph-name:
%%  Has no "clefs." prefix for an additional SMuFL clef.
#(define ekm-clef-tab '(
  ("clefs.G" #xE050 . #xE07A)
  ("clefs.GG" #xE055 . #xF630)
  ("clefs.tenorG" #xE056 . #xF631)
  ("clefs.C" #xE05C . #xE07B)
  ("clefs.varC" #xF633 . #xF634)
  ("clefs.F" #xE062 . #xE07C)
  ("clefs.percussion" #xE069 . #xF635)
  ("clefs.varpercussion" #xE06A . #xF636)
  ("semipitched" #xE06B . #xF6BE)
  ("varsemipitched" #xE06C . #xF6BF)
  ("indiandrum" #xED70 . #f)
  ("clefs.tab" #xF61E . #xF61F)
  ("moderntab" #xE06D . #xE06E)
  ("talltab" #xF40A . #xF40C)
  ("seriftab" #xF40B . #xF40D)
  ("4stringtab" #xF61F . #f)
  ("4stringmoderntab" #xE06E . #f)
  ("4stringtalltab" #xF40C . #f)
  ("4stringseriftab" #xF40D . #f)
  ("bridge" #xE078 . #f)
  ("accordion" #xE079 . #f)
  ("clefs.neomensural.c" #xE060 . #xF632)
))

#(define (ekm-clef grob)
  (let* ((name (ly:grob-property grob 'glyph-name))
         (ch (string-suffix? "_change" name))
         (name (if ch (string-drop-right name 7) name))
         (cps (assoc-ref ekm-clef-tab name))
         (sz (ly:staff-symbol-staff-space grob)) ;; 1.5 for TabStaff (1.5 * 1.4 = 2.1)
         (sz (if (= 1 sz) 0 (* sz 1.4))))
    (grob-interpret-markup grob
      (make-fontsize-markup
        (if ch (if (cdr cps) (+ sz 2) (- sz 2)) sz) ;; or (+ sz 1.5) for change
        (make-ekm-char-markup
          (if (and ch (cdr cps)) (cdr cps) (car cps)))))))

#(define (ekm-clef-mod trans style)
  (let* ((cps (case style
                ((parenthesized) '((#xED8A) . (#xED8B)))
                ((bracketed) '((#xED8C) . (#xED8D)))
                (else '(() . ()))))
         (tr (string->number trans))
         (tr (case tr
              ((8)  (cons* #xE07D (cdr cps)))
              ((15) (cons* #xE07E (cdr cps)))
              (else (ekm-number->list (cdr cps) #xED80 tr)))))
    (make-hcenter-in-markup 1.5
      (make-fontsize-markup 2.7
        (make-ekm-chars-markup (append (car cps) tr))))))


%% Time signatures
%% see scm\time-signature-settings.scm

#(define (ekm-time-subnum num)
  (case num
    ((1/4) #xE097)
    ((1/2) #xE098)
    ((3/4) #xE099)
    ((1/3) #xE09A)
    ((2/3) #xE09B)
    (else #xE08E)))

#(define (ekm-time-num l num)
  ;; Return the first element of list `l'.
  ;; If `num' is true, the element is converted to markup number.
  (if num
    (cond
      ((pair? (car l))
        (make-ekm-chars-markup
          (ekm-number->list
            (list (ekm-time-subnum (cdar l)))
            #xE080
            (caar l))))
      ((integer? (car l))
        (make-ekm-number-markup #xE080 (car l)))
      (else
        (make-ekm-char-markup (ekm-time-subnum (car l)))))
    (car l)))

#(define (ekm-time-join ls sep denom)
  ;; Return (rls . rd) where `rls' is a list with the elements from
  ;; list `ls' and `sep' inserted between them (infix) and `rd' is #f.
  ;; If `denom' is true, `rd' is the last element of `ls' (denominator)
  ;; and all elements are converted to markup.
  (if (null? ls)
    ls
    (let join ((l ls))
      (cond
        ;; last element (no denominator)
        ((null? (cdr l))
          (cons (list (ekm-time-num l denom)) #f))
        ;; last two elements (last is denominator)
        ((and denom (null? (cddr l)))
          (cons (list (ekm-time-num l #t)) (ekm-time-num (cdr l) #t)))
        (else
          (let ((r (join (cdr l))))
            (set-car! r (cons* (ekm-time-num l denom) sep (car r)))
            r))))))

#(define (ekm-time-fraction fr st)
  ;; Return fraction `fr' as markup. `fr' must be a number or a pair or
  ;; list of numbers. The last number is the denominator unless only one
  ;; number or `st' is 'single-digit.
  (let* ((t (ekm-time-join
              (cond
                ((number? fr) (list fr))
                ((number-pair? fr) (list (car fr) (cdr fr)))
                (else fr))
              (make-ekm-char-markup #xE08D)
              #t))
         (num (make-line-markup (car t))))
    (make-vcenter-markup
      (if (or (not (cdr t)) (eq? 'single-digit st))
        num
        (make-override-markup
          '(baseline-skip . 0)
          (make-center-column-markup
            (list num (cdr t))))))))

#(define (ekm-time-list sig st)
  (make-override-markup
    '(baseline-skip . 0)
    (make-line-markup
      (car
        (ekm-time-join
          (map (lambda (fr) (ekm-time-fraction fr st)) sig)
          (make-ekm-cchar-markup 2 #xE08C)
          #f)))))

#(define (ekm-time-plain sig)
  ;; Return list `sig' with sub-fractions replaced by simple fractions
  ;; required for Timing properties.
  (map (lambda (s)
    (if (pair? s)
      (let ((dd (fold (lambda (n d)
                  (cond
                    ((pair? n) (* d (denominator (cdr n))))
                    ((integer? n) d)
                    (else (* d (denominator n)))))
                  1
                  s)))
        (map (lambda (n)
          (cond
            ((pair? n) (* dd (+ (car n) (cdr n))))
            (else (* dd n))))
          s))
      s))
    (if (number? sig) (list (list sig 1))
    (if (number? (car sig))
      (if (number? (cdr sig))
        (list (list (car sig) (cdr sig)))
        (list sig))
      sig))))

#(define (ekm-time-compound sig)
  (if (and (pair? sig) (pair? (car sig)))
    (ekm-time-list sig 'numbered)
    (ekm-time-fraction sig 'numbered)))

#(define-markup-command (ekm-compound-meter layout props sig)
  (number-or-pair?)
  (interpret-markup layout props
    (ekm-time-compound sig)))

ekmCompoundMeter =
#(define-music-function (sig)
  (number-or-pair?)
  (let* ((pln (ekm-time-plain sig))
         (mlen (calculate-compound-measure-length pln))
         (beat (calculate-compound-base-beat pln))
         (beatGrouping (calculate-compound-beat-grouping pln))
         (timesig (cons (ly:moment-main-numerator mlen)
                        (ly:moment-main-denominator mlen))))
    #{
      \once \override Timing.TimeSignature.stencil =
        #(lambda (grob) (grob-interpret-markup grob (ekm-time-compound sig)))
      \set Timing.timeSignatureFraction = #timesig
      \set Timing.baseMoment = #beat
      \set Timing.beatStructure = #beatGrouping
      \set Timing.beamExceptions = #'()
      \set Timing.measureLength = #mlen
    #}))

#(define (ekm-timesig grob)
  (let* ((fr (ly:grob-property grob 'fraction))
         (st (ly:grob-property grob 'style))
         (cp (cond
               ((equal? '(4 . 4) fr) #xE08A)
               ((equal? '(2 . 2) fr) #xE08B)
               (else #f))))
    (grob-interpret-markup grob
      (if (and cp (or (eq? 'C st) (eq? 'default st)))
        (make-ekm-cchar-markup 2 cp)
        (ekm-time-fraction fr st)))))


%% Cadenza signatures

ekmCadenzaOn =
#(define-music-function (style)
  (symbol?)
  (let ((cp (case style
              ((time-x) #xE09C)
              ((time-penderecki) #xE09D)
              (else #f))))
    (if cp
      #{
        \once \override Staff.TimeSignature.stencil =
          #(lambda (grob) (ekm-cchar grob 2 cp))
        \time 1/1 % force printing
        \cadenzaOn
      #}#{
        \cadenzaOn
      #})))


%% Staff dividers and separators
%% after lsr.di.unimi.it/LSR/Item?id=650

ekmStaffDivider =
#(define-music-function (dir)
  (number?)
  #{
    \once \override Staff.BarLine.stencil =
    #(lambda (grob)
      (ly:stencil-combine-at-edge
        (ly:bar-line::print grob)
        X RIGHT
        (grob-interpret-markup grob
          (make-with-dimensions-markup
            '(0 . 0) '(0 . 0)
            (make-general-align-markup
              Y (- dir)
              (make-ekm-char-markup
                (if (= 0 dir) #xE00D
                (if (< 0 dir) #xE00C
                #xE00B))))))
        0))
    \break
  #})

ekmSlashSeparator =
#(define-scheme-function (size) (integer?)
 #{ \markup {
      \center-align
      \vcenter
      \override #'(font-size . -5)
      \ekm-char #(+ #xE007 (max 0 (min 2 size)))
    }
 #})


%% Noteheads

%% Noteheads mapped onto style and duration log:
%%  (style . (notehead-entry ...))
%% notehead-entry:
%%  (log notehead-data-up . notehead-data-down)
%%  (log notehead-data)
%%  (log . cp)
%% notehead-data:
%%  cp
%%  (cp cp-empty stem-attach-x . stem-attach-y)
%% cp-empty:
%%  Character to whiteout the background (used by note name noteheads) or #f.
#(define ekm-notehead-tab '(
  (default
    (-2 #xF637 . #xF638)
    (-1 . #xF639) ;; breve with one vertical line
    (0 . #xE0A2)
    (1 (#xE0A3 #f 1 . 0.44) . (#xE0A3 #f -1 . -0.44))
    (2 (#xE0A4 #f 1 . 0.3) . (#xE0A4 #f -1 . -0.3)))
  (altdefault
    (-2 #xF637 . #xF638)
    (-1 . #xE0A0) ;; breve with two vertical lines
    (0 . #xE0A2)
    (1 (#xE0A3 #f 1 . 0.44) . (#xE0A3 #f -1 . -0.44))
    (2 (#xE0A4 #f 1 . 0.3) . (#xE0A4 #f -1 . -0.3)))
  (harmonic
    (2 (#xE0D9 #f 1 . -0.08) . (#xE0D9 #f -1 . 0.08)))
  (harmonic-black
    (-1 . #xE0DC)
    (0 . #xE0DC)
    (1 (#xE0DB #f 1 . -0.08) . (#xE0DB #f -1 . 0.08)))
  (harmonic-white
    (-1 . #xE0DE)
    (0 . #xE0DE)
    (1 (#xE0DD #f 1 . -0.08) . (#xE0DD #f -1 . 0.08)))
  (harmonic-mixed
    (-1 . #xE0D7)
    (0 . #xE0D8)
    (1 (#xE0D9 #f 1 . -0.08) . (#xE0D9 #f -1 . 0.08))
    (2 (#xE0DB #f 1 . -0.08) . (#xE0DB #f -1 . 0.08)))
  (harmonic-wide
    (-1 . #xE0D7)
    (0 . #xE0D8)
    (1 (#xE0DA #f 1 . -0.08) . (#xE0DA #f -1 . 0.08))
    (2 (#xE0DC #f 1 . -0.08) . (#xE0DC #f -1 . 0.08)))
  (diamond
    (-1 . #xE0DF)
    (0 . #xE0E0)
    (1 (#xE0E1 #f 1 . 0.6) . (#xE0E1 #f -1 . -0.6))
    (2 (#xE0E2 #f 1 . 0.6) . (#xE0E2 #f -1 . -0.6)))
  (cross
    (-1 . #xE0A6)
    (0 . #xE0A7)
    (1 (#xE0A8 #f 1 . 0.65) . (#xE0A8 #f -1 . -0.65))
    (2 (#xE0A9 #f 1 . 0.72) . (#xE0A9 #f -1 . -0.72)))
  (plus
    (-1 . #xE0AC)
    (0 . #xE0AD)
    (1 (#xE0AE #f 0.99 . 0.05) . (#xE0AE #f -0.99 . -0.05))
    (2 (#xE0AF #f 0.99 . 0) . (#xE0AF #f -0.99 . 0)))
  (xcircle
    (-1 . #xE0B0)
    (0 . #xE0B1)
    (1 (#xE0B2 #f 0.8 . 0.44) . (#xE0B2 #f -0.8 . -0.44))
    (2 (#xE0B3 #f 0.77 . 0.54) . (#xE0B3 #f -0.77 . -0.54)))
  (withx
    (-1 . #xE0B4)
    (0 . #xE0B5)
    (1 (#xE0B6 #f 1 . 0.44) . (#xE0B6 #f -1 . -0.44))
    (2 (#xE0B7 #f 1 . 0.39) . (#xE0B7 #f -1 . -0.39)))
  (withx-black
    (-1 . #xE0B4)
    (0 . #xE0B5)
    (1 (#xE0B6 #f 1 . 0.44) . (#xE0B6 #f -1 . -0.44))
    (2 (#xF680 #f 1 . 0.15) . (#xF680 #f -1 . -0.15)))
  (slashed
    (-1 . #xE0D5)
    (0 . #xE0D3)
    (1 (#xE0D1 #f 0.95 . 0.5) . (#xE0D1 #f -0.95 . -0.5))
    (2 (#xE0CF #f 0.93 . 0.5) . (#xE0CF #f -0.93 . -0.5)))
  (backslashed
    (-1 . #xE0D6)
    (0 . #xE0D4)
    (1 (#xE0D2 #f 1 . 0.38) . (#xE0D2 #f -1 . -0.38))
    (2 (#xE0D0 #f 1 . 0.3) . (#xE0D0 #f -1 . -0.3)))
  (slash
    (-1 . #xE10A)
    (0 . #xE102)
    (1 (#xE103 #f 1 . 0.94) . (#xE103 #f -1 . -0.94))
    (2 (#xE101 #f 1 . 0.9) . (#xE101 #f -1 . -0.9)))
  (slash-muted
    (-1 . #xE109)
    (0 . #xE109)
    (1 (#xE109 #f 1 . 0.94) . (#xE109 #f -1 . -0.94))
    (2 (#xE108 #f 0.88 . 0.9) . (#xE108 #f -0.88 . -0.9)))
  (circled
    (-1 . #xE0E7)
    (0 . #xE0E6)
    (1 (#xE0E5 #f 1 . 0.44) . (#xE0E5 #f -1 . -0.44))
    (2 (#xE0E4 #f 1 . 0.3) . (#xE0E4 #f -1 . -0.3)))
  (circled-large
    (-1 . #xE0EB)
    (0 . #xE0EA)
    (1 (#xE0E9 #f 1 . 0.29) . (#xE0E9 #f -1 . -0.29))
    (2 (#xE0E8 #f 1 . 0.2) . (#xE0E8 #f -1 . -0.2)))
  (triangle
    (-1 #xE0BA . #xE0C3)
    (0 #xE0BB . #xE0C4)
    (1 (#xE0BC #f 1 . -0.95) . (#xE0C5 #f -1 . 0.95))
    (2 (#xE0BE #f 1 . -0.95) . (#xE0C7 #f -1 . 0.95)))
  (triangle-up
    (-1 . #xE0BA)
    (0 . #xE0BB)
    (1 (#xE0BC #f 1 . -0.95) . (#xE0BC #f -1 . -0.8))
    (2 (#xE0BE #f 1 . -0.95) . (#xE0BE #f -1 . -0.8)))
  (triangle-down
    (-1 . #xE0C3)
    (0 . #xE0C4)
    (1 (#xE0C5 #f 1 . 0.8) . (#xE0C5 #f -1 . 0.95))
    (2 (#xE0C7 #f 1 . 0.8) . (#xE0C7 #f -1 . 0.95)))
  (arrow
    (-1 #xE0ED . #xE0F1)
    (0 #xE0EE . #xE0F2)
    (1 (#xE0EF #f 1 . -0.94) . (#xE0F3 #f -1 . 0.94))
    (2 (#xE0F0 #f 1 . -0.94) . (#xE0F4 #f -1 . 0.94)))
  (arrow-up
    (-1 . #xE0ED)
    (0 . #xE0EE)
    (1 (#xE0EF #f 1 . -0.94) . (#xE0EF #f -1 . -0.94))
    (2 (#xE0F0 #f 1 . -0.94) . (#xE0F0 #f -1 . -0.94)))
  (arrow-down
    (-1 . #xE0F1)
    (0 . #xE0F2)
    (1 (#xE0F3 #f 1 . 0.9) . (#xE0F3 #f -1 . 0.94))
    (2 (#xE0F4 #f 1 . 0.9) . (#xE0F4 #f -1 . 0.94)))
  (round
    (0 . #xE114)
    (1 (#xE114 #f 0.91 . 0.33) . (#xE114 #f -0.91 . -0.33))
    (2 (#xE113 #f 0.91 . 0.33) . (#xE113 #f -0.91 . -0.33)))
  (round-large
    (0 . #xE111)
    (1 (#xE111 #f 1 . 0.06) . (#xE111 #f -1 . -0.06))
    (2 (#xE110 #f 1 . 0.06) . (#xE110 #f -1 . -0.06)))
  (round-dot
    (0 . #xE115)
    (1 (#xE115 #f 0.91 . 0.33) . (#xE115 #f -0.91 . -0.33))
    (2 (#xE113 #f 0.91 . 0.33) . (#xE113 #f -0.91 . -0.33)))
  (round-dot-large
    (0 . #xE112)
    (1 (#xE112 #f 1 . 0.06) . (#xE112 #f -1 . -0.06))
    (2 (#xE110 #f 1 . 0.06) . (#xE110 #f -1 . -0.06)))
  (round-slashed
    (0 . #xE119)
    (1 (#xE119 #f 0.74 . 0.17) . (#xE119 #f -0.74 . -0.17))
    (2 (#xE118 #f 0.74 . 0.17) . (#xE118 #f -0.74 . -0.17)))
  (round-slashed-large
    (0 . #xE117)
    (1 (#xE117 #f 1 . 0.06) . (#xE117 #f -1 . -0.06))
    (2 (#xE116 #f 1 . 0.06) . (#xE116 #f -1 . -0.06)))
  (square
    (0 . #xE0B8)
    (1 (#xE0B8 #f 1 . 0.78) . (#xE0B8 #f -1 . -0.78))
    (2 (#xE0B9 #f 1 . 0.78) . (#xE0B9 #f -1 . -0.78)))
  (square-large
    (0 . #xE11B)
    (1 (#xE11B #f 1 . 0.88) . (#xE11B #f -1 . -0.88))
    (2 (#xE11A #f 1 . 0.88) . (#xE11A #f -1 . -0.88)))
  (baroque
    (-1 . #xE0A1)
    (0 . #xE0A2)
    (1 (#xE0A3 #f 1 . 0.44) . (#xE0A3 #f -1 . -0.44))
    (2 (#xE0A4 #f 1 . 0.3) . (#xE0A4 #f -1 . -0.3)))
  (parenthesised
    (-1 . #xF5DF)
    (0 . #xF5DE)
    (1 (#xF5DD #f 0.61 . 0.365) . (#xF5DD #f -0.61 . -0.365))
    (2 (#xF5DC #f 0.61 . 0.23) . (#xF5DC #f -0.61 . -0.23)))

  ;; shape noteheads
  (sol ;; round
    (-1 . #xECD0)
    (0 . #xE1B0)
    (1 . #xE1B0)
    (2 . #xE1B1))
  (solFunk ;; round
    (-1 . #xECD0)
    (0 . #xE1B0)
    (1 . #xE1B0)
    (2 . #xE1B1))
  (la ;; square
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laWalker ;; square
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laThin ;; square thin
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laFunk ;; square small
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (fa ;; u: triangle left, d: triangle right
    (-1 #xECD3 . #xECD2)
    (0 #xE1B6 . #xE1B4)
    (1 #xE1B6 . #xE1B4)
    (2 #xE1B7 . #xE1B5))
  (faThin ;; u: triangle left thin, d: triangle right thin
    (-1 #xECD3 . #xECD2)
    (0 #xE1B6 . #xE1B4)
    (1 #xE1B6 . #xE1B4)
    (2 #xE1B7 . #xE1B5))
  (faFunk ;; u: triangle left small, d: triangle right small
    (-1 #xECD3 . #xECD2)
    (0 #xE1B6 . #xE1B4)
    (1 #xE1B6 . #xE1B4)
    (2 #xE1B7 . #xE1B5))
  (faWalker ;; u: triangle left small, d: triangle right small
    (-1 #xECD3 . #xECD2)
    (0 #xE1B6 . #xE1B4)
    (1 #xE1B6 . #xE1B4)
    (2 #xE1B7 . #xE1B5))
  (mi ;; diamond
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miThin ;; diamond thin
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miFunk ;; u: diamond, d: diamond rev
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miMirror ;; diamond rev
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miWalker ;; diamond rev
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (do ;; triangle up
    (-1 . #xECD5)
    (0 . #xE1BA)
    (1 . #xE1BA)
    (2 . #xE1BB))
  ;(doThin ;; triangle up thin (not used)
  ;  (-1 . #xECD5)
  ;  (0 . #xE1BA)
  ;  (1 . #xE1BA)
  ;  (2 . #xE1BB))
  (re ;; moon
    (-1 . #xECD6)
    (0 . #xE1BC)
    (1 . #xE1BC)
    (2 . #xE1BD))
  ;(reThin ;; moon thin (not used)
  ;  (-1 . #xECD6)
  ;  (0 . #xE1BC)
  ;  (1 . #xE1BC)
  ;  (2 . #xE1BD))
  (ti ;; triangle round
    (-1 . #xECD7)
    (0 . #xE1BE)
    (1 . #xE1BE)
    (2 . #xE1BF))
  ;(tiThin ;; triangle round thin (not used)
  ;  (-1 . #xECD7)
  ;  (0 . #xE1BE)
  ;  (1 . #xE1BE)
  ;  (2 . #xE1BF))
  (doWalker ;; u: keystone, d: keystone inv
    (-1 . #xECD8)
    (0 . #xE1C0)
    (1 . #xE1C0)
    (2 . #xE1C1))
  (reWalker ;; u: quarter moon, d: quarter moon rev
    (-1 . #xECD9)
    (0 . #xE1C2)
    (1 . #xE1C2)
    (2 (#xE1C3 #f 1 . 0) . (#xE1C3 #f -1 . -0.74)))
  (tiWalker ;; u: isosceles triangle rev, d: isosceles triangle
    (-1 . #xECDA)
    (0 . #xE1C4)
    (1 . #xE1C4)
    (2 . #xE1C5))
  (doFunk ;; u: moon left rev, d: moon left
    (-1 . #xECDB)
    (0 . #xE1C6)
    (1 . #xE1C6)
    (2 . #xE1C7))
  (reFunk ;; u: arrowhead left rev, d: arrowhead left
    (-1 . #xECDC)
    (0 . #xE1C8)
    (1 . #xE1C8)
    (2 (#xE1C9 #f 1 . 0.74) . (#xE1C9 #f -1 . 0)))
  (tiFunk ;; u: triangle round left rev, d: triangle round left
    (-1 . #xECDD)
    (0 . #xE1CA)
    (1 . #xE1CA)
    (2 . #xE1CB))

  ;; note name noteheads
  (doName
    (0 (#xE150 #xE1AD))
    (1 (#xE158 #xE1AE 1 . 0.44) . (#xE158 #xE1AE -1 . -0.44))
    (2 (#xE160 #xE1AF 1 . 0.3) . (#xE160 #xE1AF -1 . -0.3)))
  (reName
    (0 (#xE151 #xE1AD))
    (1 (#xE159 #xE1AE 1 . 0.44) . (#xE159 #xE1AE -1 . -0.44))
    (2 (#xE161 #xE1AF 1 . 0.3) . (#xE161 #xE1AF -1 . -0.3)))
  (miName
    (0 (#xE152 #xE1AD))
    (1 (#xE15A #xE1AE 1 . 0.44) . (#xE15A #xE1AE -1 . -0.44))
    (2 (#xE162 #xE1AF 1 . 0.3) . (#xE162 #xE1AF -1 . -0.3)))
  (faName
    (0 (#xE153 #xE1AD))
    (1 (#xE15B #xE1AE 1 . 0.44) . (#xE15B #xE1AE -1 . -0.44))
    (2 (#xE163 #xE1AF 1 . 0.3) . (#xE163 #xE1AF -1 . -0.3)))
  (soName
    (0 (#xE154 #xE1AD))
    (1 (#xE15C #xE1AE 1 . 0.44) . (#xE15C #xE1AE -1 . -0.44))
    (2 (#xE164 #xE1AF 1 . 0.3) . (#xE164 #xE1AF -1 . -0.3)))
  (laName
    (0 (#xE155 #xE1AD))
    (1 (#xE15D #xE1AE 1 . 0.44) . (#xE15D #xE1AE -1 . -0.44))
    (2 (#xE165 #xE1AF 1 . 0.3) . (#xE165 #xE1AF -1 . -0.3)))
  (siName
    (0 (#xE157 #xE1AD))
    (1 (#xE15F #xE1AE 1 . 0.44) . (#xE15F #xE1AE -1 . -0.44))
    (2 (#xE167 #xE1AF 1 . 0.3) . (#xE167 #xE1AF -1 . -0.3)))
  (tiName
    (0 (#xE156 #xE1AD))
    (1 (#xE15E #xE1AE 1 . 0.44) . (#xE15E #xE1AE -1 . -0.44))
    (2 (#xE166 #xE1AF 1 . 0.3) . (#xE166 #xE1AF -1 . -0.3)))

  ;; individual notes (for note-by-number)
  (note
    (-2 #xF637 . #xF638)
    (-1 . #xE1D0)
    (0 . #xE1D2)
    (1 #xE1D3 . #xE1D4)
    (2 #xE1D5 . #xE1D6)
    (3 #xE1D7 . #xE1D8)
    (4 #xE1D9 . #xE1DA)
    (5 #xE1DB . #xE1DC)
    (6 #xE1DD . #xE1DE)
    (7 #xE1DF . #xE1E0)
    (8 #xE1E1 . #xE1E2)
    (9 #xE1E3 . #xE1E4)
    (10 #xE1E5 . #xE1E6))
  (metronome
    (-1 . #xECA0)
    (0 . #xECA2)
    (1 #xECA3 . #xECA4)
    (2 #xECA5 . #xECA6)
    (3 #xECA7 . #xECA8)
    (4 #xECA9 . #xECAA)
    (5 #xECAB . #xECAC)
    (6 #xECAD . #xECAE)
    (7 #xECAF . #xECB0)
    (8 #xECB1 . #xECB2)
    (9 #xECB3 . #xECB4)
    (10 #xECB5 . #xECB6))
  (straight
    (-2 . #xF637)
    (-1 . #xE1D0)
    (0 . #xE1D2)
    (1 . #xE1D3)
    (2 . #xE1D5)
    (3 . #xF683)
    (4 . #xF686)
    (5 . #xF689))
  (short
    (-2 . #xF637)
    (-1 . #xE1D0)
    (0 . #xE1D2)
    (1 . #xE1D3)
    (2 . #xE1D5)
    (3 . #xF684)
    (4 . #xF687)
    (5 . #xF68A))
  (beamed
    (-2 . #xF637)
    (-1 . #xE1D0)
    (0 . #xE1D2)
    (1 . #xE1D3)
    (2 . #xE1D5)
    (3 . #xF685)
    (4 . #xF688)
    (5 . #xF68B))
))

#(define (ekm-note grob log dir)
  (let* ((d (ekm-assld ekm-notehead-tab grob log dir)))
    (if (pair? d)
      (let ((mk (make-ekm-char-markup (car d))))
        (if (ly:grob? grob)
          (ly:grob-set-property! grob 'stem-attachment (cddr d)))
        (if (cadr d)
          (make-combine-markup
            (make-with-color-markup white
              (make-ekm-char-markup (cadr d)))
            mk)
          mk))
      (make-ekm-char-markup d))))

#(define (ekm-notehead grob)
  (grob-interpret-markup grob (ekm-note grob #f #f)))

ekmNameHeads =
\set shapeNoteStyles = ##(doName reName miName faName soName laName siName)
ekmNameHeadsMinor =
\set shapeNoteStyles = ##(laName siName doName reName miName faName soName)
ekmNameHeadsTi =
\set shapeNoteStyles = ##(doName reName miName faName soName laName tiName)
ekmNameHeadsTiMinor =
\set shapeNoteStyles = ##(laName tiName doName reName miName faName soName)


%% Note clusters
%% after lilypond.1069038.n5.nabble.com/Cowell-clusters-td237881.html

%% Cluster noteheads mapped onto style and duration log:
%%  (style cluster-entry ...)
%% cluster-entry:
%%  (log cluster-data)
%%  (log cluster-data-up . cluster-data-down)  ; not used
%% cluster-data:
%%  (cp-single cp-2nd cp-3rd cp-top cp-mid cp-bottom)
%%  (cp-single cp-2nd cp-3rd cp-top cp-mid cp-bottom stem-pos stem-attach-x . stem-attach-y)
%% stem-pos:
%%  Number added to 'stem-begin-position of stem down
#(define ekm-cluster-tab '(
  (default
    (-1 (#xE0A0 #xE124 #xE128 #xE12C #xE12D #xE12E))
    (0 (#xE0A2 #xE125 #xE129 #xE12F #xE130 #xE131))
    (1 (#xE0A3 #xE126 #xE12A #xE132 #xE133 #xE134 0 1 . 0.44) .
       (#xE0A3 #xE126 #xE12A #xE132 #xE133 #xE134 0 -1 . -0.44))
    (2 (#xE0A4 #xE127 #xE12B #xE135 #xE136 #xE137 0 1 . 0.3) .
       (#xE0A4 #xE127 #xE12B #xE135 #xE136 #xE137 0 -1 . -0.3)))
  (harmonic
    (1 (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0.5 1 . -0.1) .
       (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0.5 -1 . -0.4))
    (2 (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4 1 . -0.1) .
       (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4 -1 . -0.3)))
  (diamond
    (1 (#xE0D9 #xF64B #xF64C #xF64D #xF64E #xF64F 0.5 1 . -0.1) .
       (#xE0D9 #xF64B #xF64C #xF64D #xF64E #xF64F 0.5 -1 . -0.4))
    (2 (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4 1 . -0.1) .
       (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4 -1 . -0.3)))
  (square
    (1 (#xE0B8 #f #f #xE145 #xE146 #xE147 -0.3 1 . 0.8) .
       (#xE0B8 #f #f #xE145 #xE146 #xE147 -0.3 -1 . -0.5))
    (2 (#xE0B9 #f #f #xE142 #xE143 #xE144 -0.3 1 . 0.8) .
       (#xE0B9 #f #f #xE142 #xE143 #xE144 -0.3 -1 . -0.5)))
))

#(define (ekm-cluster grob)
  (let ((nhs (ly:grob-object grob 'note-heads)))
    (if (ly:grob-array? nhs)
      (let* ((nhl (ly:grob-array->list nhs))
             (bot (fold
               (lambda (nh m)
                 (let ((p (ly:grob-property nh 'staff-position)))
                   (ly:grob-set-property! nh 'transparent #t)
                   (if (< p (car m)) (cons p nh) m)))
               (cons 999 #f)
               nhl))
             (d (ekm-assld ekm-cluster-tab (cdr bot) #f #f))
             (st (list-tail d 6))
             (top (fold
               (lambda (nh m)
                 (ly:grob-set-property! nh 'style 'default)
                 (if (pair? st) (ly:grob-set-property! nh 'stem-attachment (cdr st)))
                 (max m (ly:grob-property nh 'staff-position)))
               -999
               nhl))
             (h (- top (car bot)))
             (c (and (< h 3) (list-ref d h)))
             (stm (ly:grob-object grob 'stem))
            )
        (ly:grob-set-property! stm 'avoid-note-head #t)
        (ly:grob-set-property! stm 'note-collision-threshold 0)
        (if (and (pair? st)
                 (> 0 (ly:grob-property stm 'direction)))
          (ly:grob-set-property! stm 'stem-begin-position
            (+ (ly:grob-property stm 'stem-begin-position) (car st))))
        (ly:grob-set-property! (cdr bot) 'transparent #f)
        (ly:grob-set-property! (cdr bot) 'stencil
          (grob-interpret-markup grob
            (make-with-dimensions-from-markup
              (make-ekm-char-markup (car d))
              (if c
                (make-ekm-char-markup c)
                (make-combine-markup
                  (let bar ((m (make-ekm-char-markup (sixth d)))
                            (y (- h 3)))
                    (if (>= 0 y)
                      m
                      (bar
                        (make-combine-markup
                          m
                          (make-translate-markup
                            (cons 0 (+ 0.5 (* 0.5 y)))
                            (make-ekm-char-markup (fifth d))))
                        (1- y))))
                  (make-translate-markup
                    (cons 0 (* 0.5 h))
                    (make-ekm-char-markup (fourth d)))))))))
      '())))

ekmMakeClusters =
#(define-music-function (music)
  (ly:music?)
  #{
    \temporary \override NoteColumn.before-line-breaking = #ekm-cluster
    $music
    \revert NoteColumn.before-line-breaking
  #})


%% Augmentation dots

%% Augmentation dots mapped onto style (for note-by-number):
%%  (style cp pad-3 pad-4 pad-5)
%% pad-N:
%%  Padding between log N note and dot in units of dot width.
%%  pad-5 applies also to log > 5.
#(define ekm-dots-tab '(
  (default   #xE1E7 0 0 0)
  (note      #xE1E7 -0.2 0.7 0.7)
  (metronome #xECB7 0.2 0.7 0.7)
  (straight  #xE1E7 -0.8 -0.8 0.3)
  (short     #xE1E7 -0.8 -0.8 0.5)
  (beamed    #xE1E7 -1.4 -1.4 -1.4)
))

#(define (ekm-cat-dots count dot pad)
  (let ((ext (ekm-extent dot X)))
    (let cat ((c (max count 0))
              (r (if pad point-stencil empty-stencil)))
      (if (zero? c) r
        (cat (1- c) (ly:stencil-stack r X RIGHT dot ext))))))

#(define (ekm-dots grob)
  (ekm-cat-dots
    (ly:grob-property grob 'dot-count)
    (ekm-cchar grob 0 #xE1E7)
    #t))


%% Flags and grace note slashes

%% Flags mapped onto style and duration log (> 2):
%%  (style flag-entry ...)
%% flag-entry:
%%  (log cp-up . cp-down)
#(define ekm-flag-tab '(
  (default
    (3 #xE240 . #xE241)
    (4 #xE242 . #xE243)
    (5 #xE244 . #xE245)
    (6 #xE246 . #xE247)
    (7 #xE248 . #xE249)
    (8 #xE24A . #xE24B)
    (9 #xE24C . #xE24D)
    (10 #xE24E . #xE24F))
  (short
    (3 #xF410 . #xF6C0)
    (4 #xF413 . #xF6C1)
    (5 #xF416 . #xF6C2)
    (6 #xF419 . #xF6C3)
    (7 #xF41C . #xF6C4)
    (8 #xF41F . #xF6C5)
    (9 #xF422 . #xF6C6)
    (10 #xF425 . #xF6C7))
  (straight
    (3 #xF40F . #xF411)
    (4 #xF412 . #xF414)
    (5 #xF415 . #xF417)
    (6 #xF418 . #xF41A)
    (7 #xF41B . #xF41D)
    (8 #xF41E . #xF420)
    (9 #xF421 . #xF423)
    (10 #xF424 . #xF426))
))

%% Lengths of flag stems mapped onto style:
%%  (style stem-length extra-length ...)
%% stem-length:
%%  Nominal unmodified stem length.
%% extra-length:
%%  Amount to lengthen stem for duration log 3-10.
#(define ekm-stemlength-tab '(
  (default 3.5 0 0 0.73 1.45 2.18 2.92 3.65 4.39)
  (short 3.5 0 0 0 0.53 1.03 1.61 2.18 2.76)
  (straight 3.5 0 0 0 0.85 1.71 2.5 3.29 4.05)
))

#(define (ekm-stemlength style)
  (let* ((tab (ekm-assq ekm-stemlength-tab style))
         (len (car tab)))
    (cons* len (map (lambda (y) (+ len y)) (cdr tab)))))

#(define (ekm-flag grob)
  (let* ((stm (ly:grob-parent grob X))
         (dir (ly:grob-property stm 'direction))
         (log (ly:grob-property stm 'duration-log))
         (st (ly:grob-property grob 'style))
         (len (list-ref (ekm-assq ekm-stemlength-tab st) (- log 2)))
         (flg (grob-interpret-markup grob
                (make-ekm-char-markup
                  (ekm-asst ekm-flag-tab st log dir)))))
    (ly:stencil-translate
      (if (equal? "grace" (ly:grob-property grob 'stroke-style))
        (ly:stencil-add
          flg
          (grob-interpret-markup grob
            (make-translate-scaled-markup
              (if (= UP dir)
                (cons -0.644 -2.456)
                (cons -0.596  2.168))
              (make-ekm-char-markup (if (= UP dir) #xE564 #xE565)))))
        flg)
      (cons
        (- (* (ly:grob-property stm 'thickness) (ly:staff-symbol-line-thickness grob)))
        (if (negative? dir) len (- len))))))

ekmFlag =
#(define-music-function (style)
  (symbol?)
  #{
    \override Flag.style = #style
    \override Stem.details.lengths = #(ekm-stemlength style)
  #})


%% Rests

%% Rests mapped onto style and duration log:
%%  (style rest-entry ...)
%% rest-entry:
%%  (log . cp)
%%  (log cp . cp-ledgered)
#(define ekm-rest-tab '(
  (default
    (-3 . #xE4E0)
    (-2 . #xE4E1)
    (-1 #xE4E2 . #xE4F3)
    (0 #xE4E3 . #xE4F4)
    (1 #xE4E4 . #xE4F5)
    (2 . #xE4E5)
    (3 . #xE4E6)
    (4 . #xE4E7)
    (5 . #xE4E8)
    (6 . #xE4E9)
    (7 . #xE4EA)
    (8 . #xE4EB)
    (9 . #xE4EC)
    (10 . #xE4ED))
  (classical
    (-3 . #xE4E0)
    (-2 . #xE4E1)
    (-1 #xE4E2 . #xE4F3)
    (0 #xE4E3 . #xE4F4)
    (1 #xE4E4 . #xE4F5)
    (2 . #xE4F2)
    (3 . #xE4E6)
    (4 . #xE4E7)
    (5 . #xE4E8)
    (6 . #xE4E9)
    (7 . #xE4EA)
    (8 . #xE4EB)
    (9 . #xE4EC)
    (10 . #xE4ED))
  (z
    (-3 . #xE4E0)
    (-2 . #xE4E1)
    (-1 #xE4E2 . #xE4F3)
    (0 #xE4E3 . #xE4F4)
    (1 #xE4E4 . #xE4F5)
    (2 . #xE4F6)
    (3 . #xE4E6)
    (4 . #xE4E7)
    (5 . #xE4E8)
    (6 . #xE4E9)
    (7 . #xE4EA)
    (8 . #xE4EB)
    (9 . #xE4EC)
    (10 . #xE4ED))
))

#(define-markup-command
  (ekm-mmr layout props style ledgered measures limit width space)
  (symbol? boolean? index? integer? number? number?)
  (if (> measures limit)
    (let* ((hbar (ekm:char layout props #xE4F0))
           (lbar (ekm:char layout props #xE4EF))
           (rbar (ekm:char layout props #xE4F1))
           (edge (ekm-extent lbar X)))
      (stack-stencil-line
        (- (* edge 0.25)) ;; overlap edge and bar
        (list
          lbar
          (make-filled-box-stencil
            (cons 0 (- width (* edge 1.5)))
            (ly:stencil-extent hbar Y))
          rbar)))
    (let* ((ssp (ly:output-def-lookup layout 'staff-space))
           (cts
             (let cnt ((m measures) (d '(8 4 2 1)) (c '()))
               (if (null? d)
                 (reverse c)
                 (cnt (remainder m (car d))
                      (cdr d)
                      (cons* (quotient m (car d)) c)))))
           (sils
             (reverse (fold (lambda (ct lg sl)
               (if (zero? ct)
                 sl
                 (let con ((c ct) (s sl))
                   (if (zero? c)
                     s
                     (let ((r (ekm:char layout props
                                (ekm-assld ekm-rest-tab
                                  style lg (if ledgered -1 1)))))
                       (con (1- c)
                            (cons*
                              (if (= 0 lg)
                                (ly:stencil-translate-axis r ssp Y)
                                r)
                              s)))))))
               '() cts '(-3 -2 -1 0))))
           (pad (if (< (length sils) 2)
                  0
                (if (>= space 0)
                  space
                  (/ (fold (lambda (s w) (- w (ekm-extent s X)))
                       width sils)
                     (1- (length sils)))))))
      (stack-stencil-line pad sils))))

#(define (ekm-rest grob)
  (ekm-cchar grob 0 (ekm-assld ekm-rest-tab grob #f 1)))

#(define (ekm-mmr grob)
  (let* ((org (ly:multi-measure-rest::print grob))
         (pos (ly:grob-property grob 'staff-position 0))
         (lines (ly:grob-property (ly:grob-object grob 'staff-symbol) 'line-count))
         (sil (grob-interpret-markup grob
                (make-ekm-mmr-markup
                  (ly:grob-property grob 'style 'default)
                  (and (< 1 lines)
                       (or (odd? pos) (<= lines (abs (+ 2 pos)))))
                  (ly:grob-property grob 'measure-count)
                  (ly:grob-property grob 'expand-limit)
                  (ekm-extent org X)
                  -1)))
         (sil (ly:stencil-aligned-to sil X CENTER))
         (lb (ly:spanner-bound grob LEFT))
         (rb (ly:spanner-bound grob RIGHT))
         (refp (ly:grob-common-refpoint lb rb X))
         (sp (ly:grob-property grob 'spacing-pair
               '(break-alignment . break-alignment)))
         (l (ly:paper-column::break-align-width lb (car sp)))
         (r (ly:paper-column::break-align-width rb (cdr sp))))
    (ly:stencil-translate-axis
      sil
      (+ (* 0.5 (- (car r) (cdr l)))
         (- (cdr l) (ly:grob-relative-coordinate grob refp X)))
      X)))

#(define (ekm-mmr-number grob)
  (let ((num (ly:grob-property grob 'text #f)))
    (if num
      (grob-interpret-markup grob
        (make-ekm-number-markup #xE080 (string->number num)))
      empty-stencil)))

#(define-markup-command
  (ekm-rest-by-number layout props log dot-count)
  (integer? integer?)
  #:properties ((font-size 0)
                (ledgers '(-1 0 1))
                (style '()))
  (let* ((ledg (memv log ledgers))
         (rest (ekm-center 2 (ekm:char layout props
                 (ekm-assld ekm-rest-tab style log (if ledg -1 1)))))
         (dot (and (> dot-count 0) (ekm:char layout props #xE1E7)))
         (dots (and dot (ekm-cat-dots dot-count dot #f))))
    (if dot
      (ly:stencil-stack rest X RIGHT dots
        (* (ekm-extent dot X)
           (if (and ledg (<= -1 log 1)) 2
           (if (> log 2) (/ (- 10 log) 7)
           1))))
      rest)))

#(define-markup-command
  (ekm-multi-measure-rest-by-number layout props measures)
  (index?)
  #:properties ((font-size 0)
                (style '())
                (expand-limit 10)
                (word-space)
                (width 8)
                (multi-measure-rest-number #t))
  (let* ((bar (> measures expand-limit))
         (mmr (interpret-markup layout props
                (make-ekm-mmr-markup
                  (if (null? style) 'default style)
                  #f measures expand-limit width word-space))))
    (if (or bar
            (and multi-measure-rest-number (> measures 1)))
      (let ((num (interpret-markup layout props
                   (make-fontsize-markup -2
                     (make-ekm-number-markup #xE080 measures)))))
        (ly:stencil-combine-at-edge
          mmr
          Y UP
          (ly:stencil-translate-axis
            num
            (- (interval-center (ly:stencil-extent mmr X))
               (interval-center (ly:stencil-extent num X)))
            X)
          (if bar 0 (* 0.8 (ly:output-def-lookup layout 'staff-space)))))
      mmr)))

#(define-markup-command (ekm-rest layout props duration)
  (ly:duration?)
  #:properties (ekm-rest-by-number-markup
                ekm-multi-measure-rest-by-number-markup)
  (let ((measures (ly:duration-scale duration))
        (mmr? (chain-assoc-get 'multi-measure-rest props)))
    (if (and (index? measures) mmr?)
      (ekm-multi-measure-rest-by-number-markup layout props measures)
      (ekm-rest-by-number-markup layout props
        (ly:duration-log duration)
        (ly:duration-dot-count duration)))))


%% Parentheses

#(define (ekm-align dir size cpm)
  (markup #:general-align Y dir #:fontsize size
    (if (ekm-cp? cpm) (make-ekm-char-markup cpm) cpm)))

%% Parentheses mapped onto style and type:
%%  (style parens-entry ...)
%% parens-entry:
%%  (type text-left . text-right)
%% type:
%%  a = accidental, h = hairpin, f = function, t = normal text
#(define ekm-parens-tab `(
  (default
    (a #xE26A . #xE26B)
    ;(n #xE0F5 . #xE0F6)
    (h #xE542 . #xE543)
    (f #xEA91 . #xEA92)
    (t "(" . ")"))
  (bracket
    (a #xE26C . #xE26D)
    (h #xE544 . #xE545)
    (f #xEA8F . #xEA90)
    (t "[" . "]"))
  (brace
    (a #xF6D4 . #xF6D5)
    (f ,(ekm-align -0.5 6 "{") . ,(ekm-align -0.5 6 "}"))
    (t "{" . "}"))
  (angle
    (a #xF6D6 . #xF6D7)
    (h ,(ekm-align 0 -5 #xEA93) . ,(ekm-align 0 -5 #xEA94))
    (f #xEA93 . #xEA94)
    (t "<" . ">"))
))


%% System start delimiter

#(define ekm-system-start-tab '())

#(define-markup-command (ekm-system-start layout props style size)
  (symbol? number?)
  #:properties ((font-size 0))
  (let* (;; select
         (e (let sel ((t (ekm-assq ekm-system-start-tab style)))
              (if (null? t) '(#xE000) ;; should not occur
              (if (< size (caar t)) (cdar t)
              (sel (cdr t))))))
         (l (length e))
         (txt (first e))
         ;; scale
         (sc (if (< 1 l) (second e) 255/1000))
         (sc (if (>= sc 0) (* size sc) (abs sc)))
         (prp (cons
               `((font-size .
                  ,(+ font-size (magnification->font-size sc))))
                props))
         (sil (if txt
                (ly:stencil-aligned-to
                  (interpret-markup layout prp
                    (make-ekm-text-markup txt))
                  Y DOWN)
                #f))
         ;; stretch
         (sil (if (and sil (< 2 l) (third e))
                (ly:stencil-scale sil 1 (/ size (third e)))
                sil)))
    (if (< 6 l)
      (if sil
        ;; lengthen with end/middle segments
        (let* ((x (ly:stencil-extent sil X))
               (w (interval-length x))
               (h (interval-length (ly:stencil-extent sil Y)))
               (end (* h (fourth e)))
               (mid (* h (fifth e)))
               (add (- size h))
               (fitb (cons end (/ (- size mid) 2)))
               (fitt (cons (/ (+ size mid) 2) (- size end)))
               (fitw (cons (* (sixth e) w) (* (seventh e) w)))
               (over (/ (interval-length fitw) 8)))

          (define (mask y)
            (stencil-with-color (make-filled-box-stencil x y) white))

          (ly:stencil-add
            sil
            (ly:stencil-translate-axis sil add Y)
            (mask (cons end (- size end)))
            (ly:stencil-translate-axis sil (/ add 2) Y)
            (mask fitb)
            (mask fitt)
            (make-filled-box-stencil fitw (interval-widen fitb over))
            (make-filled-box-stencil fitw (interval-widen fitt over))))

        ;; lengthen with bottom/top-text
        (let* ((eb (interpret-markup layout prp
                     (make-ekm-text-markup (fourth e))))
               (et (interpret-markup layout prp
                     (make-ekm-text-markup (fifth e))))
               (fitw (cons (* (sixth e) sc) (* (seventh e) sc)))
               (over (/ (interval-length fitw) 8))
               (sil (ly:stencil-add
                      eb
                      (ly:stencil-translate-axis et size Y)
                      (make-filled-box-stencil fitw
                        (interval-widen (cons 0 size) over)))))
          (ly:stencil-outline
            sil
            (make-filled-box-stencil fitw (ly:stencil-extent sil Y)))))
      sil)))

#(define (ekm-system-start grob)
  (let* ((style (ly:grob-property grob 'style))
         (line (ly:grob-set-property! grob 'style 'bar-line))
         (line (ly:system-start-delimiter::print grob)))
    (if (ly:stencil? line)
      (let* ((lext (ly:stencil-extent line Y))
             (size (interval-length lext))
             (sil (grob-interpret-markup grob
                    (make-ekm-system-start-markup style size)))
             (ext (ly:stencil-extent sil Y)))
        (ly:stencil-translate-axis
          sil
          (- (car lext) (car ext) (/ (- (interval-length ext) size) 2))
          Y))
      empty-stencil)))


%% Dynamics

%% Dynamics mapped onto name (or single letter of name):
%%  ("name" . cp)
#(define ekm-dynamic-tab '(
  ("p" . #xE520)
  ("m" . #xE521)
  ("f" . #xE522)
  ("r" . #xE523)
  ("s" . #xE524)
  ("z" . #xE525)
  ("n" . #xE526)
  ("mp" . #xE52C)
  ("mf" . #xE52D)
  ("pf" . #xE52E)
  ("fp" . #xE534)
  ("pp" . #xE52B)
  ("ff" . #xE52F)
  ("ppp" . #xE52A)
  ("fff" . #xE530)
  ("pppp" . #xE529)
  ("ffff" . #xE531)
  ("ppppp" . #xE528)
  ("fffff" . #xE532)
  ("pppppp" . #xE527) ;; not used
  ("ffffff" . #xE533) ;; not used
  ("fz" . #xE535)
  ("sf" . #xE536)
  ("sfp" . #xE537)
  ("sfpp" . #xE538)
  ("sfz" . #xE539)
  ("sfzp" . #xE53A)
  ("sffz" . #xE53B)
  ("sfffz" . #xF6F4)
  ("sffffz" . #xF6F5)
  ("rf" . #xE53C)
  ("rfz" . #xE53D)
  ("sff" . #xF645)
  ("sp" . #xF646)
  ("spp" . #xF647)
))

#(define-markup-command (ekm-dynamic layout props def)
  (string?)
  (interpret-markup layout props
    (let ((c (assoc-ref ekm-dynamic-tab def)))
      (if c
        (make-ekm-char-markup c)
        (make-ekm-def-markup ekm-dynamic-tab def)))))

#(define (ekm-dyntext grob)
  (let* ((mk (ly:grob-property grob 'text)))
    (grob-interpret-markup grob
      (if (string? mk) (make-ekm-dynamic-markup mk) mk))))

ekmParensDyn =
#(define-event-function (style dyn)
  (symbol? ly:event?)
  (let ((p (ekm-asst ekm-parens-tab style 't 0)))
    (make-music 'AbsoluteDynamicEvent
      'text
      (markup #:concat (
        #:normal-text #:italic (car p)
        #:hspace 0.3
        #:ekm-dynamic (ly:music-property dyn 'text)
        #:hspace 0.3
        #:normal-text #:italic (cdr p))))))

%% after lsr.di.unimi.it/LSR/Item?id=771
ekmParensHairpin =
#(define-music-function (style)
  (symbol?)
  #{
    \once \override Hairpin.stencil =
    #(lambda (grob)
      (let* ((p (ekm-asst ekm-parens-tab style 'h 0))
             (l (ekm-ctext grob 2 (car p)))
             (r (ekm-ctext grob 2 (cdr p)))
             (x (+ (ekm-extent l X) 0.6)))
        (ly:grob-set-property! grob 'shorten-pair (cons x x))
        (ly:stencil-combine-at-edge
          (ly:stencil-combine-at-edge
            l X RIGHT (ly:hairpin::print grob) 0.6) X RIGHT r 0.6)))
  #})


%% Scripts

%% Scripts mapped onto name (car of script-stencil):
%%  ("name" text-up . text-down)
%%  ("name" text)
#(define ekm-script-tab '(
  ("sforzato" #xE4A0 . #xE4A1) ;; accent
  ("espr" #xED40 . #xED41) ;; espressivo
  ("dmarcato" #xE4AC . #xE4AD)
  ("uportato" #xE4B2 . #xE4B3)
  ("dstaccatissimo" #xE4A6 . #xE4A7)
  ("staccato" #xE4A2 . #xE4A3)
  ("tenuto" #xE4A4 . #xE4A5)
  ("trill" #xE566)
  ("prall" #xE56C)
  ("mordent" #xE56D)
  ("prallmordent" #xE5BD)
  ("upprall" (#xE59A #xE59D #xE59D #xE59E))
  ("downprall" #xE5C6)
  ("upmordent" #xE5B8)
  ("downmordent" #xE5C7)
  ("lineprall" #xE5B2)
  ("prallprall" #xE56E)
  ("pralldown" #xE5C8)
  ("prallup" (#xE59D #xE59D #xE59D #xE5A4))
  ("turn" #xE567)
  ("reverseturn" #xE568)
  ("slashturn" #xE569)
  ("haydnturn" #xE56F)
  ("upbow" #xE612)
  ("downbow" #xE610)
  ("flageolet" #xE614)
  ("snappizzicato" #xE631 . #xE630)
  ("open" #xF63C)
  ("halfopen" #xF63D)
  ("halfopenvertical" #xF63E)
  ("stopped" #xE633)
  ("upedalheel" #xE661)
  ("dpedalheel" #xE662)
  ("upedaltoe" #xE664)
  ("dpedaltoe" #xE665)
  ("dfermata" #xE4C0 . #xE4C1)
  ("dshortfermata" #xE4C4 . #xE4C5)
  ("dlongfermata" #xE4C6 . #xE4C7)
  ("dveryshortfermata" #xE4C2 . #xE4C3)
  ("dverylongfermata" #xE4C8 . #xE4C9)
  ("dextrashortfermata" #xF69E . #xF69F)
  ("dextralongfermata" #xF6A0 . #xF6A1)
  ("dhenzeshortfermata" #xE4CC . #xE4CD)
  ("dhenzelongfermata" #xE4CA . #xE4CB)
  ("lcomma" #xE4CE . #xF63F)
  ("lvarcomma" #xF640 . #xF641)
  ("segno" #xE047)
  ("coda" #xE048)
  ("varcoda" #xE049)
))

#(define (ekm-script grob)
  (let* ((dir (ly:grob-property grob 'direction))
         (d (ly:grob-property grob 'details #f)))
    (ekm-ctext grob 1
      (if d
        (ekm-asst d #f #f dir)
        (ekm-asst ekm-script-tab #f
          (cadr (ly:grob-property grob 'script-stencil)) dir)))))

ekmScript =
#(define-music-function (name txt)
  (symbol? ekm-extext?)
  (make-articulation name
    'tweaks `((details . ,txt))))

ekmScriptSmall =
#(define-music-function (name txt)
  (symbol? ekm-extext?)
  (make-articulation name
    'tweaks `((details . ,txt) (font-size . -3))))


%% Trill span

#(define (ekm-trillspan grob)
  (let* ((org (ly:grob-original grob))
         (sib (if (ly:grob? org) (ly:spanner-broken-into org) '()))
         (lbnd (assq-ref (ly:grob-property grob 'left-bound-info) 'X))
         (rbnd (assq-ref (ly:grob-property grob 'right-bound-info) 'X))
         (tr (ekm-cchar grob 0 #xE566))
         (tempo (ly:grob-property grob 'text))
         (tempo (if (integer? tempo) (min 4 (max -4 tempo)) 0))
         (seg (- #xEAA4 tempo))
         (sil (ly:stencil-combine-at-edge
                tr X RIGHT
                (grob-interpret-markup grob
                  (make-ekm-chars-markup (make-list
                    (inexact->exact (floor (/ (- rbnd lbnd (ekm-extent tr X))
                      (ekm-extent (ekm-cchar grob 0 seg) X))))
                    seg)))
                0)))
    (if (or (null? sib) (eq? grob (car sib)))
      sil
      (ly:stencil-translate-axis sil
        (- lbnd
           (ly:output-def-lookup (ly:grob-layout grob) 'short-indent)
           1)
        X))))

ekmStartTrillSpan =
#(define-event-function (tempo)
  (integer?)
  (make-music 'TrillSpanEvent
    'span-direction START
    'tweaks `((text . ,tempo))))


%% Trill pitch

#(define (ekm-trillpitch-head grob)
  (grob-interpret-markup grob
    (ekm-note
      (ly:grob-property grob 'style)
      (ly:grob-property grob 'duration-log)
      UP)))

#(define (ekm-calc-parenthesis-stencils grob)
  (let* ((parens (ekm-asst ekm-parens-tab
                   (ly:grob-property grob 'style) 'a 0)))
    (list (ekm-ctext grob 1 (car parens))
          (ekm-ctext grob 1 (cdr parens)))))

ekmPitchedTrill =
#(define-music-function (head parens main aux)
  (symbol? symbol? ly:music? ly:music?)
  #{
    \once \override TrillPitchHead.style = #head
    \once \override TrillPitchParentheses.style = #parens
    \pitchedTrill $main $aux
  #})


%% Laissez vibrer

#(define (ekm-lvtie grob)
  (let* ((d (ly:grob-property grob 'direction))
         (p (fourth (ly:grob-property grob 'control-points)))
         (x (cdr (ly:grob-property grob 'minimum-X-extent '(0 . 0))))
         (c (cond
              ((= 0 x) #xE4BA)
              ((< 7 x) #xF6FE)
              (else #xF6FC)))
         (sil (ekm-cchar grob 0 (if (< 0 d) c (1+ c)))))
    (ly:stencil-translate
      sil (cons (- (car p) 1.2) (cdr p)))))

ekmLaissezVibrer =
#(define-event-function (size)
  (integer?)
  (make-music 'LaissezVibrerEvent
    'tweaks
    `((minimum-X-extent .
       ,(cons 0 (case size ((1) 5.5) ((2) 8) (else 0)))))))


%% Breathing signs and Caesuras

ekmBreathing =
#(define-music-function (txt)
  (ekm-extext?)
  (make-music 'BreathingEvent
    'tweaks
    `((text .
      ,(make-translate-scaled-markup '(0 . -0.5)
        (make-ekm-text-markup txt))))))


%% Colon (repeat) bar lines

#(define (make-ekm-colon-bar-line grob ext)
  (ekm-cchar grob 2 #xE043))


%% Segno bar lines
%% see scm/bar-line.scm

%% Segno serpent for type index:
%%  (kern-scale text)
#(define ekm-segno-tab '#(
  (0.5 #xE04A)
  (0.5 #xE04A 1)
  (1.0 #xE04A 2)
))

#(define ((make-ekm-segno-bar-line type show) grob ext)
  (let* ((th (layout-line-thickness grob))
         (bth (* (ly:grob-property grob 'hair-thickness 1) th))
         (bext (bar-line::widen-bar-extent-on-span grob ext))
         (bar (bar-line::draw-filled-box (cons 0 bth) bext bth bext grob))
         (d (vector-ref ekm-segno-tab type))
         (kern (* (ly:grob-property grob 'segno-kern 1) th (car d)))
         (segno (ekm-ctext grob 3 (cdr d))))
    (ly:stencil-add
      (if show
        segno
        (ly:make-stencil "" (ly:stencil-extent segno X) '(0 . 0)))
      (ly:stencil-translate-axis
        (ly:stencil-combine-at-edge bar X LEFT bar kern)
        (* 1/2 kern)
        X))))

#(define (ekm-segno-init)
  (add-bar-glyph-print-procedure "S" (make-ekm-segno-bar-line 0 #t))
  (add-bar-glyph-print-procedure "s" (make-ekm-segno-bar-line 1 #t))
  (add-bar-glyph-print-procedure "$" (make-ekm-segno-bar-line 2 #t))
  (add-bar-glyph-print-procedure "=" (make-ekm-segno-bar-line 0 #f))
  (add-bar-glyph-print-procedure "#" (make-ekm-segno-bar-line 2 #f))
  (define-bar-line "s" "||" "s" "=")
  (define-bar-line "s-|" "|" "s" "=")
  (define-bar-line "s-s" "s" #f "=")
  (define-bar-line ":|.s" ":|." "s" " |.")
  (define-bar-line ":|.s-s" ":|.s" "" " |.")
  (define-bar-line "s.|:" "|" "s.|:" " .|")
  (define-bar-line "s.|:-s" "s" ".|:" " .|")
  (define-bar-line ":|.s.|:" ":|." "s.|:" " |. .|")
  (define-bar-line ":|.s.|:-s" ":|.s" ".|:" " |. .|")
  (define-bar-line "$" "||" "$" "#")
  (define-bar-line "$-|" "|" "$" "#")
  (define-bar-line "$-$" "$" #f "#")
  (define-bar-line ":|.$" ":|." "$" " |.")
  (define-bar-line ":|.$-$" ":|.$" "" " |.")
  (define-bar-line "$.|:" "|" "$.|:" " .|")
  (define-bar-line "$.|:-$" "$" ".|:" " .|")
  (define-bar-line ":|.$.|:" ":|." "$.|:" " |. .|")
  (define-bar-line ":|.$.|:-$" ":|.$" ".|:" " |. .|"))


%% Percent repeats

#(define ((ekm-percent type) grob)
  (case type
    ((1) ;; slash
      (let ((cnt (ly:event-property (event-cause grob) 'slash-count)))
        (ly:stencil-combine-at-edge
          point-stencil
          X RIGHT
          (grob-interpret-markup grob
            (make-ekm-chars-markup (make-list cnt #xE504)))
          1.3)))
    ((3) ;; percent (see measure-counter-stencil in output-lib.scm)
      (let* ((lb (ly:spanner-bound grob LEFT))
             (rb (ly:spanner-bound grob RIGHT))
             (refp (ly:grob-common-refpoint lb rb X))
             (sp (ly:grob-property grob
                   'spacing-pair
                   '(break-alignment . break-alignment)))
             (l (ly:paper-column::break-align-width lb (car sp)))
             (r (ly:paper-column::break-align-width rb (cdr sp))))
        (ly:stencil-translate-axis
          (ekm-cchar grob 3 #xE500)
          (+ (* 0.5 (- (car r) (cdr l)))
            (- (cdr l) (ly:grob-relative-coordinate grob refp X)))
          X)))
    (else ;; double slash/percent
      (ekm-cchar grob (if (= 2 type) 0 3) #xE501))))


%% Tremolo marks

#(define (ekm-repeat-tremolo grob)
  (let* ((sh (ly:grob-property grob 'shape))
         (c (min (ly:grob-property grob 'flag-count) 5))
         (stm (ly:grob-parent grob X))
         (d (ly:grob-property stm 'direction))
         (y (if (eq? 'beam-like sh)
              (* (1- c) -0.4 d)
              (* (- (interval-length (ly:grob-property stm 'Y-extent)) 1.96) -0.5 d))))
    (ly:stencil-translate
      (grob-interpret-markup grob
        (case sh
          ((ekm) (ly:grob-property grob 'text))
          ((fingered) (make-ekm-char-markup  (+ c #xE224)))
          (else (make-ekm-char-markup (+ c #xE21F)))))
      (cons 0 y))))

%% Tremolo marks mapped onto style name:
%%  ("name" . text)
#(define ekm-tremolo-tab '(
  ("buzzroll" . #xE22A)
  ("penderecki" . #xE22B)
  ("unmeasured" . #xE22C)
  ("unmeasuredS" . #xE22D)
  ("stockhausen" . #xE232)
))

ekmTremolo =
#(define-music-function (txt music)
  (ekm-extext? ly:music?)
  #{
    \override StemTremolo.shape = #'ekm
    \override StemTremolo.text = #(make-ekm-ctext-markup 3
      (or (and (string? txt) (assoc-ref ekm-tremolo-tab txt)) txt))
    $music
    \revert StemTremolo.shape
    \revert StemTremolo.text
  #})


%% Symbols on stem

#(define ((ekm-stem txt) grob)
  (let ((stm (ly:stem::print grob))
        (sym (grob-interpret-markup grob (make-ekm-ctext-markup 1 txt)))
        (d (ly:grob-property grob 'direction)))
    (if (null? stm)
      empty-stencil
      ;; set center of symbol relative to stem start at 1.5 + 0.3366 = 1.8366
      (ly:stencil-combine-at-edge
        stm Y (- d) sym (- (* (ekm-extent sym Y) -0.5) 1.8366)))))

%% Stem symbols mapped onto style name:
%%  ("name" . text)
#(define ekm-stem-tab '(
  ("sprechgesang" . #xE645)
  ("halbGesungen" . #xE64B)
  ("sussurando" . #xE646)
  ("bowBehindBridge" . #xE618)
  ("bowOnBridge" . #xE619)
  ("bowOnTailpiece" . #xE61A)
  ("fouette" . #xE622)
  ("vibrato" . #xE623)
  ("damp" . #xE63B)
  ("stringNoise" . #xE694)
  ("multiphonics" . #xE607)
  ("deadNote" . #xE80D)
  ("crush" . #xE80C)
  ("rimShot" . #xE7FD)
  ("swish" . #xE808)
  ("turnRight" . #xE809)
  ("turnLeft" . #xE80A)
  ("turnRightLeft" . #xE80B)
))

ekmStem =
#(define-music-function (txt music)
  (ekm-extext? ly:music?)
  #{
    \override Stem.stencil = #(ekm-stem
      (or (and (string? txt) (assoc-ref ekm-stem-tab txt)) txt))
    $music
    \revert Stem.stencil
  #})


%% Arpeggios

#(define (ekm-arpeggio grob)
  (let* ((p (ly:grob-property grob 'positions))
         (l (- (cdr p) (car p)))
         (c (inexact->exact (ceiling l)))
         (d (ly:grob-property grob 'arpeggio-direction))
         ;(y (+ (car p) (if (eq? -1 d) -1.5 -0.5)))
         (y (- (car p) 0.4)))
    (ly:stencil-translate
      (ly:stencil-rotate
        (grob-interpret-markup grob
          (make-ekm-chars-markup
            (case d
              ((1) (append! (make-list (- c 2) #xEAA9) '(#xEAAD))) ; up
              ((-1) (cons* #xEAAE (make-list (- c 2) #xEAAA))) ; down
              (else (make-list c #xEAA9)))))
        90 -1 0)
      ;; for left bearing -60 upm
      (cons 0.25 y))))


%% Ottavation

%% Ottavation symbols mapped onto definition key:
%%  ("definition" . cp)
#(define ekm-ottavation-tab '(
  ("8va" . #xE512)
  ("8vb" . #xE51C)
  ("8ba" . #xE513)
  ("8^va" . #xE511)
  ("8^vb" . #xF652)
  ("8" . #xE510)
  ("15ma" . #xE516)
  ("15mb" . #xE51D)
  ("15^ma" . #xE515)
  ("15^mb" . #xF653)
  ("15" . #xE514)
  ("22ma" . #xE519)
  ("22mb" . #xE51E)
  ("22^ma" . #xE518)
  ("22^mb" . #xF654)
  ("22" . #xE517)
  ("29ma" . #xF6FA)
  ("29mb" . #xF6FB)
  ("29^ma" . #xF6F9)
  ("29^mb" . #xF655)
  ("29" . #xF6F8)
  ("(" . #xE51A)
  (")" . #xE51B)
  ("bassa" . #xE51F)
  ("loco" . #xEC90)
  ("^a" . #xEC92)
  ("a" . #xEC91)
  ("^b" . #xEC94)
  ("b" . #xEC93)
  ("^m" . #xEC96)
  ("m" . #xEC95)
  ("^v" . #xEC98)
  ("v" . #xEC97)
))

#(define-markup-command (ekm-ottavation layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup ekm-ottavation-tab def)))

#(define-public ekm-ottavation-numbers
  (map ekm-cdr-text
  '((1 . #xE510)
    (-1 . #xE510)
    (2 . #xE514)
    (-2 . #xE514)
    (3 . #xE517)
    (-3 . #xE517)
    (4 . #xF6F8)
    (-4 . #xF6F8))))

#(define-public ekm-ottavation-ordinals
  (map ekm-cdr-text
  '((1 . #xE511)
    (-1 . #xE512)
    (2 . #xE515)
    (-2 . #xE516)
    (3 . #xE518)
    (-3 . #xE519)
    (4 . #xF6F9)
    (-4 . #xF6FA))))

#(define-public ekm-ottavation-simple-ordinals
  (map ekm-cdr-text
  '((1 . (#xE510 #xEC97 #xEC91))
    (-1 . #xE51C)
    (2 . (#xE514 #xEC95 #xEC91))
    (-2 . #xE51D)
    (3 . (#xE517 #xEC95 #xEC91))
    (-3 . #xE51E)
    (4 . (#xF6F8 #xEC95 #xEC91))
    (-4 . #xF6FB))))

#(define-public ekm-ottavation-ordinals-b
  (map ekm-cdr-text
  '((1 . #xE511)
    (-1 . #xE51C)
    (2 . #xE515)
    (-2 . #xE51D)
    (3 . #xE518)
    (-3 . #xE51E)
    (4 . #xF6F9)
    (-4 . #xF6FB))))

#(define-public ekm-ottavation-ordinals-bassa
  (map ekm-cdr-text
  '((1 . #xE511)
    (-1 . (#xE512 #x2009 #xE51F))
    (2 . #xE515)
    (-2 . (#xE516 #x2009 #xE51F))
    (3 . #xE518)
    (-3 . (#xE519 #x2009 #xE51F))
    (4 . #xF6F9)
    (-4 . (#xF6FA #x2009 #xE51F)))))

#(define-public ekm-ottavation-ordinals-ba
  (map ekm-cdr-text
  '((1 . #xE511)
    (-1 . #xE513)
    (2 . #xE515)
    (-2 . (#xE514 #xEC93 #xEC91))
    (3 . #xE518)
    (-3 . (#xE517 #xEC93 #xEC91))
    (4 . #xF6F9)
    (-4 . (#xF6F8 #xEC93 #xEC91)))))

#(define-public ekm-ottavation-numbers-ba
  (map ekm-cdr-text
  '((1 . #xE510)
    (-1 . #xE513)
    (2 . #xE514)
    (-2 . (#xE514 #xEC93 #xEC91))
    (3 . #xE517)
    (-3 . (#xE517 #xEC93 #xEC91))
    (4 . #xF6F8)
    (-4 . (#xF6F8 #xEC93 #xEC91)))))


%% Tuplet numbers
%% see output-lib.scm

#(define (ekm-tuplet-list num grob prop)
  (ekm-number->list '() #xE880
    (or num (ly:event-property (event-cause grob) prop))))

#(define ((ekm-tuplet-number num denom) grob)
  (let* ((ev (event-cause grob))
         (l (if (eqv? 0 denom)
              '()
              (cons* #x200A #xE88A #x200A
                (ekm-tuplet-list denom grob 'numerator)))))
    (make-ekm-chars-markup
      (append! (ekm-tuplet-list num grob 'denominator) l))))

#(define-markup-command (ekm-tuplet-note layout props dur)
  (ly:duration?)
  (interpret-markup layout props
    (make-fontsize-markup -5
      (make-ekm-note-by-number-markup
        'metronome
        (ly:duration-log dur)
        (ly:duration-dot-count dur)
        UP))))

#(define (ekm-tuplet-number::calc-denominator-text grob)
  ((ekm-tuplet-number #f 0) grob))

#(define (ekm-tuplet-number::calc-fraction-text grob)
  ((ekm-tuplet-number #f #f) grob))

#(define ((ekm-tuplet-number::non-default-tuplet-denominator-text num) grob)
  ((ekm-tuplet-number num 0) grob))

#(define ((ekm-tuplet-number::non-default-tuplet-fraction-text num denom) grob)
  ((ekm-tuplet-number num denom) grob))

#(define ((ekm-tuplet-number::append-note-wrapper fmt dur) grob)
  (let ((num (and fmt (fmt grob)))
        (note (make-ekm-tuplet-note-markup dur)))
    (if num (make-line-markup (list num note)) note)))

#(define ((ekm-tuplet-number::non-default-fraction-with-notes
           num numdur denom denomdur) grob)
  (make-concat-markup (list
    (make-ekm-chars-markup
      (ekm-tuplet-list num grob 'denominator))
    (make-ekm-tuplet-note-markup numdur)
    (make-ekm-chars-markup
      (cons* #x2009 #xE88A #x2009
        (ekm-tuplet-list denom grob 'numerator)))
    (make-ekm-tuplet-note-markup denomdur))))

#(define ((ekm-tuplet-number::fraction-with-notes
           numdur denomdur) grob)
  ((ekm-tuplet-number::non-default-fraction-with-notes
    #f numdur #f denomdur) grob))


%% Fingering

%% Fingering symbols mapped onto definition key:
%%  ("definition" . cp)
%% Note: p i m a x used by StrokeFinger
#(define ekm-finger-tab '(
  ;; italic glyphs
  ("0" . #xED80)
  ("1" . #xED81)
  ("2" . #xED82)
  ("3" . #xED83)
  ("4" . #xED84)
  ("5" . #xED85)
  ("6" . #xED86)
  ("7" . #xED87)
  ("8" . #xED88)
  ("9" . #xED89)
  ("(" . #xED8A)
  (")" . #xED8B)
  ("[" . #xED8C)
  ("]" . #xED8D)
  ;; non-italic glyphs
  ("0" . #xED10)
  ("1" . #xED11)
  ("2" . #xED12)
  ("3" . #xED13)
  ("4" . #xED14)
  ("5" . #xED15)
  ("6" . #xED24)
  ("7" . #xED25)
  ("8" . #xED26)
  ("9" . #xED27)
  ("th" . #xE624)
  ("ht" . #xE625)
  ("p" . #xED17)
  ("i" . #xED19)
  ("m" . #xED1A)
  ("a" . #xED1B)
  ("x" . #xED1D)
  ("T" . #xED16)
  ("t" . #xED18)
  ("c" . #xED1C)
  ("e" . #xED1E)
  ("o" . #xED1F)
  ("q" . #xED8E)
  ("s" . #xED8F)
  ("(" . #xED28)
  (")" . #xED29)
  ("[" . #xED2A)
  ("]" . #xED2B)
  ("." . #xED2C)
  ("," . #xED2D)
  ("/" . #xED2E)
  ("~~" . #xED20)
  ("~" . #xED21)
  ("-" . #xED22)
  ("M" . #xED23)
  ("RE" . #xE66F)
  ("R" . #xE66E)
  ("LE" . #xE671)
  ("L" . #xE670)
))

#(define-markup-command (ekm-finger layout props def)
  (string?)
  (let ((it (eqv? #\* (string-ref def 0))))
    (interpret-markup layout props
      (make-ekm-def-markup
        (if it ekm-finger-tab (list-tail ekm-finger-tab 14))
        (if it (string-drop def 1) def)))))

#(define ((ekm-fingering size) grob)
  (let ((def (ly:grob-property grob 'text))
        (shp (ly:grob-property grob 'font-shape)))
    (if (string? def)
      (grob-interpret-markup grob
        (make-fontsize-markup (+ size 5)
          (make-ekm-finger-markup
            (if (eq? 'italic shp) (string-append "*" def) def))))
      (ly:text-interface::print grob))))

ekmPlayWith =
#(define-music-function (hand start music)
  (ly:dir? boolean? ly:music?)
  #{
    \set fingeringOrientations = #(if start '(left) '(right))
    \override Fingering.padding = #0.2
    #(music-map
      (lambda (m)
        (if (music-is-of-type? m 'note-event)
          (begin
            (ly:music-set-property! m 'articulations
              (cons
                (make-music 'FingeringEvent 'text
                  (if (= LEFT hand)
                    (if start "L" "LE")
                    (if start "R" "RE")))
                (ly:music-property m 'articulations)))
            (make-music 'EventChord
              'elements (list m)))
          m))
      music)
    \revert Fingering.padding
    \unset fingeringOrientations
  #})


%% String number indications

%% String number symbols for number 0-13:
#(define ekm-stringnumber-tab '#(
  #xE833
  #xE834
  #xE835
  #xE836
  #xE837
  #xE838
  #xE839
  #xE83A
  #xE83B
  #xE83C
  #xE84A
  #xE84B
  #xE84C
  #xE84D))

#(define-markup-command (ekm-string-number layout props txt)
  (number-or-string?)
  (let* ((num (if (number? txt) txt (string->number txt 10)))
         (cp (and num (<= 0 num 13)
                  (vector-ref ekm-stringnumber-tab (round num)))))
    (interpret-markup layout props
      (if cp
        (make-fontsize-markup 3 (make-ekm-char-markup cp))
      (if num
        (make-circle-markup (number->string num 10))
        (make-italic-markup txt))))))

#(define (ekm-stringnumber grob)
  (grob-interpret-markup grob
    (make-ekm-string-number-markup
      (ly:grob-property grob 'text))))


%% Piano pedals

%% Pedal symbols mapped onto definition key:
%%  ("key" . text)
#(define ekm-pedal-tab '(
  ("Ped." . #xE650)
  ("Ped" . #xF434)
  ("P" . #xE651)
  ("e" . #xE652)
  ("d" . #xE653)
  ("Sost." . #xE659)
  ("Sost" . #xF435)
  ("Sos." . #xF6D1)
  ("sos." . #xF6D0)
  ("S" . #xE65A)
  ("unacorda" . #xF6CC)
  ("trecorde" . #xF6CD)
  ("u.c." . #xF6CE)
  ("t.c." . #xF6CF)
  ("." . #xE654)
  ("-" . #xE658)
  ("*" . #xE655)
  ("o" . #xE65D)
  ("," . #xE65B)
  ("'" . #xE65C)
  ("H" . #xE656)
  ("^" . #xE657)
  ("1/2Ped" . #xF6B0)
  ("|1/4" . #xF6BA)
  ("|1/2" . #xF6BB)
  ("|3/4" . #xF6BC)
  ("|1" . #xF6BD)
  ("l" . #xE65E)
  ("m" . #xE65F)
  ("r" . #xE660)
  ("(" . #xE676)
  (")" . #xE677)
))

#(define-markup-command (ekm-piano-pedal layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup ekm-pedal-tab def)))

#(define (ekm-pedal grob)
  (grob-interpret-markup grob
    (make-ekm-piano-pedal-markup (ly:grob-property grob 'text))))


%% Harp pedals

%% Harp pedal symbols mapped onto definition key:
%%  ("key" . text)
#(define ekm-harp-pedal-tab '(
  ("^"  . #xE680)
  ("o^" . #xF648)
  ("-"  . #xE681)
  ("o-" . #xF649)
  ("v"  . #xE682)
  ("ov" . #xF64A)
  ("|"  . #xE683)
  (" "  . #f)
))

#(define-markup-command (ekm-harp-pedal layout props def)
  (string?)
  (let* ((p (ekm-def-list ekm-harp-pedal-tab def #t))
         (l (length (car p)))
         (d (fold (lambda (k i r) (if (string=? "|" k) (cons* i r) r))
                  '() (car p) (iota l)))
         (l (- l (length d))))
    (if (not (= 7 l))
      (ly:warning "Harp pedal diagram contains ~a pedals rather than the usual 7." l))
    (if (null? d)
      (ly:warning "Harp pedal diagram does not contain a divider (usually one after third pedal).")
    (if (not (equal? '(3) d))
      (ly:warning "Harp pedal diagram contains dividers at positions ~a (usually one after third pedal)."
        (reverse d))))
    (stack-stencil-line 0
      (interpret-markup-list layout props (cdr p)))))


%% Fret diagrams

#(define-markup-command (ekm-fret-diagram-terse layout props def)
  (string?)
  (let* ((defl (string-split def #\73))
         (cnt (1- (max 4 (min 7 (length defl)))))
         (board (ekm:char layout props (+ (* cnt 2) #xE84B)))
         (thick (ly:output-def-lookup layout 'line-thickness))
         (w (/ (- (ekm-extent board X) 0.064) (1- cnt)))
         (h (ly:stencil-extent board Y))
         (ynum (- (car h) 0.5))
         (h (/ (- (cdr h) 0.064) 4))
         (ymark (+ (* h 4) 0.44 0.1))
         (bow-beg #f)
         (bow-end #f)
         (i 0)
         (dl (map (lambda (d)
              (let* ((pos (string-split d #\-))
                     (num (if (> (length pos) 1) (string->number (second pos)) #f))
                     (x (+ (* w i) 0.032)))
                (set! i (1+ i))
                (if (<= i cnt)
                  (cond
                    ((string=? "o" (car pos)) (list #xE85A x ymark #f))
                    ((string=? "x" (car pos)) (list #xE859 x ymark #f))
                    (else
                      (let ((y (+ (* h (- 4 (max 1 (min 4
                                 (or (string->number (car pos)) 1))))) 0.44)))
                        (if (> (length pos) 2)
                          (cond
                            ((string=? "(" (third pos)) (set! bow-beg (cons x y)))
                            ((string=? ")" (third pos)) (set! bow-end (cons x y)))))
                        (list #xE858 x y (if num (max 1 (min 9 num)) #f)))))
                  #f)))
              defl)))
    (fold
      (lambda (d sil)
        (if d
          (ly:stencil-add
            sil
            (ly:stencil-translate
              (ekm:char layout props (car d))
              (cons (second d) (third d)))
            (if (fourth d)
              (ly:stencil-translate
                (centered-stencil
                  (interpret-markup layout props
                    (make-whiteout-markup
                      (make-fontsize-markup -7
                        ;(make-ekm-char-markup (+ (fourth d) #xE833))
                        (make-sans-markup (number->string (fourth d)))))))
                (cons (second d) ynum))
              point-stencil))
          sil))
      (if (and bow-beg bow-end)
        (ly:stencil-add
          (make-bow-stencil bow-beg bow-end thick 0.5 0.3 1)
          board)
        board)
      dl)))


%% Accordion registers
%% replaces scm/accreg.scm

%% Accordion registers mapped onto style and name:
%%  (style register-entry ...)
%% register-entry:
%%  ("name" . cp)
%%  ("name" . (cp-empty (dot-x . dot-y) ...))
%% dot-x,dot-y:
%%  Position in percent of cp-empty width/height.
#(define ekm-accordion-tab '(
  (d ;; \discant
    ("1" . #xE8A4)
    ("10" . #xE8A1)
    ("11" . #xE8AB)
    ("1+0" . #xE8A2)
    ("1+1" . (#xE8C6 (76 . 50) (50 . 18)))
    ("1-0" . #xE8A3)
    ("1-1" . (#xE8C6 (24 . 50) (50 . 18)))
    ("20" . #xE8AE)
    ("21" . #xE8AF)
    ("2+0" . #xE8A6)
    ("2+1" . #xE8AC)
    ("2-0" . (#xE8C6 (24 . 50) (50 . 50)))
    ("2-1" . (#xE8C6 (24 . 50) (50 . 50) (50 . 18)))
    ("30" . #xE8A8)
    ("31" . #xE8B1)
    ("100" . #xE8A0)
    ("101" . #xE8A9)
    ("110" . #xE8A5)
    ("111" . #xE8AA)
    ("11+0" . (#xE8C6 (50 . 82) (76 . 50)))
    ("11+1" . (#xE8C6 (50 . 82) (76 . 50) (50 . 18)))
    ("11-0" . (#xE8C6 (50 . 82) (24 . 50)))
    ("11-1" . (#xE8C6 (50 . 82) (24 . 50) (50 . 18)))
    ("120" . #xE8B0)
    ("121" . #xE8AD)
    ("12+0" . #xE8A7)
    ("12+1" . (#xE8C6 (50 . 82) (50 . 50) (76 . 50) (50 . 18)))
    ("12-0" . (#xE8C6 (50 . 82) (24 . 50) (50 . 50)))
    ("12-1" . (#xE8C6 (50 . 82) (24 . 50) (50 . 50) (50 . 18)))
    ("130" . #xE8B2)
    ("131" . #xE8B3))
  (sb ;; \stdBass
    ("Soprano" . #xE8B4)
    ("Alto" . #xE8B5)
    ("Tenor" . #xE8B6)
    ("Master" . #xE8B7)
    ("Soft Bass" . #xE8B8)
    ("Soft Tenor" . #xE8B9)
    ("Bass/Alto" . #xE8BA))
  (sb4 ;; \stdBassIV
    ("Soprano" . #xE8B4)
    ("Alto" . #xE8B5)
    ("Tenor" . (#xE8C7 (50 . 87) (50 . 38)))
    ("Master" . (#xE8C7 (50 . 87) (50 . 62) (50 . 38) (50 . 14)))
    ("Soft Bass" . (#xE8C7 (50 . 62) (50 . 38) (50 . 14)))
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass/Alto" . (#xE8C7 (50 . 62) (50 . 14)))
    ("Soft Tenor" . #xE8B9))
  (sb5 ;; \stdBassV
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass/Alto" . (#xE8C7 (50 . 62) (50 . 14)))
    ("Alto" . (#xE8C7 (38 . 85) (62 . 85) (50 . 62)))
    ("Tenor" . (#xE8C7 (38 . 85) (62 . 85) (50 . 38)))
    ("Master" . (#xE8C7 (38 . 85) (62 . 85) (50 . 62) (50 . 38) (50 . 14)))
    ("Soft Bass" . (#xE8C7 (50 . 62) (50 . 38) (50 . 14)))
    ("Soft Tenor" . #xE8B9)
    ("Soprano" . #xE8B4)
    ("Sopranos" . (#xE8C7 (38 . 85) (62 . 85)))
    ("Solo Bass" . (#xE8C7 (50 . 14))))
  (sb6 ;; \stdBassVI
    ("Soprano" . #xE8B4)
    ("Alto" . (#xE8C7 (50 . 62)))
    ("Soft Tenor" . #xE8B9)
    ("Master" . #xE8B7)
    ("Alto/Soprano" . (#xE8C7 (50 . 87) (26 . 62)))
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass" . #xE8B8))
  (fb ;; \freeBass
    ("10" . #xE8BB)
    ("1" . #xE8BC)
    ("11" . #xE8BD)
    ("Master" . #xE8BE)
    ("Master 1" . #xE8BF)
    ("Master 11" . #xE8C0))
  (sq ;; Square
    ("1" . #xE8C1)
    ("100" . #xE8C2)
    ("2" . #xE8C3)
    ("101" . #xE8C4)
    ("102" . #xE8C5))
))

#(define-markup-command (ekm-accordion layout props name)
  (string?)
  #:properties ((font-size 0))
  (let* ((i (string-index name #\space))
         (st (if (and i (< 0 i)) (string->symbol (string-take name i)) 'd))
         (key (if (and i (< 0 i)) (string-drop name (1+ i)) name))
         (d (ekm-asst ekm-accordion-tab st key 0)))
    (if (ekm-cp? d)
      (ekm-center 1 (ekm:char layout props d))
      (let* ((reg (ekm:char layout props (car d)))
             (dot (ekm-center 3 (ekm:char layout props #xE8CA)))
             (sz (/ (magstep font-size) 100))
             (w (* sz (ekm-extent reg X)))
             (h (* sz (ekm-extent reg Y))))
        (ly:stencil-aligned-to
          (fold
            (lambda (p sil)
              (ly:stencil-add sil
                (ly:stencil-translate dot (cons (* w (car p)) (* h (cdr p))))))
            reg
            (cdr d))
          X CENTER)))))

ekmAccordion =
#(define-music-function (name)
  (string?)
  (make-event-chord (list
    (make-music 'TextScriptEvent
      'direction 1
      'text (make-ekm-accordion-markup name)))))


%% Accordion ricochet

ekmRicochet =
#(define-music-function (num)
  (integer?)
  (make-articulation 'accent
    'tweaks `((details . ,(+ #xE8CB (if (<= 2 num 6) num 2))))))

ekmStemRicochet =
#(define-music-function (num music)
  (integer? ly:music?)
  #{
    \override Stem.stencil = #(ekm-stem (+ #xE8D0 (if (<= 2 num 6) num 2)))
    $music
    \revert Stem.stencil
  #})


%% Falls and doits

%% Brass data mapped onto style and duration log (0,1,2):
%%  (style brass-entry ...)
%% brass-entry:
%%  (log minimum-length align-up cp-down cp-up)
%% align-up:
%%  #t aligns up if direction is upward.
#(define ekm-brass-tab '(
  (bend
    (0 6.3  #f #xE5D6 . #xE5D9)
    (1 4.1  #f #xE5D5 . #xE5D8)
    (2 3.12 #f #xE5D4 . #xE5D7))
  (rough
    (0 7.01 #t #xE5D3 . #xE5DF)
    (1 5.0  #t #xE5D2 . #xE5DE)
    (2 3.49 #t #xE5D1 . #xE5DD))
  (smooth
    (0 7.7  #t #xE5EE . #xE5DC)
    (1 5.2  #t #xE5ED . #xE5DB)
    (2 3.8  #t #xE5EC . #xE5DA))
))

#(define (ekm-accbrass style grob)
  (let ((dur (ly:grob-property (ly:grob-parent grob X) 'duration-log 2)))
    (ekm-asst ekm-brass-tab style (max (min dur 2) 0) 0)))

#(define (ekm-brass style up grob)
  (let* ((d (ekm-accbrass style grob))
         (w (interval-length (ly:grob-property (ly:grob-parent grob X) 'X-extent)))
         (sil (make-ekm-char-markup (if up (cdddr d) (caddr d)))))
    (grob-interpret-markup grob
      (make-translate-markup
        (cons (+ w 0.2) 0)
        (if (and (cadr d) up) (make-general-align-markup Y UP sil) sil)))))

ekmBendAfter =
#(define-event-function (style dir)
  (symbol? ly:dir?)
  (make-music 'BendAfterEvent
    'delta-step dir
    'tweaks
    `((springs-and-rods .
        ,ly:spanner::set-spacing-rods)
      (minimum-length .
        ,(lambda (grob) (car (ekm-accbrass style grob))))
      (stencil .
        ,(lambda (grob) (ekm-brass style (< dir 0) grob))))))


%% Figured bass

%% Bass figure digits 0-9
#(define ekm-fbass-digits
  '#(#xEA50 #xEA51 #xEA52 #xEA54 #xEA55 #xEA57 #xEA5B #xEA5D #xEA60 #xEA61))

%% Bass figure accidentals mapped onto alteration:
%%  (alteration . cp)
#(define ekm-fbass-acc '(
  (0 . #xEA65)
  (-1/2 . #xEA64)
  (1/2 . #xEA66)
  (-1 . #xEA63)
  (1 . #xEA67)
  (-3/2 . #xECC1)
  (3/2 . #xECC2)
))

%% Precomposed digits with + / \ mapped onto digit + flag:
%%  (digit+flag . cp)
%% flag:
%%  #x100 +,  #x200 /,  #x400 \
#(define ekm-fbass-pre '(
  (#x102 . #xEA53) ;; 2\+
  (#x104 . #xEA56) ;; 4\+
  (#x105 . #xEA58) ;; 5\+
  (#x405 . #xEA59) ;; 5\\
  (#x205 . #xEA5A) ;; 5/
  (#x406 . #xEA5C) ;; 6\\
  (#x106 . #xEA6F) ;; 6\+
  (#x107 . #xEA5E) ;; 7\+
  (#x407 . #xEA5F) ;; 7\\
  (#x207 . #xECC0) ;; 7/
  (#x409 . #xEA62) ;; 9\\
))

#(define (ekm-fbass fig ev ctx)
  (let* ((alt (ly:event-property ev 'alteration #f))
         (alt (and alt (assv-ref ekm-fbass-acc alt)))
         (aug (ly:event-property ev 'augmented #f))
         (dim (ly:event-property ev 'diminished #f))
         (bsl (ly:event-property ev 'augmented-slash #f))
         (pre (assv-ref ekm-fbass-pre
                (if (number? fig)
                  (+ fig (cond (aug #x100) (dim #x200) (bsl #x400) (else 0)))
                  0)))
         (adir (ly:context-property ctx 'figuredBassAlterationDirection LEFT))
         (pdir (ly:context-property ctx 'figuredBassPlusDirection LEFT))
         (pfx (if (and alt (= LEFT adir)) `(,alt) '()))
         (pfx (if (and aug (= LEFT pdir) (not pre)) (cons #xEA6C pfx) pfx))
         (sfx (if (and aug (= RIGHT pdir) (not pre)) '(#xEA6C) '()))
         (sfx (if (and alt (= RIGHT adir)) (cons alt sfx) sfx))
         (sil (if pre (make-ekm-cchar-markup 1 pre)
              (if (number? fig) (ekm-fbass-slash dim bsl fig)
              #f))))
    (make-translate-scaled-markup '(0.5 . 0)
      (if sil
        (make-put-adjacent-markup X RIGHT
        (make-put-adjacent-markup X LEFT
          sil (make-ekm-chars-markup pfx))
              (make-ekm-chars-markup sfx))
        (make-center-align-markup
          (make-ekm-chars-markup (append pfx sfx)))))))

#(define (ekm-fbass-slash sl bsl fig)
  (let ((num (make-center-align-markup (make-ekm-number-markup ekm-fbass-digits fig))))
    (if (or sl bsl)
      (make-combine-markup num (make-ekm-cchar-markup 1 (if sl #xEA6D #xEA6E)))
      num)))


%% Lyrics

#(define ekm-lyric? (char-set #\~ #\_ #\45))

#(define ekm-lyric-tab '(
  (#\n . #xE550)
  (#\~ . #xE551)
  (#\w . #xE552)
  (#\_ . #xE553)
  (#\45 . #xE555)
  (#\x . 0)
))

#(define (ekm-lyric-text grob)
  (let ((t (ly:grob-property grob 'text)))
    (grob-interpret-markup grob
      (if (string? t) (make-ekm-tied-lyric-markup t) t))))

#(define-markup-command (ekm-tied-lyric layout props str)
  (string?)
  (interpret-markup layout props
    (let ((ls
      (let split ((l '(("" . #\x))) (i 0) (p #\x))
        (let* ((j (string-index str ekm-lyric? i))
               (pre (substring str i (or j (string-length str))))
               (ch (if j (string-ref str j) #\x))
               (k (if (and (eqv? #\~ ch) (eqv? #\~ p))
                    (cond
                      ((string-null? pre) (set-cdr! (car l) #\x) #\w)
                      ((= j (1+ i)) (set-cdr! (car l) #\n) #\n)
                      (else ch))
                    ch))
               (r (cons* (cons pre k) l)))
          (if (not j) r (split r (1+ j) ch))))))
      (let tie ((r "") (l (reverse ls)))
        (if (null? l) r
          (tie
            (make-concat-markup (list
              r
              (caar l)
              (make-ekm-char-markup (assv-ref ekm-lyric-tab (cdar l)))))
            (cdr l)))))))


%% Analytics

%% Analytics symbols mapped onto definition key:
%%  ("key" . text)
#(define ekm-analytics-tab '(
  ("H" . #xE860)
  ("CH" . #xE86A)
  ("RH" . #xE86B)
  ("N" . #xE861)
  ("[" . #xE862)
  ("]" . #xE863)
  ("Th" . #xE864)
  ("hT" . #xE865)
  ("ihT" . #xE866)
  ("iTh" . #xE867)
  ("T" . #xE868)
  ("iT" . #xE869)
))

#(define-markup-command (ekm-analytics layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup ekm-analytics-tab def)))


%% Function theory

%% Function theory symbols mapped onto definition key:
%%  ("key" . text)
#(define ekm-func-tab `(
  ("0" . #xEA70)
  ("1" . #xEA71)
  ("2" . #xEA72)
  ("3" . #xEA73)
  ("4" . #xEA74)
  ("5" . #xEA75)
  ("6" . #xEA76)
  ("7" . #xEA77)
  ("8" . #xEA78)
  ("9" . #xEA79)
  ("<" . #xEA7A)
  (">" . #xEA7C)
  ("-" . ,(markup #:with-dimensions-from #:ekm-char #xEA70 #:ekm-char #xEA7B))
  ("DD" . #xEA81)
  ("/DD" . #xEA82)
  ("/D" . #xF644) ;; ,(markup #:ekm-combine #xEA7F 0.5 0 #xE87B)
  ("Dp" . ,(markup #:ekm-combine #xEA7F 2.1 -0.6 #xEA88))
  ("Sg" . ,(markup #:ekm-combine #xEA89 2.1 -0.6 #xEA84))
  ("Sp" . ,(markup #:ekm-combine #xEA89 2.1 -0.6 #xEA88))
  ("Tg" . ,(markup #:ekm-combine #xEA8B 1.6 -0.6 #xEA84))
  ("Tp" . ,(markup #:ekm-combine #xEA8B 1.6 -0.6 #xEA88))
  ("D" . #xEA7F)
  ("d" . #xEA80)
  ("SS" . #xEA7D)
  ("ss" . #xEA7E)
  ("S" . #xEA89)
  ("s" . #xEA8A)
  ("F" . #xEA99)
  ("G" . #xEA83)
  ("g" . #xEA84)
  ("I" . #xEA9A)
  ("i" . #xEA9B)
  ("K" . #xEA9C)
  ("k" . #xEA9D)
  ("L" . #xEA9E)
  ("l" . #xEA9F)
  ("M" . #xED00)
  ("m" . #xED01)
  ("N" . #xEA85)
  ;("^N" . #xED02)
  ("n" . #xEA86)
  ("P" . #xEA87)
  ("p" . #xEA88)
  ("r" . #xED03)
  ("T" . #xEA8B)
  ("t" . #xEA8C)
  ("V" . #xEA8D)
  ("v" . #xEA8E)
  ("(" . #xEA91)
  (")" . #xEA92)
  ("[" . #xEA8F)
  ("]" . #xEA90)
  ("{" . #xEA93)
  ("}" . #xEA94)
  ("..+" . #xEA96)
  (".." . #xEA95)
  ("+" . #xEA98)
  ("o" . #xEA97)
  ("bb" . #xED64)
  ("b" . #xED60)
  ("#" . #xED62)
  ("x" . #xED63)
  ("=" . #xED61)
  ("~" . ,(markup #:with-dimensions-from #:ekm-char #xEA70 ""))
))

#(define ekm-func-sep (string->char-set ",^"))
#(define ekm-func-paren (string->char-set "()[]{}"))

#(define (ekm-func layout props size def)
  (interpret-markup layout props
    (markup
      #:fontsize size
      #:ekm-def ekm-func-tab def)))

#(define-markup-command (ekm-func layout props def)
  (string?)
  #:properties ((font-size 0)
                (func-size -4)
                (func-skip 2.5)
                (func-space 0.3))
  (let* ((mag (magstep font-size))
         (skip (* mag func-skip))
         (space (* mag func-space))
         (lpar (eqv? 0 (string-index def ekm-func-paren)))
         (mdef (if lpar (string-drop def 1) def))
         (len (1- (string-length mdef)))
         (rpar (eqv? len (string-rindex mdef ekm-func-paren)))
         (mdef (if rpar (string-drop-right mdef 1) mdef))
         (xtra (string-split mdef #\^))
         (bas (string-split (car xtra) #\,))
         (sil (ekm-func layout props 0 (car bas)))
         (mid (cons (/ (interval-length (ly:stencil-extent sil X)) 2) 0))
         (sxtra (stack-lines UP 0.0 skip
                  (map (lambda (el) (ekm-func layout props func-size el))
                       (cdr xtra))))
         (sbas (if (< 1 (length bas))
                 (ekm-func layout props func-size (second bas)) '()))
         (ssop (if (< 2 (length bas))
                 (ekm-func layout props func-size (third bas)) '()))
         (sil (if (null? sbas)
                sil
                (stack-lines DOWN (/ skip 6) skip (list
                  sil
                  (ly:stencil-translate
                    (ly:stencil-aligned-to sbas X CENTER) mid)))))
         (sil (if (null? ssop)
                sil
                (stack-lines UP (/ skip 6) skip (list
                  sil
                  (ly:stencil-translate
                    (ly:stencil-aligned-to ssop X CENTER) mid)))))
         (sil (if (null? sxtra)
                sil
                (ly:stencil-combine-at-edge sil X RIGHT
                  (ly:stencil-translate
                    (ly:stencil-aligned-to sxtra Y DOWN)
                    ;(cons 0 (if (< 2 (length xtra)) (/ skip 2) (/ skip 1.4)))
                    (cons 0 (/ skip 1.5)))
                  space)))
         (sil (if lpar
                (ly:stencil-combine-at-edge sil X LEFT
                  (ekm-func layout props 0 (string-take def 1))
                  space)
                sil))
         (sil (if rpar
                (ly:stencil-combine-at-edge sil X RIGHT
                  (ekm-func layout props 0 (string-take-right def 1))
                  space)
                sil)))
    sil))

#(define-markup-command (ekm-magfunc layout props size def)
  (number? string?)
  (interpret-markup
    layout
    (cons `((font-size . ,size)) props)
    (make-ekm-func-markup def)))

%% after lsr.di.unimi.it/LSR/Item?id=967
ekmFunc =
#(define-music-function (def)
  (string?)
  (let* ((size -4)
         (len (string-length def))
         (sfx (if (= 0 len) #f (string-ref def (1- len))))
         (mdef (case sfx
                 ((#\- #\. #\+ #\*) (string-drop-right def 1))
                 (else def)))
         (i (string-index mdef ekm-func-sep))
         (mk (markup
               #:override `(font-size . ,size)
               #:ekm-def ekm-func-tab (if i (substring mdef 0 i) mdef))))
    (case sfx
      ((#\-) #{
        \once \override TextSpanner.direction = #DOWN
        \once \override TextSpanner.style = #'line
        \once \override TextSpanner.outside-staff-priority = ##f
        \once \override TextSpanner.thickness = #(* 2.5 (magstep size))
        \once \override TextSpanner.padding =
          #(lambda (grob) (* (ekm-dim grob mk Y) -0.4))
        \once \override TextSpanner.bound-details =
          #(lambda (grob)
           `((left .
               ((Y . 0)
                (padding . ,(+ (* (ekm-dim grob mk X)
                  (if (string-index mdef #\^) 0.55 0.8)) 0.3))
                (attach-dir . ,LEFT)))
             (left-broken .
               ((end-on-note . #t)))
             (right .
               ((Y . 0)
                (padding . 0)
                (attach-dir . ,RIGHT)))))
        \lyricmode { \markup \ekm-magfunc #size #mdef }
        \startTextSpan
      #})
      ((#\.) #{
        \lyricmode { \markup \ekm-magfunc #size #mdef }
        \stopTextSpan
      #})
      ((#\+) #{
        \once \override StanzaNumber.padding = #0.5
        \set stanza = \markup \ekm-magfunc #size #mdef
      #})
      ((#\*) #{
        \once \override StanzaNumber.padding = #0.5
        \set stanza = \markup \ekm-func #mdef
      #})
      (else #{
        \lyricmode { \markup \ekm-magfunc #size #mdef }
      #}))))

ekmFuncList =
#(define-music-function (defs)
  (list?)
  (fold
    (lambda (def music) #{ $music \ekmFunc #def #})
    #{ #}
    defs))


%% Arrows and arrow heads

%% Arrows mapped onto style:
%%  (style . #(cp-N cp-NE cp-E cp-SE cp-S cp-SW cp-W cp-NW cp-NS cp-NESW cp-EW cp-SENW))
%%  (style . #(cp-N cp-NE cp-E cp-SE cp-S cp-SW cp-W cp-NW))
%%  (style . #(cp-N cp-E cp-S cp-W))
%% cp-NESW, cp-SENW:
%%  Currently either equal cp-N or not present.
#(define ekm-arrow-tab '(
  ;; SMuFL arrows
  (black .
    #(#xEB60 #xEB61 #xEB62 #xEB63 #xEB64 #xEB65 #xEB66 #xEB67 #xF6D8 #xEB60 #xF6D9))
  (white .
    #(#xEB68 #xEB69 #xEB6A #xEB6B #xEB6C #xEB6D #xEB6E #xEB6F #xF6DA #xEB68 #xF6DB))
  (open .
    #(#xEB70 #xEB71 #xEB72 #xEB73 #xEB74 #xEB75 #xEB76 #xEB77 #xF6DC #xEB70 #xF6DD))
  ;; Unicode arrows
  (simple .
    #(#x2191 #x2197 #x2192 #x2198 #x2193 #x2199 #x2190 #x2196 #x2195 #x2922 #x2194 #x2921))
  (double .
    #(#x21D1 #x21D7 #x21D2 #x21D8 #x21D3 #x21D9 #x21D0 #x21D6 #x21D5 #x21D1 #x21D4))
  (black-wide .
    #(#x2B06 #x2B08 #x2B95 #x2B0A #x2B07 #x2B0B #x2B05 #x2B09 #x2B0D #x2B06 #x2B0C))
  (white-wide .
    #(#x21E7 #x2B00 #x21E8 #x2B02 #x21E9 #x2B03 #x21E6 #x2B01 #x21F3 #x21E7 #x2B04))
  (triangle .
    #(#x2B61 #x2B67 #x2B62 #x2B68 #x2B63 #x2B69 #x2B60 #x2B66 #x2B65 #x2B61 #x2B64))
  (triangle-bar .
    #(#x2B71 #x2B77 #x2B72 #x2B78 #x2B73 #x2B79 #x2B70 #x2B76 #xF6DE #x2B71 #xF6DF))
  (triple .
    #(#x290A #x21DB #x290B #x21DA))
  (quadruple .
    #(#x27F0 #x2B46 #x27F1 #x2B45))
  (dashed .
    #(#x21E1 #x21E2 #x21E3 #x21E0))
  (triangle-dashed .
    #(#x2B6B #x2B6C #x2B6D #x2B6A))
  (opposite .
    #(#x21C5 #x21C4 #x21F5 #x21C6))
  (triangle-opposite .
    #(#x2B81 #x2B82 #x2B83 #x2B80))
  (paired .
    #(#x21C8 #x21C9 #x21CA #x21C7))
  (triangle-paired .
    #(#x2B85 #x2B86 #x2B87 #x2B84))
  (two-headed .
    #(#x2BED #x2BEE #x2BEF #x2BEC))
  (bent-tip .
    #(#x21B1 #x2B0F #x2B0E #x21B3 #x21B2 #x2B10 #x2B11 #x21B0))
  (long-bent-tip .
    #(#x2BA3 #x2BA5 #x2BA7 #x2BA1 #x2BA0 #x2BA6 #x2BA4 #x2BA2))
  (curving .
    #(#x2934 #x2937 #x2935 #x2936))
  ;; SMuFL arrow heads
  (black-head .
    #(#xEB78 #xEB79 #xEB7A #xEB7B #xEB7C #xEB7D #xEB7E #xEB7F))
  (white-head .
    #(#xEB80 #xEB81 #xEB82 #xEB83 #xEB84 #xEB85 #xEB86 #xEB87))
  (open-head .
    #(#xEB88 #xEB89 #xEB8A #xEB8B #xEB8C #xEB8D #xEB8E #xEB8F))
  ;; Unicode arrow heads
  (equilateral-head .
    #(#x2B9D #xF62C #x2B9E #xF62D #x2B9F #xF62E #x2B9C #xF62F))
  (three-d-head .
    #(#x2B99 #xF628 #x2B9A #xF629 #x2B9B #xF62A #x2B98 #xF62B))
  ;; Geometric shapes
  (black-triangle .
    #(#x25B2 #x25E5 #x25B6 #x25E2 #x25BC #x25E3 #x25C0 #x25E4))
  (white-triangle .
    #(#x25B3 #x25F9 #x25B7 #x25FF #x25BD #x25FA #x25C1 #x25F8))
  (black-small-triangle .
    #(#x25B4 #x25B8 #x25BE #x25C2))
  (white-small-triangle .
    #(#x25B5 #x25B9 #x25BF #x25C3))
  (half-circle .
    #(#x2BCA #x25D7 #x2BCB #x25D6))
  (circle-half-black .
    #(#x25D3 #x25D1 #x25D2 #x25D0))
  (square-half-black .
    #(#x2B12 #x2B14 #x25E8 #x25EA #x2B13 #x2B15 #x25E7 #x25E9))
  (diamond-half-black .
    #(#x2B18 #x2B17 #x2B19 #x2B16))
  (circle-quarters .
    #(#x25CF #x25D4 #x25D1 #x25D5))
))

#(define-markup-command (ekm-arrow layout props style orient)
  (symbol? number?)
  (ekm:char layout props (ekm-oref (ekm-assq ekm-arrow-tab style) orient)))

#(define-markup-command (ekm-arrow-head layout props axis dir filled)
  (integer? ly:dir? boolean?)
  (interpret-markup layout props
    (make-ekm-arrow-markup (if filled 'black-head 'open-head) (+ axis dir))))


%% Percussion symbols

%% Beaters mapped onto style and type:
%%  (style dirs beater-entry ...)
%% dirs:
%%  #t = udrl, #f = ud
%% beater-entry:
%%  (type . cp)
%%  (type . #(cp ...))
%% cp < 0 draws -cp flipped
#(define ekm-beater-tab '(
  (xyl #t
    (soft . #xE770)
    (medium . #xE774)
    (hard . #xE778)
    (wood . #xE77C))
  (glsp #t
    (soft . #xE780)
    (hard . #xE784))
  (timpani #t
    (soft . #xE788)
    (medium . #xE78C)
    (hard . #xE790)
    (wood . #xE794))
  (yarn #t
    (soft . #xE7A2)
    (medium . #xE7A6)
    (hard . #xE7AA))
  (gum #t
    (soft . #xE7BB)
    (medium . #xE7BF)
    (hard . #xE7C3))
  (bass #f
    (soft . #xE798)
    (medium . #xE79A)
    (hard . #xE79C)
    (metal . #xE79E)
    (double . #xE7A0))
  (stick #f
    (normal . #(#xE7E8 #x-E7E8))
    (snare . #xE7D1)
    (jazz . #xE7D3))
  (hammer #f
    (plastic . #xE7CD)
    (wood . #xE7CB)
    (metal . #xE7CF))
  (wound #t
    (soft . #xE7B7)
    (hard . #xE7B3))
  (superball #t
    (normal . #xE7AE))
  (metal #t
    (normal . #xE7C7))
  (brass #t
    (normal . #(#xE7D9 #xE7DA #xE7ED #xE7EE)))
  (triangle #f
    (normal . #xE7D5)
    (plain . #(#xE7EF #x-E7EF)))
  (brushes #f
    (normal . #xE7D7))
  (mallet #f
    (normal . #(#xE7DF #xE7EC)))
  (hand #f
    (normal . #(#xE7E3 #x-E7E3))
    (finger . #(#xE7E4 #x-E7E4))
    (fist . #(#xE7E5 #x-E7E5))
    (fingernail . #(#xE7E6 #x-E7E6)))
))

%% Beater orientations for orientation index
%%  (cp-offset-udrl xform-udrl cp-offset-ud xform-ud)
%% xform:
%%  #f = no, #t = flip, or rotation angle
#(define ekm-beater-dir '#(
  (0 #f  0 #f)
  (2 #f  0 -30)
  (0 -90 0 -90)
  (2 #t  1 30)
  (1 #f  1 #f)
  (3 #t  1 -30)
  (1 -90 1 -90)
  (3 #f  0 30)))

#(define-markup-command (ekm-beater layout props style orient)
  (symbol? number?)
  (let* ((n (string-split (symbol->string style) #\-))
         (ntab (ekm-assq ekm-beater-tab (string->symbol (car n))))
         (cpvec (ekm-assq (cdr ntab) (if (null? (cdr n)) #f (string->symbol (cadr n)))))
         (dir (ekm-oref ekm-beater-dir orient))
         (dir (if (car ntab) dir (cddr dir)))
         (cp ((if (vector? cpvec) vector-ref +) cpvec (car dir)))
         (sil (ekm:char layout props (abs cp)))
         (sil (if (or (negative? cp) (eq? #t (cadr dir))) (flip-stencil Y sil) sil)))
    (if (number? (cadr dir)) (ly:stencil-rotate sil (cadr dir) 0 0) sil)))


%% Electronic music symbols

#(define (ekm-control precp ctrlcp thumbcp level fmt)
  (let* ((lv (inexact->exact (min 100 (round
                (if (< level 0) (* (exp (* level 0.05 (log 10))) 100) level)))))
         (pre (zero? (remainder lv 20))))
    (list
      (make-ekm-char-markup (if pre (+ precp (round (/ lv 20))) ctrlcp))
      (if pre #f (make-ekm-char-markup thumbcp))
      lv
      (format #f (or fmt (if (< level 0) "~adB" "~a%")) level))))

#(define-markup-command (ekm-fader layout props level orient)
  (number? boolean-or-number?)
  #:properties ((label-format))
  (let ((cl (ekm-control #xEB2E #xEB2C #xEB2D level label-format)))
    (interpret-markup layout props
      (make-ekm-label-markup orient
        (fourth cl)
        (if (second cl)
          (make-stencil-markup
            (let* ((ctrl (interpret-markup layout props (first cl)))
                   (thumb (interpret-markup layout props (second cl)))
                   (y (ly:stencil-extent thumb Y))
                   (y (- (ekm-extent ctrl Y) (interval-length y) (* 2 (car y)))))
              (stack-stencil-line 0 (list
                ctrl
                (ly:stencil-translate thumb (cons 0 (* y (third cl) 1/100)))
                (ly:make-stencil ""
                  (interval-scale (ly:stencil-extent ctrl X) 2)
                  '(0 . 0))))))
          (first cl))))))

#(define-markup-command (ekm-midi layout props level orient)
  ;; Assumes the original thumb pointing upward = 50%, whole range = 270 deg
  (number? boolean-or-number?)
  #:properties ((label-format))
  (let ((cl (ekm-control #xEB36 #xF6D2 #xF6D3 level label-format)))
    (interpret-markup layout props
      (make-ekm-label-markup orient
        (fourth cl)
        (if (second cl)
          (make-stencil-markup
            (let* ((ctrl (interpret-markup layout props (first cl)))
                   (thumb (interpret-markup layout props (second cl)))
                   (ext (ly:stencil-extent thumb Y)))
              (ly:stencil-add
                ctrl
                (ly:stencil-translate
                  (ly:stencil-rotate
                    thumb
                    (* 27/10 (- 50 (third cl)))
                    -1 (- (/ (+ (cdr ext) (car ext)) (interval-length ext))))
                  (cons
                    (* 1/2 (ekm-extent ctrl X))
                    (* 1/2 (ekm-extent ctrl Y)))))))
          (first cl))))))


%% Other symbols

#(define-markup-command (ekm-fermata layout props style)
  (symbol?)
  #:properties ((direction UP))
  (let ((cp (or (assoc-ref ekm-script-tab (string-append "d" (symbol->string style) "fermata"))
                (assoc-ref ekm-script-tab "dfermata"))))
    (ekm:char layout props (if (<= 0 direction) (car cp) (cdr cp)))))


#(define-markup-command (ekm-eyeglasses layout props dir)
  (ly:dir?)
  (ekm:char layout props (if (< 0 dir) #xF65F #xEC62)))


#(define-markup-command (ekm-note-by-number layout props style log dots dir)
  (symbol? integer? integer? ly:dir?)
  (let* ((note (interpret-markup layout props
                 (ekm-note style log (if (zero? dir) UP dir))))
         (cp (ekm-assq ekm-dots-tab style))
         (dt (ekm:char layout props (car cp)))
         (dts (ekm-cat-dots dots dt #f)))
    (ly:stencil-stack note X RIGHT dts
      (* (ekm-extent dt X)
         (if (and (<= 3 log) (< 0 dir))
           (list-ref cp (- (min log 5) 2)) 1)))))


#(define-markup-command (ekm-metronome layout props cnt)
  (integer?)
  #:properties ((word-space))
  (ly:stencil-aligned-to
    (stack-stencil-line word-space
      (make-list cnt
        (ekm:char layout (cons '((font-size . -3)) props) #xF614)))
    X CENTER))

ekmMetronome =
#(define-music-function (music)
  (ly:music?)
  (for-some-music
    (lambda (m)
      (let* ((mom (ly:music-length m))
             (cnt (ceiling
                    (* (ly:moment-main-numerator mom)
                       (/ 4 (ly:moment-main-denominator mom))))))
        (if (music-is-of-type? m 'multi-measure-rest)
          (begin
            (ly:music-set-property!
              m 'articulations
              (cons
                (make-music
                  'MultiMeasureTextEvent
                  'direction UP
                  'text (markup #:ekm-metronome cnt))
                (ly:music-property m 'articulations)))
            #t)
        (if (or (music-is-of-type? m 'note-event)
                (music-is-of-type? m 'rest-event)
                (music-is-of-type? m 'event-chord))
          (begin
            (set! m #{
              #m - \tweak parent-alignment-X #CENTER
                 - \tweak extra-spacing-width #'(-0.8 . 0.8)
                 % - \tweak extra-spacing-height #'(-inf.0 . +inf.0)
                 ^ \markup \ekm-metronome #cnt
              #})
            #t)
        #f))))
    music)
  music)


ekmSmuflOn =
#(define-music-function (type)
  (symbol-list-or-symbol?)
  (let* ((typ (if (symbol? type) (list type) type))
         (all (memq 'all typ))
         (music #{ #}))
    (define (on t m)
      (if (or all (memq t typ)) (set! music #{ #music #m #})))

    (on 'clef #{
      \override Clef.stencil = #ekm-clef
      \set clefTranspositionFormatter = #ekm-clef-mod
      \set cueClefTranspositionFormatter = #ekm-clef-mod
    #})
    (on 'time #{
      \override Timing.TimeSignature.stencil = #ekm-timesig
    #})
    (on 'notehead #{
      \override NoteHead.stencil = #ekm-notehead
    #})
    (on 'dot #{
      \override Dots.stencil = #ekm-dots
    #})
    (on 'flag #{
      \override Stem.details.lengths = #(ekm-stemlength 'default)
      \override Flag.stencil = #ekm-flag
    #})
    (on 'rest #{
      \override Rest.stencil = #ekm-rest
      \override MultiMeasureRest.stencil = #ekm-mmr
      \override MultiMeasureRestNumber.stencil = #ekm-mmr-number
    #})
    (on 'systemstart #{
      \override SystemStartBrace.stencil = #ekm-system-start
      \override SystemStartBracket.stencil = #ekm-system-start
    #})
    (on 'dynamic #{
      \override DynamicText.stencil = #ekm-dyntext
    #})
    (on 'script #{
      \override Script.stencil = #ekm-script
    #})
    (on 'lv #{
      \override LaissezVibrerTie.stencil = #ekm-lvtie
    #})
    (on 'trill #{
      \override TrillSpanner.stencil = #ekm-trillspan
      \override TrillPitchHead.stencil = #ekm-trillpitch-head
      \override TrillPitchParentheses.stencils = #ekm-calc-parenthesis-stencils
    #})
    (if (or all (memq 'segno typ))
      (ekm-segno-init))
    (if (or all (memq 'colon typ))
      (add-bar-glyph-print-procedure ":" make-ekm-colon-bar-line))
    (on 'percent #{
      \override RepeatSlash.stencil = #(ekm-percent 1)
      \override DoubleRepeatSlash.stencil = #(ekm-percent 2)
      \override PercentRepeat.stencil = #(ekm-percent 3)
      \override DoublePercentRepeat.stencil = #(ekm-percent 4)
    #})
    (on 'tremolo #{
      \override StemTremolo.stencil = #ekm-repeat-tremolo
    #})
    (on 'arpeggio #{
      \override Arpeggio.stencil = #ekm-arpeggio
    #})
    (on 'tuplet #{
      \override TupletNumber.text = #ekm-tuplet-number::calc-denominator-text
    #})
    (on 'fingering #{
      \override Fingering.stencil = #(ekm-fingering 0)
      \override StrokeFinger.stencil = #(ekm-fingering -5)
    #})
    (on 'stringnumber #{
      \override StringNumber.stencil = #ekm-stringnumber
    #})
    (on 'fbass #{
      \set figuredBassFormatter = #ekm-fbass
    #})
    (on 'pedal #{
      \override SustainPedal.stencil = #ekm-pedal
      \override SostenutoPedal.stencil = #ekm-pedal
      \override UnaCordaPedal.stencil = #ekm-pedal
    #})
    (on 'lyric #{
      \override LyricText.stencil = #ekm-lyric-text
    #})
    music))

ekmSmuflOff =
#(define-music-function (type)
  (symbol-list-or-symbol?)
  (let* ((typ (if (symbol? type) (list type) type))
         (all (memq 'all typ))
         (music #{ #}))
    (define (on t m)
      (if (or all (memq t typ)) (set! music #{ #music #m #})))

    (on 'clef #{
      \revert Clef.stencil
      \unset clefTranspositionFormatter
      \unset cueClefTranspositionFormatter
    #})
    (on 'time #{
      \revert Timing.TimeSignature.stencil
    #})
    (on 'notehead #{
      \revert NoteHead.stencil
    #})
    (on 'dot #{
      \revert Dots.stencil
    #})
    (on 'flag #{
      \revert Stem.details.lengths
      \revert Flag.stencil
    #})
    (on 'rest #{
      \revert Rest.stencil
      \revert MultiMeasureRest.stencil
      \revert MultiMeasureRestNumber.stencil
    #})
    (on 'systemstart #{
      \revert SystemStartBrace.stencil
      \revert SystemStartBracket.stencil
    #})
    (on 'dynamic #{
      \revert DynamicText.stencil
    #})
    (on 'script #{
      \revert Script.stencil
    #})
    (on 'lv #{
      \revert LaissezVibrerTie.stencil
    #})
    (on 'trill #{
      \revert TrillSpanner.stencil
      \revert TrillPitchHead.stencil
      \revert TrillPitchParentheses.stencils
    #})
    (on 'percent #{
      \revert RepeatSlash.stencil
      \revert DoubleRepeatSlash.stencil
      \revert PercentRepeat.stencil
      \revert DoublePercentRepeat.stencil
    #})
    (on 'tremolo #{
      \revert StemTremolo.stencil
    #})
    (on 'arpeggio #{
      \revert Arpeggio.stencil
    #})
    (on 'tuplet #{
      \revert TupletNumber.text
    #})
    (on 'fingering #{
      \revert Fingering.stencil
      \revert StrokeFinger.stencil
    #})
    (on 'stringnumber #{
      \revert StringNumber.stencil
    #})
    (on 'fbass #{
      \unset figuredBassFormatter
    #})
    (on 'pedal #{
      \revert SustainPedal.stencil
      \revert SostenutoPedal.stencil
      \revert UnaCordaPedal.stencil
    #})
    (on 'lyric #{
      \revert LyricText.stencil
    #})
    music))


%% Initializations

#(let* ((f (or (ly:get-option 'ekmfont) (ly:get-option 'ekmelic-font)))
        (f (if f (symbol->string f)
           (if (defined? 'ekmFont) ekmFont
           (if (defined? 'ekmelicFont) ekmelicFont ""))))
        (p (string-suffix? "#" f))
        (f (if p (string-drop-right f 1) f)))
  (set! ekm:font-name (if (string-null? f) "Ekmelos" f))
  (set! ekm:draw-paths (and p (defined? 'ekm-path-stencil))))

%% System start delimiters mapped onto style and size limit:
%%  (style delimiter-entry ...)
%% delimiter-entry:
%%  (limit text scale stretch end-height middle-height left right)
%%  (limit text scale stretch bottom-text top-text left right)
%%  (limit text scale stretch)
%% limit:
%%  entry is used for size < limit
%% text:
%%  symbol to draw or #f for lengthen with bottom/top-text
%% scale:
%%  scale factor em -> ssp, default is 255/1000
%% stretch:
%%  stretch factor or #f for no stretch (= 1), default is #f
%% left, right:
%%  X extent of fitting segments
#(set! ekm-system-start-tab
  (if (string=? "Bravura" ekm:font-name)
    `((brace
        (9  (#xE000 1))
        (18 #xE000)
        (27 (#xE000 2))
        (36 (#xE000 3))
        (45 (#xE000 4))
        (50 (#xE000 4) ,(* -43 255/1000) 43)
        (+inf.0 (#xE000 4) ,(* -40 255/1000) #f 126/1000 468/1000 24/57 35/57))
      (bracket
        (+inf.0 #f ,(* -4 255/1000) #f #xE004 #xE003 0 0.45))
    )
  (if (string=? "Ekmelos" ekm:font-name)
    `((brace
        (3  #xF706 255/500)
        (6  #xE000)
        (12 #xF708 255/2000)
        (20 #xF70A 255/4000)
        (28 #xF70C 255/6000)
        (36 #xF70E 255/8000)
        (44 #xF710 255/10000)
        (52 #xF712 255/12000)
        (60 #xF714 255/14000)
        (68 #xF716 255/16000)
        (76 #xF718 255/18000)
        (84 #xF71A 255/20000)
        (97 #xF71A ,(* -83 255/20000) 83)
        (+inf.0 #xF71A ,(* -83 255/20000) #f 1529/20016 16958/20016 158/676 320/676))
      (bracket
        (+inf.0 #f ,(* -4 255/1000) #f #xE004 #xE003 0 0.35))
      ;(line-bracket
      ;  (+inf.0 #f ,(* -4 255/1000) #f #xF702 #xF701 0 0.1))
    )
    `((brace
        (+inf.0 #xE000))
      (bracket
        (+inf.0 #f ,(* -4 255/1000) #f #xE004 #xE003 0 0.45))
    ))))
