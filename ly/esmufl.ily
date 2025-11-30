%% Esmuflily - Support for SMuFL/Ekmelos
%% Copyright (c) 2020-2025 Thomas Richter
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
%%
%%

\version "2.24.0"

%% let-values
#(use-modules (srfi srfi-11))

#(load "ekmd.scm")


%% Font

#(define ekm:font-name #f)
#(define ekm:font-size 5)
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
  (or (not x)
      (ekm-cp? x)
      (string? x)
      (pair? x)))


%% Markup and stencils

#(define-markup-command (ekm-str layout props str)
  (string?)
  #:properties ((font-size 0))
  (interpret-markup
    layout
    (cons
      `((font-size . ,(+ ekm:font-size font-size))
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
        `((font-size . ,(+ ekm:font-size font-size))
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
            `(font-size . ,(+ ekm:font-size font-size))
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
      (make-ekm-str-markup (apply string (map integer->char cps)))))))

#(define-markup-command (ekm-text layout props txt)
  (ekm-extext?)
  (if (not txt)
    empty-stencil
    (interpret-markup layout props
      (if (ekm-cp? txt)
        (make-ekm-char-markup txt)
      (if (pair? txt)
        (if (ekm-cp? (car txt))
          (if (ekm-cdr-cp? txt)
            (make-ekm-chars-markup txt)
            (make-ekm-charf-markup (car txt) (cdr txt)))
          txt)
        txt)))))

#(define-markup-command (ekm-concat layout props args)
  (cheap-list?)
  (stack-stencil-line 0
    (interpret-markup-list layout props
      (map make-ekm-text-markup args))))

#(define-markup-command (ekm-line layout props args)
  (cheap-list?)
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

#(define (ekm:text layout props txt)
  (interpret-markup layout props (make-ekm-text-markup txt)))


#(define-public CX #b101)
#(define-public CY #b110)
#(define-public CXY #b111)
#(define-public STEMCX #b001)
#(define-public STEMCXY #b011)

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
    (if (> center 3)
      center
      (logand center
        (if (ekm-cp? txt) 0
        (if (pair? txt)
          (if (ekm-cp? (car txt)) (if (ekm-cdr-cp? txt) 1 0) 3) 3))))
    (interpret-markup layout props
      (make-ekm-text-markup txt))))

#(define-markup-command (ekm-combine layout props cp x y scp)
  (ekm-cp? number? number? ekm-cp?)
  (interpret-markup layout props
    (markup #:combine
      #:ekm-char cp
      #:translate-scaled (cons x y) #:ekm-char scp)))


#(define (ekm-extent sil dir)
  (interval-length (ly:stencil-extent sil dir)))

#(define (ekm-dim grob mk dir)
  (cdr (ly:stencil-extent (grob-interpret-markup grob mk) dir)))

#(define (ekm-dir stm)
  (let ((dir (if (null? (ly:grob-object stm 'beam))
              (ly:grob-property stm 'direction)
              (ly:grob-property-data stm 'direction))))
    (if (number? dir) dir UP)))


%% Table access

#(define MAIN -1)

#(define (ekm:mv f) (if f 1 MAIN))

#(define (ekm:reverse dir)
  (and dir (if (negative? dir) UP DOWN)))

#(define (ekm:sym val dir)
  (if (or (not-pair? val) (not dir))
    val
    (if (or (null? (cdr val)) (negative? dir)) (car val) (cdr val))))

#(define (ekm:assq tab key)
  (or (assq-ref tab key) (cdar tab)))

#(define (ekm:asstl type style)
  (ekm:assq (assq-ref ekm:types type) style))

#(define (ekm:assid type key)
  (let ((t (cdar (assq-ref ekm:types type))))
    (if key (assoc-ref t key)
    (if (eq? #t (car t)) (cdr t) t))))

#(define (ekm:asst tab style key dir)
  (let ((stab (if style (ekm:assq (if (symbol? tab) (assq-ref ekm:types tab) tab) style) tab)))
    (ekm:sym
      ;; key #f never occurs
      ;(if key (or (assoc-ref stab key) (cdr (last stab))) stab)
      (or (assoc-ref stab key) (cdr (last stab)))
      dir)))

#(define (ekm:assld tab grob log dir)
  (ekm:asst tab
    (if (ly:grob? grob) (ly:grob-property grob 'style) grob)
    (or log (ly:grob-property grob 'duration-log))
    (or dir (ekm-dir (ly:grob-object grob 'stem)))))

#(define (ekm:asslim type style size dir)
  (let ((e (let sel ((t (ekm:asstl type style)))
              (if (or (not t) (null? t)) 0
              (if (<= size (caar t)) (cdar t)
              (sel (cdr t)))))))
    (ekm:sym e dir)))


%% Token table

#(define (ekm:asstk tab def)
  (if (null? tab) #f
  (if (string-prefix? (caar tab) def) (car tab)
  (ekm:asstk (cdr tab) def))))

#(define (ekm:tokens tab def tokens)
  (let cvt ((k '()) (v '()) (d def))
    (if (string-null? d)
      (cons* (reverse k) (reverse v))
      (let ((f (or (ekm:asstk tab d)
                   (ekm:asstk ekm-shared-tab d))))
        (if (not f)
          (begin
            (ly:warning "Definition string has unknown characters `~a'" d)
            (cvt k v ""))
          (cvt
            (if (and tokens (cdr f)) (cons* (car f) k) k)
            (cons* (make-ekm-text-markup (cdr f)) v)
            (substring d (string-length (car f)))))))))

#(define-markup-command (ekm-def layout props tab def)
  (pair? string?)
  (stack-stencil-line 0
    (interpret-markup-list layout props
      (cdr (ekm:tokens tab def #f)))))


%% Number

#(define-markup-command (ekm-number layout props style num)
  (symbol? integer?)
  (interpret-markup layout props
    (let ((tab (ekm:asstl 'number style)))
      (cond
      ((not tab)
        (number->string num 10))
      ((pair? tab)
        (let ((sym (or (assv-ref tab num)
                       (assq-ref (ekm:asstl 'number 'ekm) style))))
          (if (procedure? sym)
            (sym (number->string num 10))
            (make-ekm-text-markup sym))))
      ((procedure? tab)
        (tab (number->string num 10)))
      (else
        (let digit ((f (not num)) (n num) (l '()))
          (if f (make-ekm-concat-markup l)
          (digit
            (< n 10)
            (quotient n 10)
            (cons* ((if (vector? tab) vector-ref +) tab (remainder n 10)) l)))))))))


%% Metadata access

#(define (ekm:md key)
  (assq-ref ekmd:defaults key))

#(define (ekm:md-glyph grob cp)
  (let ((d (assv-ref ekmd:glyphs cp)))
    (if d
      (if (first d)
        d
        (let* ((sil (grob-interpret-markup grob (make-ekm-char-markup cp)))
               (w (* 0.5 (ekm-extent sil X)))
               (h (ly:stencil-extent sil Y))
               (c (list #t
                    (cons (1- (/ (car (second d)) w))
                          (/ (cdr (second d)) (abs (cdr h))))
                    (cons (1- (/ (car (third d)) w))
                          (/ (cdr (third d)) (abs (car h)))))))
          (assv-set! ekmd:glyphs cp c)
          c))
      '(#t (-1 . 0) (1 . 0)))))


%% Orientation

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

#(define (ekm:orient-trans angle sil)
  (case angle
    ((0 1) (flip-stencil angle sil))
    (else (ly:stencil-rotate sil angle 0 0))))

#(define-markup-command (ekm-orient layout props type style orient)
  (symbol? symbol? number?)
  (let* ((val (ekm:asstl type style))
         (len (if (vector? val) (vector-length val) 0))
         (tmap (ekm:asstl type 'ekm))
         (imap (assv-ref tmap len))
         (tmap (assv-ref tmap -1))
         (idx (max 0 (min (1- (vector-length tmap))
                (- 4 (inexact->exact (ceiling (* orient 2))))))))
    (let fnd ((i idx))
      (let* ((j (if imap (list-index (lambda (e) (= e i)) imap) i))
             (sym (and j (if (= 0 len) (+ val j)
                         (if (< j len) (vector-ref val j) #f)))))
        (if sym
          (interpret-markup layout props (make-ekm-text-markup sym))
          (let ((t (vector-ref tmap i)))
            (if (pair? t)
              (ekm:orient-trans (cdr t) (fnd (car t)))
              (fnd t))))))))

#(define ekm-orient-pos '#(
  (0 . 1)
  (1 . 1)
  (1 . 0.5)
  (1 . 0)
  (0 . -1)
  (-1 . 0)
  (-1 . 0.5)
  (-1 . 1)))

#(define-markup-command (ekm-label layout props orient label arg)
  (boolean-or-number? markup? markup?)
  #:properties ((font-size 0)
                (label-size -4)
                (padding 0.3))
  (let ((pos (if (number? orient)
              (vector-ref ekm-orient-pos
                (max 0 (min 7 (- 4 (inexact->exact (ceiling (* orient 2)))))))
              #f)))
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


%% Standard staff line position

#(define ekm-linepos-tab '(
  (5 4 2 0 -2 -4)
  (2 1 -1)
  (1 0)
))

#(define (ekm-linepos lines)
  (or (assv-ref ekm-linepos-tab lines)
      (let ((lp (iota lines (1- lines) -2)))
        (set! ekm-linepos-tab (acons lines lp ekm-linepos-tab))
        lp)))


%% Clef

#(define (ekm-init-clef)
  (let iter-s ((t (ekm:assid 'clef #f)))
    (if (null? t) #t
    (let ((sym (ekm:sym (cdar t) MAIN)))
      (if (not (string-prefix? "clefs." (caar t)))
        (if (or (not-pair? sym) (null? (cdr sym)))
          (add-new-clef (caar t) (caar t) 0 0 0)
          (add-new-clef (caar t) (caar t) (second sym) (third sym) (fourth sym))))
      (iter-s (cdr t))))))

#(define-public ekm:clef-change-font-size '(1.5 . -2))

#(define (ekm-clef grob)
  (let* ((name (ly:grob-property grob 'glyph-name))
         (ch (string-suffix? "_change" name))
         (val (ekm:assid 'clef (if ch (string-drop-right name 7) name)))
         (sym (ekm:sym val (ekm:mv ch)))
         (mk (make-ekm-char-markup
              (ekm:sym (or sym (ekm:sym val MAIN)) MAIN))))
    (grob-interpret-markup grob
      (if ch
        (make-fontsize-markup
          ((if sym car cdr) ekm:clef-change-font-size)
          mk)
        mk))))

#(define (ekm-clef-mod trans style)
  (let* ((tr (ekm:assid 'clef-mod trans))
         (paren (ekm:assid 'clef-mod style)))
    (make-hcenter-in-markup 1.5
      (make-fontsize-markup 2.7
        (make-ekm-concat-markup (list
          (if paren (ekm:sym paren LEFT) 0)
          (if tr
            tr
            (make-ekm-def-markup (ekm:asstl 'fingering 'italic) trans))
          (if paren (ekm:sym paren RIGHT) 0)))))))


%% Time signature

#(define (ekm-time-subnum num)
  (or (ekm:assid 'time-sub num)
      (ekm:assid 'time "//")))

#(define (ekm-time-num l num)
  (if num
    (cond
      ((pair? (car l))
        (make-ekm-concat-markup
          (list
            (make-ekm-number-markup 'time (caar l))
            (ekm-time-subnum (cdar l)))))
      ((integer? (car l))
        (make-ekm-number-markup 'time (car l)))
      (else
        (make-ekm-text-markup (ekm-time-subnum (car l)))))
    (car l)))

#(define (ekm-time-join ls sep denom)
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
  (let* ((t (ekm-time-join
              (cond
                ((number? fr) (list fr))
                ((number-pair? fr) (list (car fr) (cdr fr)))
                (else fr))
              (make-ekm-text-markup (ekm:assid 'time "/+"))
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
          (make-ekm-ctext-markup CY (ekm:assid 'time "+"))
          #f)))))

#(define (ekm-time-plain sig)
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
         (mlen-moment (if (ly:version? < '(2 25)) mlen (ly:make-moment mlen)))
         (beatGrouping (calculate-compound-beat-grouping pln))
         (timesig (cons (ly:moment-main-numerator mlen-moment)
                        (ly:moment-main-denominator mlen-moment))))
    (if (ly:version? < '(2 25))
      #{
        \once \override Timing.TimeSignature.stencil =
          #(lambda (grob) (grob-interpret-markup grob (ekm-time-compound sig)))
        \set Timing.timeSignatureFraction = #timesig
        \set Timing.baseMoment = #(calculate-compound-base-beat pln)
        \set Timing.beatStructure = #beatGrouping
        \set Timing.beamExceptions = #'()
        \set Timing.measureLength = #mlen
      #}
      #{
        \once \override Timing.TimeSignature.stencil =
          #(lambda (grob) (grob-interpret-markup grob (ekm-time-compound sig)))
        \set Timing.timeSignature = #timesig
        \set Timing.beatBase = #(calculate-compound-beat-base pln)
        \set Timing.beatStructure = #beatGrouping
        \set Timing.beamExceptions = #'()
        \set Timing.measureLength = #mlen
      #})))

#(define (ekm-timesig grob)
  (let* ((fr (or (ly:grob-property grob 'time-signature #f)
                 (ly:grob-property grob 'fraction)))
         (st (ly:grob-property grob 'style))
         (sym (cond
               ((equal? '(4 . 4) fr) (ekm:assid 'time "C"))
               ((equal? '(2 . 2) fr) (ekm:assid 'time "|C"))
               (else #f))))
    (grob-interpret-markup grob
      (if (and sym (or (eq? 'C st) (eq? 'default st)))
        (make-ekm-ctext-markup CY sym)
        (ekm-time-fraction fr st)))))


%% Cadenza signature

ekmCadenzaOn =
#(define-music-function (style)
  (string-or-symbol?)
  (let ((sym (ekm:assid 'time style)))
    (if sym
      #{
        \once \override Staff.TimeSignature.stencil =
          #(lambda (grob) (ekm-ctext grob CY sym))
        \time 1/1 % force printing
        \cadenzaOn
      #}#{
        \cadenzaOn
      #})))


%% Staff divider / separator

ekmStaffDivider =
#(define-music-function (dir)
  (ly:dir?)
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
              (make-ekm-text-markup (ekm:assid 'barline dir)))))
        0))
    \break
  #})

ekmSlashSeparator =
#(define-scheme-function (size)
  (number?)
  #{ \markup {
       \center-align
       \vcenter
       \override #'(font-size . -5)
       \ekm-text #(ekm:asslim 'separator 'default size #f)
     }
  #})


%% Note head

#(define (ekm-note grob log dir)
  (let ((d (ekm:assld ekm-notehead-tab grob log dir)))
    (if (pair? d)
      (make-combine-markup
        (make-with-color-markup white (make-ekm-char-markup (cdr d)))
        (make-ekm-char-markup (car d)))
      (make-ekm-char-markup d))))

#(define ((ekm-notehead dir) grob)
  (grob-interpret-markup grob (ekm-note grob #f dir)))

#(define (ekm-stem-attachment grob)
  (let* ((stm (ly:grob-object grob 'stem))
         (dir (ly:grob-property stm 'direction))
         (d (ekm:assld ekm-notehead-tab grob #f dir))
         (md (ekm:md-glyph grob (if (pair? d) (car d) d))))
    (if (< dir 0) (second md) (third md))))

ekmNameHeads =
\set shapeNoteStyles = ##(doName reName miName faName soName laName siName)
ekmNameHeadsMinor =
\set shapeNoteStyles = ##(laName siName doName reName miName faName soName)
ekmNameHeadsTi =
\set shapeNoteStyles = ##(doName reName miName faName soName laName tiName)
ekmNameHeadsTiMinor =
\set shapeNoteStyles = ##(laName tiName doName reName miName faName soName)


%% Note cluster

#(define (ekm-cluster grob)
  (let ((nhs (ly:grob-object grob 'note-heads)))
    (if (ly:grob-array? nhs)
      (let* ((nhl (ly:grob-array->list nhs))
             (bot (fold (lambda (nh b)
                (let ((p (ly:grob-property nh 'staff-position)))
                  (ly:grob-set-property! nh 'transparent #t)
                  (if (< p (car b)) (cons p nh) b)))
               (cons 999 #f)
               nhl))
             (d (ekm:assld (assq-ref ekm:types 'cluster) (cdr bot) #f #f))
             (top (fold (lambda (nh t)
                (ly:grob-set-property! nh 'style 'default)
                (max t (ly:grob-property nh 'staff-position)))
               -999
               nhl))
             (h (- top (car bot)))
             (cp (and (< h 3) (list-ref d h)))
             (stm (ly:grob-object grob 'stem))
             (dir (ekm-dir stm))
             (md (ekm:md-glyph (cdr bot)
                  (or cp (if (>= dir 0) (fourth d) (sixth d))))))
        (ly:grob-set-property! (cdr bot) 'stem-attachment
          (if (< dir 0) (second md) (third md)))
        (ly:grob-set-property! stm 'avoid-note-head #t)
        (ly:grob-set-property! stm 'note-collision-threshold 0)
        (if (and (< dir 0) (> h 0))
          (ly:grob-set-property! stm 'stem-begin-position
            (+ (ly:grob-property stm 'stem-begin-position) (seventh d))))
        (ly:grob-set-property! (cdr bot) 'transparent #f)
        (ly:grob-set-property! (cdr bot) 'stencil
          (grob-interpret-markup grob
            (make-with-dimensions-from-markup
              (make-ekm-char-markup (car d))
              (if cp
                (make-ekm-char-markup cp)
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


%% Augmentation dot

#(define (ekm-cat-dots count dot)
  (let ((ext (ekm-extent dot X)))
    (let cat ((c (max count 0))
              (r empty-stencil))
      (if (zero? c) r
        (cat (1- c) (ly:stencil-stack r X RIGHT dot ext))))))

#(define (ekm-dots grob)
  (ekm-cat-dots
    (ly:grob-property grob 'dot-count)
    (ekm-ctext grob 0 (car (ekm:asstl 'dot 'default)))))


%% Flag / Grace note slash

#(define ekm-stemlength-tab '())

#(define (ekm-set-stemlength! len log cp)
  (let ((e (assv-ref ekmd:glyphs cp)))
    (if e
      (if (number? (cdr (first e)))
        (list-set! (car len) (- log 2) (cdr (first e)))
        (list-set! (cdr len) (- log 2) (cdr (second e)))))))

#(define (ekm-init-stemlength)
  (let iter-s ((tab ekm-flag-tab))
    (if (null? tab) #t
      (let* ((s (caar tab))
             (len (cons
              (list 3.5 0.132 0.128 -0.448 -1.244 -2.076 -2.812 -3.608 -4.684)
              (list 3.5 -0.04 -0.088 0.376 1.172 1.9 2.592 3.324 4.064))))
        (let iter-f ((t (cdar tab)))
          (if (null? t)
            (let* ((nom (caar len))
                   (down (cons* nom (map (lambda (y) (abs (- y nom))) (cdar len))))
                   (nom (cadr len))
                   (up (cons* nom (map (lambda (y) (+ nom y)) (cddr len)))))
              (set! ekm-stemlength-tab
                (append ekm-stemlength-tab (list
                  (list s len (cons down up))))))
            (let ((e (car t)))
              (ekm-set-stemlength! len (car e) (cadr e)) ; DOWN
              (ekm-set-stemlength! len (car e) (cddr e)) ; UP
              (iter-f (cdr t)))))
        (iter-s (cdr tab))))))

#(define (ekm-stem-print grob)
  (let* ((flag (ly:grob-object grob 'flag))
         (dir (ly:grob-property grob 'direction))
         (log (ly:grob-property grob 'duration-log))
         (len (second (ekm:assq ekm-stemlength-tab
          (if (null? flag) 'default (ly:grob-property flag 'style))))))
  (ly:grob-set-nested-property! grob '(details lengths)
    (if (< dir 0) (car len) (cdr len)))
  (ly:stem::print grob)))

#(define (ekm-flag grob)
  (let* ((stm (ly:grob-parent grob X))
         (dir (ly:grob-property stm 'direction))
         (log (ly:grob-property stm 'duration-log))
         (style (ly:grob-property grob 'style))
         (len (first (ekm:assq ekm-stemlength-tab style)))
         (flg (grob-interpret-markup grob
                (make-ekm-text-markup (ekm:asst ekm-flag-tab style log dir)))))
    (ly:stencil-translate
      (if (equal? "grace" (ly:grob-property grob 'stroke-style))
        (let ((sym (ekm:asst 'grace style log dir)))
          (ly:stencil-add
            flg
            (grob-interpret-markup grob
              (make-translate-scaled-markup
                (cdr sym)
                (make-ekm-text-markup (car sym))))))
        flg)
      (cons
        (- (* (ly:grob-property stm 'thickness) (ly:staff-symbol-line-thickness grob)))
        (- (list-ref (if (< dir 0) (car len) (cdr len)) (- log 2)))))))

ekmFlag =
#(define-music-function (style)
  (symbol?)
  #{
    \override Flag.style = #style
  #})


%% Rest

#(define-markup-command
  (ekm-mmr layout props style oneline ledgered measures limit width space)
  (symbol? boolean? boolean? index? integer? number? number?)
  (if (> measures limit)
    (let* ((sym (ekm:asstl 'mmrest style))
           (lbar (ekm:text layout props (ekm:sym sym LEFT)))
           (rbar (ekm:text layout props (ekm:sym sym RIGHT)))
           (edge (ekm-extent lbar X)) ; to overlap with bar
           (hbar (* 0.5 (ekm:md 'hBarThickness)))
           (hbar (make-filled-box-stencil
                  (cons 0 (- width (* edge 1.5))) (cons (- hbar) hbar))))
      (stack-stencil-line
        (- (* edge 0.25))
        (list lbar hbar rbar)))
    (let* ((ssp (ly:output-def-lookup layout 'staff-space))
           (cts
             (let cnt ((m measures) (d '(8 4 2 1)) (c '()))
               (if (null? d)
                 (reverse c)
                 (cnt (remainder m (car d))
                      (cdr d)
                      (cons* (quotient m (car d)) c)))))
           (sils
             (reverse (fold (lambda (ct log sil)
               (if (zero? ct) sil
               (let con ((c ct) (s sil))
                 (if (zero? c) s
                 (let ((r (ekm:char layout props
                            (ekm:asst ekm-rest-tab style log (ekm:mv ledgered)))))
                   (con (1- c)
                        (cons*
                          (if ((if oneline = <) log 0) r
                          (ly:stencil-translate-axis r (if oneline (- ssp) ssp) Y))
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
  (ekm-cchar grob 0 (ekm:assld ekm-rest-tab grob #f DOWN)))

#(define (ekm-mmr grob)
  (let* ((org (ly:multi-measure-rest::print grob))
         (lines (ly:grob-property (ly:grob-object grob 'staff-symbol) 'line-count))
         (lpos (ly:grob-property (ly:grob-object grob 'staff-symbol) 'line-positions))
         (lpos (if (null? lpos) (ekm-linepos lines) lpos))
         (pos (inexact->exact (ly:grob-property grob 'staff-position 0)))
         (oneline (= 1 lines))
         (sil (grob-interpret-markup grob
                (make-ekm-mmr-markup
                  (ly:grob-property grob 'style 'default)
                  oneline
                  (not (memv (if oneline pos (+ 2 pos)) lpos))
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
        (make-ekm-number-markup 'time (string->number num)))
      empty-stencil)))

#(define-markup-command
  (ekm-rest-by-number layout props log dot-count)
  (integer? integer?)
  #:properties ((font-size 0)
                (ledgers '(-1 0 1))
                (style '()))
  (let* ((ledgered (memv log ledgers))
         (rest (ekm-center 2 (ekm:char layout props
                 (ekm:asst ekm-rest-tab style log (ekm:mv ledgered)))))
         (dot (and (> dot-count 0)
                   (ekm:text layout props (car (ekm:asstl 'dot 'note))))))
    (if dot
      (ly:stencil-stack rest X RIGHT
        (ekm-cat-dots dot-count dot)
        (* (ekm-extent dot X)
           (if (and ledgered (<= -1 log 1)) 2
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
                  #f #f measures expand-limit width word-space))))
    (if (or bar
            (and multi-measure-rest-number (> measures 1)))
      (let ((num (interpret-markup layout props
                   (make-fontsize-markup -2
                     (make-ekm-number-markup 'time measures)))))
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


%% Parenthesis

#(define (ekm-parens-align val dir)
  (let ((sym (ekm:sym val dir)))
    (if (not-pair? sym)
      (make-ekm-text-markup sym)
      (make-general-align-markup Y (second sym)
        (make-fontsize-markup (third sym)
          (if (string? (first sym))
            (make-sans-markup (first sym))
            (make-ekm-text-markup (first sym))))))))

#(define (ekm-parens style name)
  (let* ((p (ekm:asst 'parens style name #f)))
    (cons
      (ekm-parens-align p LEFT)
      (ekm-parens-align p RIGHT))))


%% System start delimiter

#(define-markup-command (ekm-system-start layout props style size)
  (symbol? number?)
  #:properties ((font-size 0)
                (thickness 0.45))
  (let* (;; select
         (e (ekm:asslim 'systemstart style size LEFT))
         (e (if (pair? e) e (list e)))
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
               (fitw (cons (* (sixth e) sc) (* thickness sc)))
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
                    (make-override-markup
                      `(thickness . ,(ly:grob-property grob 'thickness))
                    (make-ekm-system-start-markup style size))))
             (ext (ly:stencil-extent sil Y)))
        (ly:stencil-translate-axis
          sil
          (- (car lext) (car ext) (/ (- (interval-length ext) size) 2))
          Y))
      empty-stencil)))


%% Dynamic

#(define-markup-command (ekm-dynamic layout props def)
  (string?)
  (interpret-markup layout props
    (let ((c (ekm:assid 'dynamic def)))
      (if c
        (make-ekm-char-markup c)
        (make-ekm-def-markup (ekm:assid 'dynamic #f) def)))))

#(define (ekm-dyntext grob)
  (let ((def (ly:grob-property grob 'text)))
    (grob-interpret-markup grob
      (if (string? def) (make-ekm-dynamic-markup def) def))))

ekmParensDyn =
#(define-event-function (style dyn)
  (symbol? ly:event?)
  (let ((p (ekm-parens style 'dynamic))
        (sp (make-ekm-text-markup (assoc-ref ekm-shared-tab "_"))))
    (make-music 'AbsoluteDynamicEvent
      'text
      (make-concat-markup (list
        (car p) sp
        (make-ekm-dynamic-markup (ly:music-property dyn 'text))
        sp (cdr p))))))

ekmParensHairpin =
#(define-music-function (style)
  (symbol?)
  #{
    \once \override Hairpin.stencil =
    #(lambda (grob)
      (let* ((p (ekm-parens style 'hairpin))
             (l (ekm-ctext grob CY (car p)))
             (r (ekm-ctext grob CY (cdr p)))
             (sp (ekm-ctext grob 0 (assoc-ref ekm-shared-tab "__")))
             (sp (ekm-extent sp X))
             (x (+ (ekm-extent l X) sp)))
        (ly:grob-set-property! grob 'shorten-pair (cons x x))
        (ly:stencil-combine-at-edge
        (ly:stencil-combine-at-edge
          l X RIGHT (ly:hairpin::print grob) sp) X RIGHT r sp)))
  #})


%% Script

#(define (ekm-script grob)
  (let* ((dir (ly:grob-property grob 'direction))
         (d (ly:grob-property grob 'details #f)))
    (ekm-ctext grob CX
      (ekm:sym
        (or d (ekm:assid 'script (cadr (ly:grob-property grob 'script-stencil))))
        (if d (ekm:reverse dir) dir)))))

ekmScript =
#(define-music-function (name text)
  (symbol? ekm-extext?)
  (make-articulation name
    'tweaks `((details . ,text))))

ekmScriptSmall =
#(define-music-function (name text)
  (symbol? ekm-extext?)
  (make-articulation name
    'tweaks `((details . ,text) (font-size . -3))))

#(define-markup-command (ekm-script layout props style dir)
  (string-or-symbol? ly:dir?)
  (interpret-markup layout props
    (make-ekm-text-markup
      (ekm:sym
        (if (string? style)
          (ekm:assid 'script style)
          (assq-ref (ekm:asstl 'spanner style) 'text))
        dir))))


%% Multi-segment spanner

#(define (ekm-segment-spanner grob tab tempo text)
  (let* ((lsil (ly:stencil-translate-axis
                (ekm-ctext grob 0 (or (ekm:sym text LEFT) 0))
                (car (ly:stencil-extent (ly:grob-property grob 'stencil) X))
                X))
         (rsil (ekm-ctext grob 0 (or (if (pair? text) (cdr text) #f) 0)))
         (siblings (ly:spanner-broken-into (ly:grob-original grob)))
         (len (fold (lambda (p l)
           (cons*
             (+ (first l)
                (- (interval-length
                     (ly:stencil-extent (ly:grob-property p 'stencil) X))
                   (ekm-extent lsil X)
                   (ekm-extent rsil X)))
             (if (eq? p grob) (list (car l)) l)))
           '(0)
           (if (null? siblings) (list grob) siblings)))
         (tmp (if (pair? tempo) tempo (cons tempo tempo)))
         (tmp (cons (round (car tmp)) (round (cdr tmp))))
         (tmpdir (- (cdr tmp) (car tmp)))
         (tmpcnt (1+ (abs tmpdir)))
         (tmplen (/ (first len) tmpcnt))
         (tmpidx (iota tmpcnt (car tmp) (if (<= 0 tmpdir) 1 -1)))
         (len (reverse len)))
    (fold (lambda (s sil)
      (if (car s)
        (ly:stencil-stack
          sil X RIGHT
          (if (eq? #t (car s))
            rsil
            (let ((seg (ekm:asst tab #f (abs (car s)) (car s))))
              (grob-interpret-markup grob
                (make-ekm-chars-markup (make-list
                  (inexact->exact (round
                    (/ (cdr s) (ekm-extent (ekm-cchar grob 0 seg) X))))
                  seg)))))
          0)
        sil))
      lsil
      (let-values (((i prv) (floor/ (first len) tmplen))
                   ((j rem) (floor/ (second len) tmplen)))
        (append
          (list
            (cons
              (if (> 0.1 prv) #f (list-ref tmpidx (inexact->exact i)))
              (- tmplen prv)))
          (map
            (lambda (s) (cons s tmplen))
            (list-tail
              (take tmpidx (inexact->exact j))
              (inexact->exact (if (= 0 prv) i (1+ i)))))
          (list
            (cons
              (if (> 0.1 rem) #f (list-ref tmpidx (inexact->exact j)))
              rem)
            '(#t . 0)))))))

#(define (ekm-spanner grob)
  (let ((tab (ekm:asstl 'spanner (ly:grob-property grob 'style 'line))))
    (ly:grob-set-property! grob 'stencil
      (if (null? tab)
        ly:line-spanner::print
        (ekm-segment-spanner grob
          tab
          (ly:grob-property grob 'zigzag-width 0)
          (or (ly:grob-property grob 'text #f) (assq-ref tab 'text)))))))

#(define (ekm-trill? s)
  (let ((t (string-prefix-length "trill-" (symbol->string s))))
    (if (< 4 t) t #f)))

ekmStartSpan =
#(define-event-function (style tempo text)
  (symbol? number-or-pair? ekm-extext?)
  (let* ((t (ekm-trill? style))
         (s (symbol->string style))
         (s (if t (string-drop s t) s)))
    (make-music
      (if t 'TrillSpanEvent 'TextSpanEvent)
      'span-direction START
      'tweaks `((style . ,(if (string-null? s) 'trill (string->symbol s)))
                (zigzag-width . ,tempo)
                (text . ,text)))))

ekmStartSpanMusic =
#(define-music-function (style tempo text music)
  (symbol? number-or-pair? ekm-extext? ly:music?)
  (if (ekm-trill? style)
    #{
      \once \override TrillSpanner.after-line-breaking = #ekm-spanner
      $music \ekmStartSpan #style #tempo #text
    #}
    #{
      \once \override TextSpanner.after-line-breaking = #ekm-spanner
      $music \ekmStartSpan #style #tempo #text
    #}))


%% Trill span

ekmStartTrillSpan =
#(define-event-function (tempo)
  (number-or-pair?)
  (make-music 'TrillSpanEvent
    'span-direction START
    'tweaks `((zigzag-width . ,tempo))))


%% Trill pitch

#(define (ekm-trillpitch-head grob)
  (grob-interpret-markup grob
    (ekm-note
      (ly:grob-property grob 'style)
      (ly:grob-property grob 'duration-log)
      UP)))

#(define (ekm-calc-parenthesis-stencils grob)
  (let ((p (ekm-parens (ly:grob-property grob 'style) 'accidental)))
    (list (ekm-ctext grob CX (car p))
          (ekm-ctext grob CX (cdr p)))))

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
  (let* ((p (fourth (ly:grob-property grob 'control-points)))
         (size (cdr (ly:grob-property grob 'minimum-X-extent '(0 . 0))))
         (sym (ekm:asslim 'lv 'default size
                (ly:grob-property grob 'direction))))
    (ly:stencil-translate
      (ekm-ctext grob 0 sym)
      (cons (- (car p) 1.2) (cdr p)))))

ekmLaissezVibrer =
#(define-event-function (size)
  (number?)
  (make-music 'LaissezVibrerEvent
    'tweaks
    `((minimum-X-extent . ,(cons 0 size)))))


%% Breathing sign / Caesura

ekmBreathing =
#(define-music-function (txt)
  (ekm-extext?)
  (make-music 'BreathingEvent
    'tweaks
    `((text .
      ,(make-translate-scaled-markup '(0 . -0.5)
        (make-ekm-text-markup txt))))))


%% Bar line

#(define ((make-ekm-old-bar-line type name) grob extent)
  (let* ((sym (ekm:assid type name))
         (scale (third sym))
         (sil (ekm-ctext grob (if scale CXY CY) (car sym)))
         (sil (if (second sym)
                sil (ly:make-stencil "" (ly:stencil-extent sil X) '(0 . 0)))))
    (if scale ; segno
      (let* ((th (layout-line-thickness grob))
             (bth (* (ly:grob-property grob 'hair-thickness 1) th))
             (bext (bar-line::widen-bar-extent-on-span grob extent))
             (bar (bar-line::draw-filled-box (cons 0 bth) bext bth bext grob))
             (kern (* (ly:grob-property grob 'segno-kern 1) th scale)))
        (ly:stencil-add
          sil
          (ly:stencil-translate-axis
            (ly:stencil-combine-at-edge bar X LEFT bar kern)
            (* 1/2 kern)
            X)))
      sil)))

#(define ((make-ekm-bar-line type name) is-span grob extent)
  ((make-ekm-old-bar-line type name) grob extent))

#(define (ekm-bar-init type)
  (for-each (lambda (e)
    (if (char? (car e))
      (add-bar-glyph-print-procedure (string (car e))
        (if (ly:version? < '(2 25))
          (make-ekm-old-bar-line type (car e))
          (make-ekm-bar-line type (car e))))
      (apply define-bar-line e)))
    (ekm:assid type #f)))


%% Percent repeat

#(define (ekm-repeat grob)
  (let ((sym (ekm:assid 'percent "/"))
        (cnt (ly:event-property (event-cause grob) 'slash-count)))
    (ly:stencil-combine-at-edge
      point-stencil
      X RIGHT
      (grob-interpret-markup grob
        (make-ekm-concat-markup (make-list cnt sym)))
      1.3)))

#(define (ekm-doublerepeat grob)
  (ekm-ctext grob 0 (ekm:assid 'percent "//")))

#(define (ekm-percent grob)
  (let* ((sym (ekm:assid 'percent "%"))
         (lb (ly:spanner-bound grob LEFT))
         (rb (ly:spanner-bound grob RIGHT))
         (refp (ly:grob-common-refpoint lb rb X))
         (sp (ly:grob-property grob
               'spacing-pair
               '(break-alignment . break-alignment)))
         (l (ly:paper-column::break-align-width lb (car sp)))
         (r (ly:paper-column::break-align-width rb (cdr sp))))
    (ly:stencil-translate-axis
      (ekm-ctext grob CXY sym)
      (+ (* 0.5 (- (car r) (cdr l)))
        (- (cdr l) (ly:grob-relative-coordinate grob refp X)))
      X)))

#(define (ekm-doublepercent grob)
  (ekm-ctext grob CXY (ekm:assid 'percent "%%")))


%% Tremolo mark

#(define ((ekm-repeat-tremolo text) grob)
  (let* ((style (ly:grob-property grob 'shape))
         (cnt (min (ly:grob-property grob 'flag-count) 5))
         (stm (ly:grob-parent grob X))
         (dir (ly:grob-property stm 'direction))
         (y (if (eq? 'ekm style)
              (* (- (interval-length (ly:grob-property stm 'Y-extent)) 1.96) -0.5 dir)
              (* (1- cnt) -0.4 dir))))
    (ly:stencil-translate
      (grob-interpret-markup grob
        (if text
          (make-ekm-ctext-markup STEMCXY
            (or (and (string? text) (ekm:sym (ekm:assid 'stem text) dir))
                text))
          (make-ekm-text-markup
            (ekm:asst 'tremolo style cnt dir))))
      (cons 0 y))))

ekmTremolo =
#(define-music-function (text music)
  (ekm-extext? ly:music?)
  #{
    \override StemTremolo.shape = #'ekm
    \override StemTremolo.stencil = #(ekm-repeat-tremolo text)
    $music
    \revert StemTremolo.shape
    \revert StemTremolo.stencil
  #})


%% Symbol on stem

#(define ((ekm-stem text) grob)
  (let* ((stm (ly:stem::print grob))
         (dir (ly:grob-property grob 'direction))
         (sym (grob-interpret-markup grob
           (make-ekm-ctext-markup STEMCX
             (or (and (string? text) (ekm:sym (ekm:assid 'stem text) dir))
                 text)))))
    (if (null? stm)
      empty-stencil
      ;; set center of symbol relative to stem start at 1.5 + 0.3366 = 1.8366
      (ly:stencil-combine-at-edge
        stm Y (- dir) sym (- (* (ekm-extent sym Y) -0.5) 1.8366)))))

ekmStem =
#(define-music-function (text music)
  (ekm-extext? ly:music?)
  #{
    \override Stem.stencil = #(ekm-stem text)
    $music
    \revert Stem.stencil
  #})


%% Scoop / Plop

#(define-markup-command (ekm-scoop layout props dir)
  (ly:dir?)
  (let* ((sil (interpret-markup layout props
                (make-ekm-text-markup (ekm:asst 'brass 'scoop 0 dir))))
         (xext (ly:stencil-extent sil X))
         (h (ekm-extent sil Y)))
    (ly:stencil-outline sil
      (make-filled-box-stencil xext
        (if (<= 0 dir)
          (cons (- (- h 0.35)) (+ h 0.35))
          (cons (- (+ h 0.35)) (- h 0.35)))))))

ekmScoop =
#(define-music-function (dir music)
  (ly:dir? ly:music?)
  #{
    \set fingeringOrientations = #'(left)
    \override Fingering.padding = #0.2
    #(music-map
      (lambda (m)
        (if (music-is-of-type? m 'note-event)
          (begin
            (ly:music-set-property! m 'articulations
              (cons
                (make-music 'FingeringEvent
                  'text (if (<= 0 dir) "S" "P")
                  'tweaks (list (cons 'stencil (ekm-fingering 0))))
                (ly:music-property m 'articulations)))
            (make-music 'EventChord
              'elements (list m)))
          m))
      music)
    \revert Fingering.padding
    \unset fingeringOrientations
  #})


%% Arpeggio

#(define (ekm-arpeggio grob)
  (let* ((style (ly:grob-property grob 'style 'default))
         (dir (ly:grob-property grob 'arpeggio-direction 0))
         (sym (ekm:asst 'arpeggio style dir dir))
         (bot (make-ekm-text-markup (first sym)))
         (top (make-ekm-text-markup (third sym)))
         (pos (ly:grob-property grob 'positions))
         (cnt (inexact->exact (round
                (- (interval-length pos) (ekm-dim grob bot X) (ekm-dim grob top X))))))
    (ly:stencil-translate
      (ly:stencil-rotate
        (grob-interpret-markup grob
          (make-ekm-concat-markup
            (append!
              (list bot)
              (make-list (max cnt 0) (second sym))
              (list top))))
        90 -1 0)
      ;; for left bearing -60 upm
      (cons 0.3 (- (car pos) 0.4)))))

ekmArpeggioArrowUp = {
  \revert Arpeggio.X-extent
  \override Arpeggio.arpeggio-direction = #UP
}
ekmArpeggioArrowDown = {
  \revert Arpeggio.X-extent
  \override Arpeggio.arpeggio-direction = #DOWN
}
ekmArpeggioNormal = {
  \revert Arpeggio.stencil
  \revert Arpeggio.X-extent
  \revert Arpeggio.arpeggio-direction
  \revert Arpeggio.dash-definition
  \override Arpeggio.stencil = #ekm-arpeggio
}


%% Ottavation

#(define-markup-command (ekm-ottavation layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup (ekm:assid 'ottava #f) def)))

#(define-public (ekm-ottavation style)
  (let ((tab (ekm:assid 'ottava #f)))
    (append-map (lambda (e)
      (list
        (cons (car e) (make-ekm-def-markup tab (ekm:sym (cdr e) UP)))
        (cons (- (car e)) (make-ekm-def-markup tab (ekm:sym (cdr e) DOWN)))))
      (ekm:asstl 'ottavation style))))


%% Tuplet number

#(define (ekm-tuplet-num num grob prop)
  (make-ekm-number-markup
    'tuplet
    (or num (ly:event-property (event-cause grob) prop))))

#(define (ekm-tuplet-frac grob tail)
  (let ((sp (make-hspace-markup (ly:grob-property grob 'word-space 0.2))))
    (cons* sp (ekm:assid 'tuplet ":") sp tail)))

#(define-public ((ekm-tuplet-number num denom) grob)
  (make-ekm-concat-markup (cons*
    (ekm-tuplet-num num grob 'denominator)
    (if (eqv? 0 denom)
      '()
      (ekm-tuplet-frac grob
        (list (ekm-tuplet-num denom grob 'numerator)))))))

#(define-markup-command (ekm-tuplet-note layout props dur)
  (ly:duration?)
  (interpret-markup layout props
    (make-fontsize-markup -5
      (make-ekm-note-by-number-markup
        'metronome
        (ly:duration-log dur)
        (ly:duration-dot-count dur)
        UP))))

#(define-public (ekm-tuplet-number::calc-denominator-text grob)
  ((ekm-tuplet-number #f 0) grob))

#(define-public (ekm-tuplet-number::calc-fraction-text grob)
  ((ekm-tuplet-number #f #f) grob))

#(define-public ((ekm-tuplet-number::non-default-tuplet-denominator-text num) grob)
  ((ekm-tuplet-number num 0) grob))

#(define-public ((ekm-tuplet-number::non-default-tuplet-fraction-text num denom) grob)
  ((ekm-tuplet-number num denom) grob))

#(define-public ((ekm-tuplet-number::append-note-wrapper fmt dur) grob)
  (let ((num (and fmt (fmt grob)))
        (note (make-ekm-tuplet-note-markup dur)))
    (if num (make-line-markup (list num note)) note)))

#(define-public ((ekm-tuplet-number::non-default-fraction-with-notes
           num numdur denom denomdur) grob)
  (make-ekm-concat-markup (cons*
    (ekm-tuplet-num num grob 'denominator)
    (make-ekm-tuplet-note-markup numdur)
    (ekm-tuplet-frac grob
      (list
        (ekm-tuplet-num denom grob 'numerator)
        (make-ekm-tuplet-note-markup denomdur))))))

#(define-public ((ekm-tuplet-number::fraction-with-notes
           numdur denomdur) grob)
  ((ekm-tuplet-number::non-default-fraction-with-notes
    #f numdur #f denomdur) grob))


%% Fingering

#(define-markup-command (ekm-finger layout props def)
  (string?)
  (let ((it (eqv? #\* (string-ref def 0))))
    (interpret-markup layout props
      (make-ekm-def-markup
        (ekm:asstl 'fingering (if it 'italic 'default))
        (if it (string-drop def 1) def)))))

#(define ((ekm-fingering size) grob)
  (let ((def (ly:grob-property grob 'text)))
    (if (string? def)
      (grob-interpret-markup grob
        (make-fontsize-markup (+ size 5)
          (make-ekm-finger-markup def)))
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


%% String number

#(define-markup-command (ekm-default-string-number layout props txt)
  (string?)
  (interpret-markup layout props
    (make-circle-markup (make-fontsize-markup -6 txt))))

#(define-markup-command (ekm-string-number layout props txt)
  (number-or-string?)
  (let ((num (if (number? txt) txt (string->number txt 10))))
    (interpret-markup layout props
      (if num
        (make-fontsize-markup 3 (make-ekm-number-markup 'string (round num)))
        (make-italic-markup txt)))))

#(define (ekm-stringnumber grob)
  (grob-interpret-markup grob
    (make-ekm-string-number-markup
      (ly:grob-property grob 'text))))


%% Piano pedal

#(define-markup-command (ekm-piano-pedal layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup (ekm:assid 'pedal #f) def)))

#(define (ekm-pedal grob)
  (grob-interpret-markup grob
    (make-ekm-piano-pedal-markup (ly:grob-property grob 'text))))


%% Harp pedal

#(define-markup-command (ekm-harp-pedal layout props def)
  (string?)
  (let* ((p (ekm:tokens (ekm:assid 'harp #f) def #t))
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

#(define-markup-command (ekm-harp-change layout props txt y)
  (ekm-extext? number?)
  (interpret-markup layout props
    (markup
      #:combine
        #:ekm-text txt
        #:override '(x-padding . -0.6)
        #:override '(y-padding . -0.1)
        #:translate-scaled (cons 0 y)
        #:ellipse #:transparent #:ekm-text (assoc-ref (ekm:assid 'harp #f) "-"))))


%% Fret diagram

#(define-markup-command (ekm-fret-diagram-terse layout props def)
  (string?)
  #:properties ((fret-diagram-details '()))
  (let* ((defl (string-split def #\x3B))
         (cnt (1- (max 4 (min 7 (length defl)))))
         (tab (ekm:assid 'fret #f))
         (board (ekm:text layout props (ekm:sym (assoc-ref tab cnt)
                  (ekm:mv (> (or (assq-ref fret-diagram-details 'top-fret-thickness) 3) 1)))))
         (finger (or (assq-ref fret-diagram-details 'finger-code) #t))
         (finger (if (eq? 'none finger) #f
                  (or (assq-ref fret-diagram-details 'finger-style) 'sans)))
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
                  (let ((mark (assoc-ref tab (car pos))))
                    (if mark
                      (list mark x ymark #f)
                      (let ((y (+ (* h (- 4 (max 1 (min 4
                                 (or (string->number (car pos)) 1))))) 0.44)))
                        (if (> (length pos) 2)
                          (cond
                            ((string=? "(" (third pos)) (set! bow-beg (cons x y)))
                            ((string=? ")" (third pos)) (set! bow-end (cons x y)))))
                        (list (assoc-ref tab ".") x y (if num (max 1 (min 9 num)) #f)))))
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
            (if (and finger (fourth d))
              (ly:stencil-translate
                (centered-stencil
                  (interpret-markup layout props
                    (make-whiteout-markup
                      (make-fontsize-markup -7
                        (make-ekm-number-markup finger (fourth d))))))
                (cons (second d) ynum))
              point-stencil))
          sil))
      (if (and bow-beg bow-end)
        (ly:stencil-add
          (make-bow-stencil bow-beg bow-end thick 0.5 0.3 1)
          board)
        board)
      dl)))


%% Accordion register

#(define-markup-command (ekm-accordion layout props name)
  (string?)
  #:properties ((font-size 0))
  (let* ((i (string-index name #\space))
         (style (if (and i (< 0 i)) (string->symbol (string-take name i)) 'd))
         (key (if (and i (< 0 i)) (string-drop name (1+ i)) name))
         (d (ekm:asst 'accordion style key #f)))
    (if (ekm-cp? d)
      (ekm-center CX (ekm:char layout props d))
      (let* ((reg (ekm:text layout props (car d)))
             (dot (ekm-center CXY (ekm:text layout props
                    (ekm:asstl 'accordion 'dot))))
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
    'tweaks `((details . ,(ekm:assid 'script (format #f "ricochet~d" num))))))

ekmStemRicochet =
#(define-music-function (num music)
  (integer? ly:music?)
  #{
    \override Stem.stencil = #(ekm-stem (format #f "ricochet~d" num))
    $music
    \revert Stem.stencil
  #})


%% Fall / Doit

#(define (ekm-assb grob style dir)
  (let* ((log (ly:grob-property (ly:grob-parent grob X) 'duration-log 2))
         (sym (ekm:asst 'brass style (max log 0) dir)))
    (if (pair? sym) sym (cons sym #f))))

ekmBendAfter =
#(define-event-function (style dir)
  (symbol? ly:dir?)
  (make-music 'BendAfterEvent
    'delta-step dir
    'tweaks
    `((springs-and-rods .
        ,ly:spanner::set-spacing-rods)
      (minimum-length .
        ,(lambda (grob)
          (let* ((sym (ekm-assb grob style dir))
                 (w (interval-length (ly:grob-property (ly:grob-parent grob X) 'X-extent)))
                 (mk (make-ekm-text-markup (car sym))))
            (+ 0.6 w (ekm-dim grob mk X)))))
      (stencil .
        ,(lambda (grob)
          (let* ((sym (ekm-assb grob style dir))
                 (w (interval-length (ly:grob-property (ly:grob-parent grob X) 'X-extent)))
                 (mk (make-ekm-text-markup (car sym))))
            (grob-interpret-markup grob
              (make-translate-markup
                (cons (+ w 0.2) 0)
                (if (cdr sym) (make-general-align-markup Y UP mk) mk)))))))))


%% Figured bass

#(define (ekm-fbass fig ev ctx)
  (let* ((alt (ly:event-property ev 'alteration #f))
         (alt (and alt (ekm:assid 'fbass-acc alt)))
         (aug (and (ly:event-property ev 'augmented #f) "\\+"))
         (dim (and (ly:event-property ev 'diminished #f) "/"))
         (augs (and (ly:event-property ev 'augmented-slash #f) "\\\\"))
         (adir (ly:context-property ctx 'figuredBassAlterationDirection LEFT))
         (pdir (ly:context-property ctx 'figuredBassPlusDirection LEFT))
         (pre (if (number? fig)
                (ekm:assid 'fbass
                  (string-append (number->string fig) (or aug dim augs "")))
                #f))
         (pfx (list
                (if (and aug (= LEFT pdir) (not pre)) (ekm:assid 'fbass aug) 0)
                (if (and alt (= LEFT adir)) alt 0)))
         (sfx (list
                (if (and alt (= RIGHT adir)) alt 0)
                (if (and aug (= RIGHT pdir) (not pre)) (ekm:assid 'fbass aug) 0)))
         (sil (if pre
                (make-ekm-ctext-markup CX pre)
              (if (number? fig)
                (let ((num (make-center-align-markup
                        (make-ekm-number-markup 'fbass fig))))
                  (if (or dim augs)
                    (make-combine-markup num
                      (make-ekm-ctext-markup CX
                        (ekm:assid 'fbass (or dim augs))))
                  num))
              #f))))
    (make-translate-scaled-markup '(0.5 . 0)
      (if sil
        (make-put-adjacent-markup X RIGHT
        (make-put-adjacent-markup X LEFT
          sil (make-ekm-concat-markup pfx))
              (make-ekm-concat-markup sfx))
        (make-center-align-markup
          (make-ekm-concat-markup (append pfx sfx)))))))


%% Lyrics

#(define ekm-lyric? (string->char-set "~_%"))

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
              (make-ekm-text-markup (ekm:assid 'lyric (cdar l)))))
            (cdr l)))))))


%% Chord name

#(define (ekm:chord-set! text)
  (if (list? text)
    (cond
      ((and (eq? fontsize-markup (car text))
            (equal? "\xB0" (third text)))
        (set-car! text super-markup)
        (set-cdr! text `((,ekm-char-markup #xE870))))
      ((and (eq? super-markup (car text))
            (equal? "\xF8" (second text)))
        (set-car! text super-markup)
        (set-cdr! text `((,ekm-char-markup #xE871))))
      ((and (eq? fontsize-markup (car text))
            (pair? (third text))
            (eq? triangle-markup (car (third text))))
        (set-cdr! text `(-2 (,ekm-char-markup #xE873))))
      ((equal? "+" (car text))
        (set-car! text `(,ekm-char-markup #xE872)))
      ((equal? "-" (car text))
        (set-car! text `(,ekm-char-markup #xE874)))
      (else
        (for-each ekm:chord-set! text)))))


%% Analytics

#(define-markup-command (ekm-analytics layout props def)
  (string?)
  (interpret-markup layout props
    (make-ekm-def-markup (ekm:assid 'analytics #f) def)))


%% Function theory

#(define ekm-func-sep (string->char-set ",^"))
#(define ekm-func-paren (string->char-set "()[]{}"))

#(define (ekm-func layout props size def)
  (interpret-markup layout props
    (make-fontsize-markup size
    (make-ekm-def-markup (ekm:assid 'func #f) def))))

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
               #:ekm-def (ekm:assid 'func #f) (if i (substring mdef 0 i) mdef))))
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


%% Arrow / Beater

#(define-markup-command (ekm-arrow layout props style orient)
  (symbol? number?)
  (interpret-markup layout props
    (make-ekm-orient-markup 'arrow style orient)))

#(define-markup-command (ekm-arrow-head layout props axis dir filled)
  (integer? ly:dir? boolean?)
  (interpret-markup layout props
    (make-ekm-orient-markup 'arrow (if filled 'black-head 'open-head) (+ axis dir))))

#(define-markup-command (ekm-beater layout props style orient)
  (symbol? number?)
  (interpret-markup layout props
    (make-ekm-orient-markup 'beater style orient)))


%% Electronic music symbol

#(define (ekm-control style level fmt)
  (let* ((lv (inexact->exact (min 100
              (if (negative? level) (* (exp (* level 0.05 (log 10))) 100) level))))
         (sub (ekm:asst 'level style -1 #f))
         (pre (if (pair? sub)
                (ekm:asst 'level style lv #f)
                (ekm:asslim 'level style (* (round (/ lv sub)) sub) #f))))
    (list
      (make-ekm-text-markup (or pre (car sub)))
      (if pre #f (make-ekm-text-markup (cdr sub)))
      lv
      (format #f (or fmt (if (negative? level) "~adB" "~a%")) level))))

#(define-markup-command (ekm-fader layout props level orient)
  (number? boolean-or-number?)
  #:properties ((label-format))
  (let ((cl (ekm-control 'fader level label-format)))
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
  (let ((cl (ekm-control 'midi level label-format)))
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


%% Other symbol

#(define-markup-command (ekm-fermata layout props style)
  (symbol?)
  #:properties ((direction UP))
  (let ((val (or (ekm:assid 'script (string-append "d" (symbol->string style) "fermata"))
                 (ekm:assid 'script "dfermata"))))
    (ekm:text layout props (ekm:sym val direction))))

#(define-markup-command (ekm-note-by-number layout props style log dots dir)
  (symbol? integer? integer? ly:dir?)
  (let* ((note (interpret-markup layout props
                 (ekm-note style log dir)))
         (val (ekm:asstl 'dot style))
         (dot (ekm:text layout props (car val))))
    (ly:stencil-stack note X RIGHT
      (ekm-cat-dots dots dot)
      (* (ekm-extent dot X)
         (if (and (<= 3 log) (< 0 dir))
           (list-ref val (- (min log 5) 2)) 1)))))

#(define-markup-command (ekm-misc layout props key dir)
  (symbol? ly:dir?)
  (ekm:text layout props (ekm:sym (or (ekm:assid 'misc key) 0) dir)))

#(define-markup-command (ekm-eyeglasses layout props dir)
  (ly:dir?)
  (ekm:text layout props (ekm:sym (ekm:assid 'misc 'eyeglasses) dir)))

#(define-markup-command (ekm-metronome layout props count)
  (integer?)
  #:properties ((word-space))
  (ly:stencil-aligned-to
    (stack-stencil-line word-space
      (make-list count
        (ekm:text layout (cons '((font-size . -3)) props)
          (ekm:assid 'misc 'metronome))))
    X CENTER))

ekmMetronome =
#(define-music-function (music)
  (ly:music?)
  (for-some-music
    (lambda (m)
      (let* ((mom (ly:music-length m))
             (count (ceiling
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
                  'text (markup #:ekm-metronome count))
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
                 ^ \markup \ekm-metronome #count
              #})
            #t)
        #f))))
    music)
  music)

#(define-markup-command (ekm-default-scale-number layout props txt)
  (string?)
  (interpret-markup layout props
    (make-general-align-markup Y DOWN
    (make-fontsize-markup 4.5
    (make-sans-markup
    (make-override-markup '(baseline-skip . 1.2)
    (make-center-column-markup (list "⌃" txt))))))))


%% Types table

#(define ekm:types `(
  (notehead
  (default
    (-1 . #xE0A0)
    (0 . #xE0A2)
    (1 . #xE0A3)
    (2 . #xE0A4))
  (harmonic
    (2 . #xE0D9))
  (harmonic-black
    (-1 . #xE0DC)
    (0 . #xE0DC)
    (1 . #xE0DB))
  (harmonic-white
    (-1 . #xE0DE)
    (0 . #xE0DE)
    (1 . #xE0DD))
  (harmonic-mixed
    (-1 . #xE0D7)
    (0 . #xE0D8)
    (1 . #xE0D9)
    (2 . #xE0DB))
  (harmonic-wide
    (-1 . #xE0D7)
    (0 . #xE0D8)
    (1 . #xE0DA)
    (2 . #xE0DC))
  (diamond
    (-1 . #xE0DF)
    (0 . #xE0E0)
    (1 . #xE0E1)
    (2 . #xE0E2))
  (cross
    (-1 . #xE0A6)
    (0 . #xE0A7)
    (1 . #xE0A8)
    (2 . #xE0A9))
  (plus
    (-1 . #xE0AC)
    (0 . #xE0AD)
    (1 . #xE0AE)
    (2 . #xE0AF))
  (xcircle
    (-1 . #xE0B0)
    (0 . #xE0B1)
    (1 . #xE0B2)
    (2 . #xE0B3))
  (withx
    (-1 . #xE0B4)
    (0 . #xE0B5)
    (1 . #xE0B6)
    (2 . #xE0B7))
  (slashed
    (-1 . #xE0D5)
    (0 . #xE0D3)
    (1 . #xE0D1)
    (2 . #xE0CF))
  (backslashed
    (-1 . #xE0D6)
    (0 . #xE0D4)
    (1 . #xE0D2)
    (2 . #xE0D0))
  (slash
    (-1 . #xE10A)
    (0 . #xE102)
    (1 . #xE103)
    (2 . #xE101))
  (slash-muted
    (-1 . #xE109)
    (0 . #xE109)
    (1 . #xE109)
    (2 . #xE108))
  (circled
    (-1 . #xE0E7)
    (0 . #xE0E6)
    (1 . #xE0E5)
    (2 . #xE0E4))
  (circled-large
    (-1 . #xE0EB)
    (0 . #xE0EA)
    (1 . #xE0E9)
    (2 . #xE0E8))
  (triangle
    (-1 #xE0C3 . #xE0BA)
    (0  #xE0C4 . #xE0BB)
    (1  #xE0C5 . #xE0BC)
    (2  #xE0C7 . #xE0BE))
  (triangle-up
    (-1 . #xE0BA)
    (0 . #xE0BB)
    (1 . #xE0BC)
    (2 . #xE0BE))
  (triangle-down
    (-1 . #xE0C3)
    (0 . #xE0C4)
    (1 . #xE0C5)
    (2 . #xE0C7))
  (arrow
    (-1 #xE0F1 . #xE0ED)
    (0  #xE0F2 . #xE0EE)
    (1  #xE0F3 . #xE0EF)
    (2  #xE0F4 . #xE0F0))
  (arrow-up
    (-1 . #xE0ED)
    (0 . #xE0EE)
    (1 . #xE0EF)
    (2 . #xE0F0))
  (arrow-down
    (-1 . #xE0F1)
    (0 . #xE0F2)
    (1 . #xE0F3)
    (2 . #xE0F4))
  (round
    (0 . #xE114)
    (1 . #xE114)
    (2 . #xE113))
  (round-large
    (0 . #xE111)
    (1 . #xE111)
    (2 . #xE110))
  (round-dot
    (0 . #xE115)
    (1 . #xE115)
    (2 . #xE113))
  (round-dot-large
    (0 . #xE112)
    (1 . #xE112)
    (2 . #xE110))
  (round-slashed
    (0 . #xE119)
    (1 . #xE119)
    (2 . #xE118))
  (round-slashed-large
    (0 . #xE117)
    (1 . #xE117)
    (2 . #xE116))
  (square
    (0 . #xE0B8)
    (1 . #xE0B8)
    (2 . #xE0B9))
  (square-large
    (0 . #xE11B)
    (1 . #xE11B)
    (2 . #xE11A))
  (baroque
    (-1 . #xE0A1)
    (0 . #xE0A2)
    (1 . #xE0A3)
    (2 . #xE0A4))
  ;; shape noteheads
  (sol
    (-1 . #xECD0)
    (0 . #xE1B0)
    (1 . #xE1B0)
    (2 . #xE1B1))
  (solFunk
    (-1 . #xECD0)
    (0 . #xE1B0)
    (1 . #xE1B0)
    (2 . #xE1B1))
  (la
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laWalker
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laThin
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (laFunk
    (-1 . #xECD1)
    (0 . #xE1B2)
    (1 . #xE1B2)
    (2 . #xE1B3))
  (fa
    (-1 #xECD2 . #xECD3)
    (0  #xE1B4 . #xE1B6)
    (1  #xE1B4 . #xE1B6)
    (2  #xE1B5 . #xE1B7))
  (faThin
    (-1 #xECD2 . #xECD3)
    (0  #xE1B4 . #xE1B6)
    (1  #xE1B4 . #xE1B6)
    (2  #xE1B5 . #xE1B7))
  (faFunk
    (-1 #xECD2 . #xECD3)
    (0  #xE1B4 . #xE1B6)
    (1  #xE1B4 . #xE1B6)
    (2  #xE1B5 . #xE1B7))
  (faWalker
    (-1 #xECD2 . #xECD3)
    (0  #xE1B4 . #xE1B6)
    (1  #xE1B4 . #xE1B6)
    (2  #xE1B5 . #xE1B7))
  (mi
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miThin
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miFunk
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miMirror
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (miWalker
    (-1 . #xECD4)
    (0 . #xE1B8)
    (1 . #xE1B8)
    (2 . #xE1B9))
  (do
    (-1 . #xECD5)
    (0 . #xE1BA)
    (1 . #xE1BA)
    (2 . #xE1BB))
  (re
    (-1 . #xECD6)
    (0 . #xE1BC)
    (1 . #xE1BC)
    (2 . #xE1BD))
  (ti
    (-1 . #xECD7)
    (0 . #xE1BE)
    (1 . #xE1BE)
    (2 . #xE1BF))
  (doWalker
    (-1 . #xECD8)
    (0 . #xE1C0)
    (1 . #xE1C0)
    (2 . #xE1C1))
  (reWalker
    (-1 . #xECD9)
    (0 . #xE1C2)
    (1 . #xE1C2)
    (2 . #xE1C3))
  (tiWalker
    (-1 . #xECDA)
    (0 . #xE1C4)
    (1 . #xE1C4)
    (2 . #xE1C5))
  (doFunk
    (-1 . #xECDB)
    (0 . #xE1C6)
    (1 . #xE1C6)
    (2 . #xE1C7))
  (reFunk
    (-1 . #xECDC)
    (0 . #xE1C8)
    (1 . #xE1C8)
    (2 . #xE1C9))
  (tiFunk
    (-1 . #xECDD)
    (0 . #xE1CA)
    (1 . #xE1CA)
    (2 . #xE1CB))
  ;; note name noteheads
  (doName
    (0 (#xE150 . #xE1AD))
    (1 (#xE158 . #xE1AE))
    (2 (#xE160 . #xE1AF)))
  (reName
    (0 (#xE151 . #xE1AD))
    (1 (#xE159 . #xE1AE))
    (2 (#xE161 . #xE1AF)))
  (miName
    (0 (#xE152 . #xE1AD))
    (1 (#xE15A . #xE1AE))
    (2 (#xE162 . #xE1AF)))
  (faName
    (0 (#xE153 . #xE1AD))
    (1 (#xE15B . #xE1AE))
    (2 (#xE163 . #xE1AF)))
  (soName
    (0 (#xE154 . #xE1AD))
    (1 (#xE15C . #xE1AE))
    (2 (#xE164 . #xE1AF)))
  (laName
    (0 (#xE155 . #xE1AD))
    (1 (#xE15D . #xE1AE))
    (2 (#xE165 . #xE1AF)))
  (siName
    (0 (#xE157 . #xE1AD))
    (1 (#xE15F . #xE1AE))
    (2 (#xE167 . #xE1AF)))
  (tiName
    (0 (#xE156 . #xE1AD))
    (1 (#xE15E . #xE1AE))
    (2 (#xE166 . #xE1AF)))
  ;; individual notes
  (note
    (-1 . #xE1D0)
    (0 . #xE1D2)
    (1  #xE1D4 . #xE1D3)
    (2  #xE1D6 . #xE1D5)
    (3  #xE1D8 . #xE1D7)
    (4  #xE1DA . #xE1D9)
    (5  #xE1DC . #xE1DB)
    (6  #xE1DE . #xE1DD)
    (7  #xE1E0 . #xE1DF)
    (8  #xE1E2 . #xE1E1)
    (9  #xE1E4 . #xE1E3)
    (10 #xE1E6 . #xE1E5))
  (metronome
    (-1 . #xECA0)
    (0 . #xECA2)
    (1  #xECA4 . #xECA3)
    (2  #xECA6 . #xECA5)
    (3  #xECA8 . #xECA7)
    (4  #xECAA . #xECA9)
    (5  #xECAC . #xECAB)
    (6  #xECAE . #xECAD)
    (7  #xECB0 . #xECAF)
    (8  #xECB2 . #xECB1)
    (9  #xECB4 . #xECB3)
    (10 #xECB6 . #xECB5))
  )

  (flag
  (default
    (3  #xE241 . #xE240)
    (4  #xE243 . #xE242)
    (5  #xE245 . #xE244)
    (6  #xE247 . #xE246)
    (7  #xE249 . #xE248)
    (8  #xE24B . #xE24A)
    (9  #xE24D . #xE24C)
    (10 #xE24F . #xE24E))
  )

  (rest
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
  )

  (dot
  (default   #xE1E7 0 0 0)
  (note      #xE1E7 -0.2 0.7 0.7)
  (metronome #xECB7 0.2 0.7 0.7)
  (straight  #xE1E7 -0.8 -0.8 0.3)
  (short     #xE1E7 -0.8 -0.8 0.5)
  (beamed    #xE1E7 -1.4 -1.4 -1.4)
  (text      #xE1FC 0 0 0)
  )

  (script (#t
  ("sforzato" #xE4A1 . #xE4A0) ; accent
  ("espr" #xED41 . #xED40)
  ("dmarcato" #xE4AD . #xE4AC)
  ("uportato" #xE4B3 . #xE4B2)
  ("dstaccatissimo" #xE4A7 . #xE4A6)
  ("staccato" #xE4A3 . #xE4A2)
  ("tenuto" #xE4A5 . #xE4A4)
  ("trill" . #xE566)
  ("prall" . #xE56C)
  ("mordent" . #xE56D)
  ("prallmordent" . #xE5BD)
  ("upprall" (#xE59A #xE59D #xE59D #xE59E))
  ("downprall" . #xE5C6)
  ("upmordent" . #xE5B8)
  ("downmordent" . #xE5C7)
  ("lineprall" . #xE5B2)
  ("prallprall" . #xE56E)
  ("pralldown" . #xE5C8)
  ("prallup" (#xE59D #xE59D #xE59D #xE5A4))
  ("turn" . #xE567)
  ("reverseturn" . #xE568)
  ("slashturn" . #xE569)
  ("haydnturn" . #xE56F)
  ("upbow" . #xE612)
  ("downbow" . #xE610)
  ("flageolet" . #xE614)
  ("open" . #xE614)
  ("halfopen" . #xE615)
  ("snappizzicato" #xE630 . #xE631)
  ("stopped" . #xE633)
  ("upedalheel" . #xE661)
  ("dpedalheel" . #xE662)
  ("upedaltoe" . #xE664)
  ("dpedaltoe" . #xE665)
  ("dfermata" #xE4C1 . #xE4C0)
  ("dshortfermata" #xE4C5 . #xE4C4)
  ("dlongfermata" #xE4C7 . #xE4C6)
  ("dveryshortfermata" #xE4C3 . #xE4C2)
  ("dverylongfermata" #xE4C9 . #xE4C8)
  ("dhenzeshortfermata" #xE4CD . #xE4CC)
  ("dhenzelongfermata" #xE4CB . #xE4CA)
  ("lcomma" #xE4CE)
  ("segno" . #xE047)
  ("coda" . #xE048)
  ("varcoda" . #xE049)
  ("ricochet2" . #xE8CD)
  ("ricochet3" . #xE8CE)
  ("ricochet4" . #xE8CF)
  ("ricochet5" . #xE8D0)
  ("ricochet6" . #xE8D1)
  ))

  (clef (#t
  ("clefs.G" #xE050 . #xE07A)
  ("clefs.GG" #xE055 . #f)
  ("clefs.tenorG" #xE056 . #f)
  ("clefs.C" #xE05C . #xE07B)
  ("clefs.varC" #xE05C . #xE07B)
  ("clefs.F" #xE062 . #xE07C)
  ("clefs.percussion" #xE069 . #f)
  ("clefs.varpercussion" #xE06A . #f)
  ("semipitched" #xE06B . #f)
  ("varsemipitched" #xE06C . #f)
  ("indiandrum" #xED70 . #f)
  ("clefs.tab" #xE06D . #xE06E)
  ("4stringtab" #xE06E . #f)
  ("bridge" #xE078 . #f)
  ("accordion" #xE079 . #f)
  ("clefs.neomensural.c" #xE060 . #f)
  ))

  (clef-mod (#t
  ("8" . #xE07D)
  ("15" . #xE07E)
  (parenthesized #xED8A . #xED8B)
  (bracketed #xED8C . #xED8D)
  ))

  (time (#t
  ("+" . #xE08C)
  ("/+" . #xE08D)
  ("//" . #xE08E)
  ("C" . #xE08A)
  ("|C" . #xE08B)
  ("|2" . #xEC85)
  ("|3" . #xEC86)
  ("X" . #xE09C)
  ("~" . #xE09D)
  (time-x . #xE09C)
  (time-penderecki . #xE09D)
  ))

  (time-sub (#t
  (1/4 . #xE097)
  (1/2 . #xE098)
  (3/4 . #xE099)
  (1/3 . #xE09A)
  (2/3 . #xE09B)
  ))

  (colon (#t
    (#\: #xE043 #t #f)
  ))

  (segno (#t
    (#\S #xE04A #t 0.5)
    (#\= #xE04A #f 0.5)
  ))

  (barline (#t
    (-1 . #xE00B)
    (1  . #xE00C)
    (0  . #xE00D)
  ))

  (separator
  (default
    (0 . #xE007)
    (1 . #xE008)
    (+inf.0 . #xE009))
  )

  (shared (#t #t
  (" " . ,(markup #:hspace 1))
  ("____" . ,(markup #:hspace 4))
  ("___" . ,(markup #:hspace 2))
  ("__" . ,(markup #:hspace 0.78))
  ("_" . ,(markup #:hspace 0.17))
  ("`" . #f)
  ))

  (dynamic (#t
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
  ("pppppp" . #xE527) ; not used
  ("ffffff" . #xE533) ; not used
  ("fz" . #xE535)
  ("sf" . #xE536)
  ("sfp" . #xE537)
  ("sfpp" . #xE538)
  ("sfz" . #xE539)
  ("sfzp" . #xE53A)
  ("sffz" . #xE53B)
  ("rf" . #xE53C)
  ("rfz" . #xE53D)
  ))

  (systemstart
  (brace
    (+inf.0 #xE000 . #xE001))
  (bracket
    (+inf.0 (#f ,(* -4 255/1000) #f #xE004 #xE003 0 #f)))
  )

  (mmrest
  (default #xE4EF . #xE4F1)
  )

  (spanner
  (line
  )
  (trill
    (text . #xE566)
    (0 . #xEAA4)
    (1 #xEAA5 . #xEAA3)
    (2 #xEAA6 . #xEAA2)
    (3 #xEAA7 . #xEAA1)
    (4 #xEAA8 . #xEAA0))
  (vibrato
    (text . #xEACC)
    (0 . #xEADE)
    (1 #xEADF . #xEADD)
    (2 #xEAE0 . #xEADC)
    (3 #xEAE1 . #xEADB))
  (vibrato-small
    (text . #xEACC)
    (0 . #xEAD7)
    (1 #xEAD8 . #xEAD6)
    (2 #xEAD9 . #xEAD5)
    (3 #xEADA . #xEAD4))
  (vibrato-large
    (text . #xEACC)
    (0 . #xEAE5)
    (1 #xEAE6 . #xEAE4)
    (2 #xEAE7 . #xEAE3)
    (3 #xEAE8 . #xEAE2))
  (vibrato-smallest
    (text . #xEACC)
    (0 . #xEAD0)
    (1 #xEAD1 . #xEACF)
    (2 #xEAD2 . #xEACE)
    (3 #xEAD3 . #xEACD))
  (vibrato-largest
    (text . #xEACC)
    (0 . #xEAEC)
    (1 #xEAED . #xEAEB)
    (2 #xEAEE . #xEAEA)
    (3 #xEAEF . #xEAE9))
  (circular
    (text #xEAC4 . #xEACB)
    (0 . #xEAC9)
    (1 #xEAC8 . #xEACA)
    (2 #xEAC7 . #xEACA)
    (3 #xEAC6 . #xEACA)
    (4 #xEAC5 . #xEACA))
  (circular-constant
    (0 . #xEAC0)
    (1 #xEAC1 . #xEAC0)
    (2 #xEAC3 . #xEAC2))
  (wavy
    (0 . #xEAB5)
    (1 #xEAB6 . #xEAB4))
  (square
    (0 . #xEAB8)
    (1 #xEAB9 . #xEAB7))
  (sawtooth
    (0 . #xEABB)
    (1 #xEABC . #xEABA))
  (beam
    (text 0 . #xEB03)
    (0 . #xEAFB)
    (1 #xEAFA . #xEAFC)
    (2 #xEAF9 . #xEAFD)
    (3 #xEAF8 . #xEAFE)
    (4 #xEAF7 . #xEAFF)
    (5 #xEAF6 . #xEB00)
    (6 #xEAF5 . #xEB01)
    (7 #xEAF4 . #xEB02))
  )

  (fingering
  (default
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
    ("S" . ,(markup #:ekm-scoop UP))
    ("P" . ,(markup #:ekm-scoop DOWN)))
  (italic
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
    ("." . #xED2C)
    ("," . #xED2D)
    ("/" . #xED2E)
    ("~~" . #xED20)
    ("~" . #xED21)
    ("-" . #xED22))
  )

  (pedal (#t #t
  ("Ped." . #xE650)
  ("P" . #xE651)
  ("e" . #xE652)
  ("d" . #xE653)
  ("Sost." . #xE659)
  ("S" . #xE65A)
  ("." . #xE654)
  ("-" . #xE658)
  ("*" . #xE655)
  ("o" . #xE65D)
  ("," . #xE65B)
  ("'" . #xE65C)
  ("H" . #xE656)
  ("^" . #xE657)
  ("l" . #xE65E)
  ("m" . #xE65F)
  ("r" . #xE660)
  ("(" . #xE676)
  (")" . #xE677)
  ))

  (ottava (#t #t
  ("8va" . #xE512)
  ("8vb" . #xE51C)
  ("8ba" . #xE513)
  ("8^va" . #xE511)
  ("8" . #xE510)
  ("15ma" . #xE516)
  ("15mb" . #xE51D)
  ("15^ma" . #xE515)
  ("15" . #xE514)
  ("22ma" . #xE519)
  ("22mb" . #xE51E)
  ("22^ma" . #xE518)
  ("22" . #xE517)
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

  (ottavation
  (numbers
    (1 . "8")
    (2 . "15")
    (3 . "22"))
  (ordinals
    (1 "8va" . "8^va")
    (2 "15ma" . "15^ma")
    (3 "22ma" . "22^ma"))
  (simple-ordinals
    (1 "8vb" . "8va")
    (2 "15mb" . "15ma")
    (3 "22mb" . "22ma"))
  (ordinals-b
    (1 "8vb" . "8^va")
    (2 "15mb" . "15^ma")
    (3 "22mb" . "22^ma"))
  (ordinals-bassa
    (1 "8va__bassa" . "8^va")
    (2 "15ma__bassa" . "15^ma")
    (3 "22ma__bassa" . "22^ma"))
  (ordinals-ba
    (1 "8ba" . "8^va")
    (2 "15ba" . "15^ma")
    (3 "22ba" . "22^ma"))
  (numbers-ba
    (1 "8ba" . "8")
    (2 "15ba" . "15")
    (3 "22ba" . "22"))
  )

  (tuplet (#t
    (":" . #xE88A)
  ))

  (number
  (time .
    #xE080)
  (time-turned .
    #xECE0)
  (time-reversed .
    #xECF0)
  (tuplet .
    #xE880)
  (fingering .
    #(#xED10 #xED11 #xED12 #xED13 #xED14 #xED15 #xED24 #xED25 #xED26 #xED27))
  (fingering-italic .
    #xED80)
  (fbass .
    #(#xEA50 #xEA51 #xEA52 #xEA54 #xEA55 #xEA57 #xEA5B #xEA5D #xEA60 #xEA61))
  (func .
    #xEA70)
  (sans .
    ,make-sans-markup)
  (serif .
    ,(if (ly:version? < '(2 25)) make-roman-markup make-serif-markup))
  (typewriter .
    ,make-typewriter-markup)
  (string
    (0  . #xE833)
    (1  . #xE834)
    (2  . #xE835)
    (3  . #xE836)
    (4  . #xE837)
    (5  . #xE838)
    (6  . #xE839)
    (7  . #xE83A)
    (8  . #xE83B)
    (9  . #xE83C)
    (10 . #xE84A)
    (11 . #xE84B)
    (12 . #xE84C)
    (13 . #xE84D))
  (scale
    (1 . #xEF00)
    (2 . #xEF01)
    (3 . #xEF02)
    (4 . #xEF03)
    (5 . #xEF04)
    (6 . #xEF05)
    (7 . #xEF06)
    (8 . #xEF07)
    (9 . #xEF08))
  (ekm
    (string . ,make-ekm-default-string-number-markup)
    (scale . ,make-ekm-default-scale-number-markup))
  )

  (tremolo
  (beam-like
    (1 . #xE220)
    (2 . #xE221)
    (3 . #xE222)
    (4 . #xE223)
    (5 . #xE224))
  (fingered
    (1 . #xE225)
    (2 . #xE226)
    (3 . #xE227)
    (4 . #xE228)
    (5 . #xE229))
  )

  (stem (#t
  ;; tremolo marks
  ("buzzroll" . #xE22A)
  ("penderecki" . #xE22B)
  ("unmeasured" . #xE22C)
  ("unmeasuredS" . #xE22D)
  ("stockhausen" . #xE232)
  ;; stem decoration
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
  ("ricochet2" . #xE8D2)
  ("ricochet3" . #xE8D3)
  ("ricochet4" . #xE8D4)
  ("ricochet5" . #xE8D5)
  ("ricochet6" . #xE8D6)
  ))

  (grace
  (default
    (3 (#xE565 -0.596 . 2.168) . (#xE564 -0.644 . -2.456)))
  )

  (lv
  (default
    (+inf.0 #xE4BB . #xE4BA))
  )

  (arpeggio
  (default
    (0 (0 #xEAAA 0) . (0 #xEAA9 0))
    (1 (#xEAAE #xEAAA 0) . (0 #xEAA9 #xEAAD)))
  (swash
    (0 (0 #xEAAA #xEAAC) . (#xEAAB #xEAA9 0))
    (1 (#xEAAE #xEAAA #xEAAC) . (#xEAAB #xEAA9 #xEAAD)))
  )

  (cluster
  (default
    (-1 (#xE0A0 #xE124 #xE128 #xE12C #xE12D #xE12E 0))
    (0 (#xE0A2 #xE125 #xE129 #xE12F #xE130 #xE131 0))
    (1 (#xE0A3 #xE126 #xE12A #xE132 #xE133 #xE134 0) .
       (#xE0A3 #xE126 #xE12A #xE132 #xE133 #xE134 0))
    (2 (#xE0A4 #xE127 #xE12B #xE135 #xE136 #xE137 0) .
       (#xE0A4 #xE127 #xE12B #xE135 #xE136 #xE137 0)))
  (harmonic
    (0 (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0) .
       (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0))
    (1 (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0.5) .
       (#xE0DD #xE138 #xE13A #xE13C #xE13D #xE13E 0.5))
    (2 (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4) .
       (#xE0DB #xE139 #xE13B #xE13F #xE140 #xE141 0.4)))
  (square
    (0 (#xE0B8 #f #f #xE145 #xE146 #xE147 0) .
       (#xE0B8 #f #f #xE145 #xE146 #xE147 0))
    (1 (#xE0B8 #f #f #xE145 #xE146 #xE147 -0.3) .
       (#xE0B8 #f #f #xE145 #xE146 #xE147 -0.3))
    (2 (#xE0B9 #f #f #xE142 #xE143 #xE144 -0.3) .
       (#xE0B9 #f #f #xE142 #xE143 #xE144 -0.3)))
  )

  (percent (#t
  ("/" . #xE504)
  ("//" . #xE501)
  ("%" . #xE500)
  ("%%" . #xE501)
  ;("%%%%" . #xE502) ; not used
  ))

  (harp (#t #t
  ("^"  . #xE680)
  ("-"  . #xE681)
  ("v"  . #xE682)
  ("|"  . #xE683)
  ("o^" . ,(markup #:ekm-harp-change #xE680 0.93))
  ("o-" . ,(markup #:ekm-harp-change #xE681 0))
  ("ov" . ,(markup #:ekm-harp-change #xE682 -0.93))
  (" "  . #f)
  ))

  (fret (#t
  ("." . #xE858)
  ("x" . #xE859)
  ("o" . #xE85A)
  (3 #xE850 . #xE851)
  (4 #xE852 . #xE853)
  (5 #xE854 . #xE855)
  (6 #xE856 . #xE857)
  ))

  (accordion
  (ekm
    (#f . 0))
  (dot . #xE8CA)
  (d
    ("1" . #xE8A4)
    ("10" . #xE8A1)
    ("11" . #xE8AB)
    ("1+0" . #xE8A2)
    ("1+1" #xE8C6 (76 . 50) (50 . 18))
    ("1-0" . #xE8A3)
    ("1-1" #xE8C6 (24 . 50) (50 . 18))
    ("20" . #xE8AE)
    ("21" . #xE8AF)
    ("2+0" . #xE8A6)
    ("2+1" . #xE8AC)
    ("2-0" #xE8C6 (24 . 50) (50 . 50))
    ("2-1" #xE8C6 (24 . 50) (50 . 50) (50 . 18))
    ("30" . #xE8A8)
    ("31" . #xE8B1)
    ("100" . #xE8A0)
    ("101" . #xE8A9)
    ("110" . #xE8A5)
    ("111" . #xE8AA)
    ("11+0" #xE8C6 (50 . 82) (76 . 50))
    ("11+1" #xE8C6 (50 . 82) (76 . 50) (50 . 18))
    ("11-0" #xE8C6 (50 . 82) (24 . 50))
    ("11-1" #xE8C6 (50 . 82) (24 . 50) (50 . 18))
    ("120" . #xE8B0)
    ("121" . #xE8AD)
    ("12+0" . #xE8A7)
    ("12+1" #xE8C6 (50 . 82) (50 . 50) (76 . 50) (50 . 18))
    ("12-0" #xE8C6 (50 . 82) (24 . 50) (50 . 50))
    ("12-1" #xE8C6 (50 . 82) (24 . 50) (50 . 50) (50 . 18))
    ("130" . #xE8B2)
    ("131" . #xE8B3))
  (sb
    ("Soprano" . #xE8B4)
    ("Alto" . #xE8B5)
    ("Tenor" . #xE8B6)
    ("Master" . #xE8B7)
    ("Soft Bass" . #xE8B8)
    ("Soft Tenor" . #xE8B9)
    ("Bass/Alto" . #xE8BA))
  (sb4
    ("Soprano" . #xE8B4)
    ("Alto" . #xE8B5)
    ("Tenor" #xE8C7 (50 . 87) (50 . 38))
    ("Master" #xE8C7 (50 . 87) (50 . 62) (50 . 38) (50 . 14))
    ("Soft Bass" #xE8C7 (50 . 62) (50 . 38) (50 . 14))
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass/Alto" #xE8C7 (50 . 62) (50 . 14))
    ("Soft Tenor" . #xE8B9))
  (sb5
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass/Alto" #xE8C7 (50 . 62) (50 . 14))
    ("Alto" #xE8C7 (38 . 85) (62 . 85) (50 . 62))
    ("Tenor" #xE8C7 (38 . 85) (62 . 85) (50 . 38))
    ("Master" #xE8C7 (38 . 85) (62 . 85) (50 . 62) (50 . 38) (50 . 14))
    ("Soft Bass" #xE8C7 (50 . 62) (50 . 38) (50 . 14))
    ("Soft Tenor" . #xE8B9)
    ("Soprano" . #xE8B4)
    ("Sopranos" #xE8C7 (38 . 85) (62 . 85))
    ("Solo Bass" #xE8C7 (50 . 14)))
  (sb6
    ("Soprano" . #xE8B4)
    ("Alto" #xE8C7 (50 . 62))
    ("Soft Tenor" . #xE8B9)
    ("Master" . #xE8B7)
    ("Alto/Soprano" #xE8C7 (50 . 87) (26 . 62))
    ("Bass/Alto" . #xE8BA)
    ("Soft Bass" . #xE8B8))
  (fb
    ("10" . #xE8BB)
    ("1" . #xE8BC)
    ("11" . #xE8BD)
    ("Master" . #xE8BE)
    ("Master 1" . #xE8BF)
    ("Master 11" . #xE8C0))
  (sq
    ("1" . #xE8C1)
    ("100" . #xE8C2)
    ("2" . #xE8C3)
    ("101" . #xE8C4)
    ("102" . #xE8C5))
  )

  (brass
  (bend
    (0 #xE5D9 . #xE5D6)
    (1 #xE5D8 . #xE5D5)
    (2 #xE5D7 . #xE5D4))
  (rough
    (0 (#xE5DF . #t) . #xE5D3)
    (1 (#xE5DE . #t) . #xE5D2)
    (2 (#xE5DD . #t) . #xE5D1))
  (smooth
    (0 (#xE5DC . #t) . #xE5EE)
    (1 (#xE5DB . #t) . #xE5ED)
    (2 (#xE5DA . #t) . #xE5EC))
  (scoop
    (0 #xE5E0 . #xE5D0))
  )

  (parens
  (default
    (accidental #xE26A . #xE26B)
    (dynamic ("(" -0.5 -1) . (")" -0.5 -1))
    (hairpin #xE542 . #xE543))
  (bracket
    (accidental #xE26C . #xE26D)
    (dynamic ("[" -0.5 -1) . ("]" -0.5 -1))
    (hairpin #xE544 . #xE545))
  (brace
    (dynamic ("{" -0.5 -1) . ("}" -0.5 -1)))
  (angle
    (dynamic ("<" -0.5 -1) . (">" -0.5 -1)))
  )

  (fbass (#t
  ("2\\+"  . #xEA53)
  ("4\\+"  . #xEA56)
  ("5\\\\" . #xEA59)
  ("5\\+"  . #xEA58)
  ("5/"    . #xEA5A)
  ("6\\\\" . #xEA5C)
  ("6\\+"  . #xEA6F)
  ("7\\+"  . #xEA5E)
  ("7\\\\" . #xEA5F)
  ("7/"    . #xECC0)
  ("9\\\\" . #xEA62)
  ("\\\\" . #xEA6E)
  ("\\+" . #xEA6C)
  ("/" . #xEA6D)
  ))

  (fbass-acc (#t
  (0 . #xEA65)
  (-1/2 . #xEA64)
  (1/2 . #xEA66)
  (-1 . #xEA63)
  (1 . #xEA67)
  (-3/2 . #xECC1)
  (3/2 . #xECC2)
  ))

  (lyric (#t
  (#\~ . #xE551)
  (#\n . #xE550)
  (#\w . #xE552)
  (#\_ . #xE553)
  (#\x25 . #xE555)
  (#\x . 0)
  ))

  (analytics (#t #t
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

  (func (#t #t
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
  ("/D" . ,(markup #:ekm-combine #xEA7F 0.4 0 #xE87B))
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

  (arrow
  (black . #xEB60)
  (ekm
    (0 0 1 2 3 4 5 6 7)
    (1 0)
    (2 0 4)
    (4 0 2 4 6)
    (-1 . #(0 (0 . -45) (0 . -90) (1 . ,Y) (0 . ,Y) (7 . ,Y) (2 . ,X) (1 . ,X)
            0 (8 . -45) (8 . -90) (9 . ,Y))))
  (white . #xEB68)
  (open . #xEB70)
  (black-head . #xEB78)
  (white-head . #xEB80)
  (open-head . #xEB88)
  )

  (beater
  (xyl-soft . #xE770)
  (ekm
    (0 0 4 1 7)
    (1 0)
    (2 0 4)
    (4 0 4 1 7)
    (-1 . #(0 (0 . -30) (0 . -90) (1 . ,Y) (0 . ,Y) (7 . ,Y) (2 . ,X) (1 . ,X)
            0 (8 . -30) (8 . -90) (9 . ,Y))))
  (xyl-medium . #xE774)
  (xyl-hard . #xE778)
  (xyl-wood . #xE77C)
  (glsp-soft . #xE780)
  (glsp-hard . #xE784)
  (timpani-soft . #xE788)
  (timpani-medium . #xE78C)
  (timpani-hard . #xE790)
  (timpani-wood . #xE794)
  (yarn-soft . #xE7A2)
  (yarn-medium . #xE7A6)
  (yarn-hard . #xE7AA)
  (gum-soft . #xE7BB)
  (gum-medium . #xE7BF)
  (gum-hard . #xE7C3)
  (bass-soft . #(#xE798 #xE799))
  (bass-medium . #(#xE79A #xE79B))
  (bass-hard . #(#xE79C #xE79D))
  (bass-metal . #(#xE79E #xE79F))
  (bass-double . #(#xE7A0 #xE7A1))
  (stick . #(#xE7E8))
  (stick-snare . #(#xE7D1 #xE7D2))
  (stick-jazz . #(#xE7D3 #xE7D4))
  (hammer-wood . #(#xE7CB #xE7CC))
  (hammer-plastic . #(#xE7CD #xE7CE))
  (hammer-metal . #(#xE7CF #xE7D0))
  (superball . #xE7AE)
  (wound-hard . #xE7B3)
  (wound-soft . #xE7B7)
  (metal . #xE7C7)
  (brass-mallets . #(#xE7D9 #xE7DA #xE7ED #xE7EE))
  (triangle . #(#xE7D5 #xE7D6))
  (triangle-plain . #(#xE7EF))
  (wire-brushes . #(#xE7D7 #xE7D8))
  (mallet . #(#xE7DF #xE7EC))
  (metal-hammer . #(#xE7E0))
  (hammer . #(#xE7E1))
  (hand . #(#xE7E3))
  (finger . #(#xE7E4))
  (fist . #(#xE7E5))
  (fingernails . #(#xE7E6))
  )

  (level
  (fader
    (-1 #xEB2C . #xEB2D)
    (0 . #xEB2E)
    (20 . #xEB2F)
    (40 . #xEB30)
    (60 . #xEB31)
    (80 . #xEB32)
    (100 . #xEB33)
    (+inf.0 . #f))
  (midi
    (-1 . 20)
    (0 . #xEB36)
    (20 . #xEB37)
    (40 . #xEB38)
    (60 . #xEB39)
    (80 . #xEB3A)
    (100 . #xEB3B)
    (+inf.0 . #f))
  )

  (misc (#t
    (eyeglasses . #xEC62)
    (metronome . ,(markup #:filled-box '(0 . 0.3) '(-0.7 . 0.7) 0.15))
  ))
))

#(define (ekm:merge-type type tab)
  (let ((ttab (assq-ref ekm:types type)))
    (if ttab
      (for-each (lambda (s)
        (let ((stab (assq-ref ttab (car s))))
          (if stab
            (if (eq? #t (car s))
              (if (eq? #t (car stab))
                ;; merge tokens
                (for-each (lambda (e)
                  (let fnd ((t stab))
                    (if (null? (cdr t))
                      (set-cdr! t (cons* e '())) ; add
                    (if (string-prefix? (caadr t) (car e))
                      (if (= (string-length (caadr t)) (string-length (car e)))
                        (set-cdr! (cadr t) (cdr e)) ; replace
                        (set-cdr! t (cons* e (cdr t)))) ; insert before same prefix
                      (fnd (cdr t))))))
                  (cdr s))
                ;; merge ids
                (for-each (lambda (e)
                  (let ((ee (assoc (car e) stab)))
                    (if ee
                      (set-cdr! ee (cdr e)) ; replace
                      (append! stab (list e))))) ; add
                  (cdr s)))
              (assq-set! ttab (car s) (cdr s))) ; replace table
            (append! ttab (list s))))) ; add table
        tab))))

ekmMergeType =
#(define-void-function (type tab)
  (symbol? cheap-list?)
  (ekm:merge-type type tab))


%% SMuFL switches

ekmSmuflOn =
#(define-music-function (type)
  (symbol-list-or-symbol?)
  (let* ((typ (if (symbol? type) (list type) type))
         (neg (if (and (not (null? typ)) (eq? '- (car typ))) not identity))
         (typ (if (neg #f) (cdr typ) typ))
         (all (memq 'all typ))
         (music #{ #}))
    (define (on t m)
      (if (neg (or all (memq t typ)))
        (if (procedure? m)
          (m t)
          (set! music #{ #music #m #}))))

    (on 'clef #{
      \override Clef.stencil = #ekm-clef
      \set clefTranspositionFormatter = #ekm-clef-mod
      \set cueClefTranspositionFormatter = #ekm-clef-mod
    #})
    (on 'time #{
      \override Timing.TimeSignature.stencil = #ekm-timesig
    #})
    (on 'notehead #{
      \override NoteHead.stencil = #(ekm-notehead #f)
      \override NoteHead.stem-attachment = #ekm-stem-attachment
      \override AmbitusNoteHead.stencil = #(ekm-notehead 0)
    #})
    (on 'dot #{
      \override Dots.stencil = #ekm-dots
    #})
    (on 'flag #{
      \override Flag.stencil = #ekm-flag
      \override Flag.style = #'default
      \override Stem.stencil = #ekm-stem-print
    #})
    (on 'rest #{
      \override Rest.stencil = #ekm-rest
      \override MultiMeasureRest.stencil = #ekm-mmr
      \override MultiMeasureRestNumber.stencil = #ekm-mmr-number
    #})
    (on 'systemstart #{
      \override SystemStartBrace.stencil = #ekm-system-start
      \override SystemStartBracket.stencil = #ekm-system-start
      \override SystemStartBracket.thickness = #(ekm:md 'bracketThickness)
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
    (on 'textspan #{
      \override TextSpanner.after-line-breaking = #ekm-spanner
    #})
    (on 'trill #{
      \override TrillSpanner.after-line-breaking = #ekm-spanner
      \override TrillPitchHead.stencil = #ekm-trillpitch-head
      \override TrillPitchParentheses.stencils = #ekm-calc-parenthesis-stencils
    #})
    (on 'segno ekm-bar-init)
    (on 'colon ekm-bar-init)
    (on 'percent #{
      \override RepeatSlash.stencil = #ekm-repeat
      \override DoubleRepeatSlash.stencil = #ekm-doublerepeat
      \override PercentRepeat.stencil = #ekm-percent
      \override DoublePercentRepeat.stencil = #ekm-doublepercent
    #})
    (on 'tremolo #{
      \override StemTremolo.stencil = #(ekm-repeat-tremolo #f)
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
    (on 'chord #{
      \override ChordName.stencil = #(lambda (grob)
        (ekm:chord-set! (ly:grob-property grob 'text))
        (if (defined? 'ekm:chord-acc-set!)
          (ekm:chord-acc-set! (ly:grob-property grob 'text)))
        (ly:text-interface::print grob))
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
      \revert NoteHead.stem-attachment
      \revert AmbitusNoteHead.stencil
    #})
    (on 'dot #{
      \revert Dots.stencil
    #})
    (on 'flag #{
      \revert Flag.stencil
      \revert Flag.style
      \revert Stem.stencil
    #})
    (on 'rest #{
      \revert Rest.stencil
      \revert MultiMeasureRest.stencil
      \revert MultiMeasureRestNumber.stencil
    #})
    (on 'systemstart #{
      \revert SystemStartBrace.stencil
      \revert SystemStartBracket.stencil
      \revert SystemStartBracket.thickness
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
    (on 'textspan #{
      \revert TextSpanner.after-line-breaking
    #})
    (on 'trill #{
      \revert TrillSpanner.after-line-breaking
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
    (on 'chord #{
      \revert ChordName.stencil
    #})
    music))


%% Initializations

#(let* ((font (ly:get-option 'ekmfont))
        (font (if font (symbol->string font)
              (if (defined? 'ekmFont) ekmFont "")))
        (path (string-suffix? "#" font))
        (font (if path (string-drop-right font 1) font))
        (font (if (string-null? font) "Ekmelos" font))
        (f (string-downcase font))
        (dir (ly:get-option 'ekmmetadata))
        (dir (if dir (symbol->string dir)
             (if (defined? 'ekmMetadata) ekmMetadata "")))
        (cr (string-suffix? "%" dir))
        (dir (if cr (string-drop-right dir 1) dir))
        (name (string-append "ekmd-" f ".scm"))
        (tab (ekmd:load dir font name)))

  ;; create metadata table
  (if (or cr (not tab))
    (let* ((tpl (ekmd:set-dg! (ekmd:load dir font "ekmd-template.scm")))
           (types (or (ekmd:load dir font (string-append "types-" f ".scm"))
                      (ekmd:load dir font "types-template.scm")
                      '()))
           (md (ekmd:read
                dir font font
                '((fontName . #t)
                  (fontVersion . #t)
                  (engravingDefaults (#t . #\d))
                  (glyphsWithAnchors
                    ("flag" (stemDownSW . #\01) (stemUpNW . #\02))
                    ("note" (stemDownNW . #\02) (stemUpSE . #\03)))
                  (optionalGlyphs
                    (#t (codepoint . #\c))))
                '(("flag" (#f . #f) (#f . #f))))))
      (if (and tpl md)
        (begin
          (ekmd:name->cp (append ekmd:glyphnames (or (assq-ref md 'optionalGlyphs) '())))
          (set! tab (list 'quasiquote (list
            (cons 'fontName font)
            (cons 'fontVersion (assq-ref md 'fontVersion))
            (cons 'defaults ekmd:defaults)
            (cons 'glyphs ekmd:glyphs)
            (cons 'types types))))
          (ekmd:save (string-append ekmd:dir "/" name) tab)))))

  (set! ekm:font-name font)
  (set! ekm:draw-paths (and path (defined? 'ekm-path-stencil)))

  (set! tab (primitive-eval tab))
  (ekmd:set-dg! tab)
  (for-each (lambda (t)
    (ekm:merge-type (car t) (cdr t)))
    (or (assq-ref tab 'types) '())))

%% Symbols for frequently used types
#(define ekm-notehead-tab (assq-ref ekm:types 'notehead))
#(define ekm-flag-tab (assq-ref ekm:types 'flag))
#(define ekm-rest-tab (assq-ref ekm:types 'rest))
#(define ekm-shared-tab (ekm:assid 'shared #f))

#(ekm-init-stemlength)
#(ekm-init-clef)
